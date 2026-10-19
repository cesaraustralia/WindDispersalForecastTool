// Wind-dispersal cellular automaton: C++ core (Rcpp)
//
// Port of the simulation loops in wind_ca_model.R and the helpers
// wind_speed(), wind_direction() and next_cell() in wind_scr.R, plus the fixes
// described below.
//
// Compile / load from R with:   Rcpp::sourceCpp("wind_ca.cpp")
//
// Conventions (same as the R code):
//   * x = column, y = row, both 1-based, row 1 is the top (north) row
//   * wind direction is the meteorological "coming from" bearing, 0-360
//   * the grid is stored row-major, i.e. cell index = (row - 1) * ncol + (col - 1),
//     which is the order terra::values() returns
//
// MODES  (argument `mode` of ca_simulate_cpp)
//   0 legacy  Reproduces the original R model: the number of steps per hour is
//             ceiling(speed * 3600 / cellsize), and every step of an hour starts from the
//             cell the particle was in at the START of that hour (so at most one cell of
//             net movement per hour).
//   1 move    legacy step count, but the particle advances cell by cell inside the hour.
//   2 budget  within-hour movement + DISTANCE BUDGET + PROBABILISTIC ROUNDING.
//             Each hour the particle gets a budget of speed * 3.6 km. Each move spends its
//             true ground length (cell height = dlat * 111.2 km, cell width =
//             dlon * 111.2 * cos(lat) km, diagonals = the hypotenuse). The last move,
//             which would overshoot the budget, is taken with probability
//             remaining_budget / move_length. Expected distance per hour is therefore exact,
//             with no rounding-up bias, no "at least one cell" floor for light winds and no
//             dependence on the nominal `cellsize` (which is ignored in modes 2 and 3).
//   3 fixed   mode 2 + GEOMETRY-AWARE HEADINGS. In cell space the 8 neighbours are not 45
//             degrees apart on the ground (a cell is narrower than it is tall away from the
//             equator), so picking a compass sector still rotates tracks toward north-south.
//             Here the jittered heading is converted to a move by choosing between the two
//             neighbours that bracket it on the ground, with probabilities chosen so the
//             EXPECTED displacement points exactly along the heading.
//
// In modes 2 and 3 the grid must be lon/lat: lat_top is the latitude of the centre of row 1,
// dlat / dlon are the cell sizes in degrees.
//
// Particles that leave the grid are dropped. Calm / NA cells mean no movement.

#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <algorithm>

using namespace Rcpp;

static const double PI_ = 3.14159265358979323846;
static const double KM_PER_DEG = 6371.0088 * PI_ / 180.0;   // km per degree of arc (mean Earth radius)

enum { MODE_LEGACY = 0, MODE_MOVE = 1, MODE_BUDGET = 2, MODE_FIXED = 3 };

// R's  x %% 360  (result always in [0, 360))
static inline double wrap360(double d) {
  d = std::fmod(d, 360.0);
  if (d < 0.0) d += 360.0;
  return d;
}

// meteorological direction from u (east-west) and v (north-south) components
static inline double dir_from_uv(double u, double v) {
  return wrap360(180.0 + std::atan2(u, v) * 180.0 / PI_);
}

// Identical to next_cell() in wind_scr.R. (i, j) = (col, row).
// Returns false if `dir` is NaN (the R version errors in that case).
static inline bool next_cell_impl(double dir, int i, int j, int &ni, int &nj) {
  if (std::isnan(dir)) return false;
  if (dir <= 22.5 || dir > 337.5)       { ni = i;     nj = j + 1; }  // from N -> moves south
  else if (dir <= 67.5)                 { ni = i - 1; nj = j + 1; }
  else if (dir <= 112.5)                { ni = i - 1; nj = j;     }
  else if (dir <= 157.5)                { ni = i - 1; nj = j - 1; }
  else if (dir <= 202.5)                { ni = i;     nj = j - 1; }
  else if (dir <= 247.5)                { ni = i + 1; nj = j - 1; }
  else if (dir <= 292.5)                { ni = i + 1; nj = j;     }
  else                                  { ni = i + 1; nj = j + 1; }
  return true;
}

// ---------------------------------------------------------------------------
// Vectorised helpers, exposed mainly so they can be checked against the R
// originals (wind_speed / wind_direction / next_cell).
// ---------------------------------------------------------------------------

// [[Rcpp::export]]
NumericVector wind_speed_cpp(const NumericVector &u, const NumericVector &v) {
  R_xlen_t n = u.size();
  NumericVector out(n);
  for (R_xlen_t k = 0; k < n; ++k) out[k] = std::sqrt(v[k] * v[k] + u[k] * u[k]);
  return out;
}

// [[Rcpp::export]]
NumericVector wind_direction_cpp(const NumericVector &u, const NumericVector &v) {
  R_xlen_t n = u.size();
  NumericVector out(n);
  for (R_xlen_t k = 0; k < n; ++k) out[k] = dir_from_uv(u[k], v[k]);
  return out;
}

// [[Rcpp::export]]
IntegerVector next_cell_cpp(double dir, int i, int j) {
  int ni, nj;
  if (!next_cell_impl(dir, i, j, ni, nj))
    return IntegerVector::create(NA_INTEGER, NA_INTEGER);
  return IntegerVector::create(ni, nj);
}

// ---------------------------------------------------------------------------
// Movement helpers
// ---------------------------------------------------------------------------

enum MoveStatus { MOVE_OK = 0, MOVE_CALM = 1 };

// Weighted draw of a neighbour of (ax, ay) by weight * wind speed (centre weight 3), then the
// "coming from" heading of that neighbour's wind (+180 if backwards) plus uniform +/-30 degrees.
static inline MoveStatus pick_heading(const double *u, const double *v, int ax, int ay,
                                      int ncol, int nrow, bool backwards, double &heading) {
  static const double WT[9] = {1, 1, 1, 1, 3, 1, 1, 1, 1};   // i (col offset) outer, j (row offset) inner

  R_xlen_t cand_cell[9];
  double   cand_p[9];
  int      cnt = 0;
  double   tot = 0.0;

  for (int i = -1; i <= 1; ++i) {
    for (int j = -1; j <= 1; ++j) {
      const int xi = ax + i, yj = ay + j;
      if (xi < 1 || xi > ncol || yj < 1 || yj > nrow) continue;
      const R_xlen_t c = (R_xlen_t)(yj - 1) * ncol + (xi - 1);
      double sp = std::sqrt(v[c] * v[c] + u[c] * u[c]);
      if (std::isnan(sp)) sp = 0.0;
      const double p = WT[(i + 1) * 3 + (j + 1)] * sp;
      cand_cell[cnt] = c;
      cand_p[cnt]    = p;
      tot += p;
      ++cnt;
    }
  }
  if (!(tot > 0.0)) return MOVE_CALM;                // dead calm everywhere: no movement

  const double r = unif_rand() * tot;
  int sel = -1, last_pos = -1;
  double acc = 0.0;
  for (int q = 0; q < cnt; ++q) {
    if (cand_p[q] > 0.0) last_pos = q;
    acc += cand_p[q];
    if (r < acc) { sel = q; break; }
  }
  if (sel < 0) sel = last_pos;                        // guards against rounding at r ~ tot

  const R_xlen_t cs = cand_cell[sel];
  double d = dir_from_uv(u[cs], v[cs]);
  if (backwards) d = wrap360(d + 180.0);
  d = wrap360(d + (-30.0 + 60.0 * unif_rand()));
  if (std::isnan(d)) return MOVE_CALM;
  heading = d;
  return MOVE_OK;
}

// The 8 neighbours in clockwise order of ground bearing: N, NE, E, SE, S, SW, W, NW.
// (dx = column offset, dy = row offset; rows grow southward.)
static const int MV_DX[8] = { 0,  1, 1, 1, 0, -1, -1, -1};
static const int MV_DY[8] = {-1, -1, 0, 1, 1,  1,  0, -1};

// Ground vector (km east, km north) of move k made from row ay.
static inline void move_vec(int k, int ay, double lat_top, double dlat, double dlon,
                            double &east, double &north) {
  const double lat_a = lat_top - (ay - 1) * dlat;
  const double lat_b = lat_a - MV_DY[k] * dlat;
  east  = MV_DX[k] * dlon * KM_PER_DEG * std::cos(0.5 * (lat_a + lat_b) * PI_ / 180.0);
  north = -MV_DY[k] * dlat * KM_PER_DEG;
}

// Ground length (km) of the move (ax, ay) -> (nx, ny).
static inline double move_cost_km(int ax, int ay, int nx, int ny,
                                  double lat_top, double dlat, double dlon) {
  const double lat_a = lat_top - (ay - 1) * dlat;
  const double lat_b = lat_top - (ny - 1) * dlat;
  const double dy_km = (ny - ay) * dlat * KM_PER_DEG;
  const double dx_km = (nx - ax) * dlon * KM_PER_DEG * std::cos(0.5 * (lat_a + lat_b) * PI_ / 180.0);
  return std::sqrt(dx_km * dx_km + dy_km * dy_km);
}

// Mode 3: turn a "coming from" heading into a move that follows it on the ground.
// The wind blows toward heading + 180. The two neighbours whose ground bearings bracket that
// direction are mixed with probabilities proportional to the coefficients that decompose the
// unit heading vector into the two neighbour vectors, so the expected step points along it.
static inline void choose_move_geo(double from_heading, int ax, int ay,
                                   double lat_top, double dlat, double dlon,
                                   int &nx, int &ny) {
  const double mb = wrap360(from_heading + 180.0);       // bearing the particle should travel along
  double e[8], n[8], th[8];
  for (int k = 0; k < 8; ++k) {
    move_vec(k, ay, lat_top, dlat, dlon, e[k], n[k]);
    double t = std::atan2(e[k], n[k]) * 180.0 / PI_;
    if (t < 0.0) t += 360.0;
    th[k] = t;
  }
  int ka = 7;                                              // default: between NW (7) and N (0)
  for (int k = 0; k < 7; ++k) {
    if (mb >= th[k] && mb < th[k + 1]) { ka = k; break; }
  }
  const int kb = (ka + 1) % 8;

  const double sx = std::sin(mb * PI_ / 180.0), sy = std::cos(mb * PI_ / 180.0);
  const double det = e[ka] * n[kb] - e[kb] * n[ka];
  double alpha = (sx * n[kb] - e[kb] * sy) / det;
  double beta  = (e[ka] * sy - n[ka] * sx) / det;
  if (alpha < 0.0) alpha = 0.0;
  if (beta  < 0.0) beta  = 0.0;
  const double sum = alpha + beta;
  const double qa = (sum > 0.0) ? alpha / sum : 0.5;

  const int k = (unif_rand() < qa) ? ka : kb;
  nx = ax + MV_DX[k];
  ny = ay + MV_DY[k];
}

// ---------------------------------------------------------------------------
// Simulation core: all `nsim` replicates for ONE starting cell.
//
//   U, V          ncell x nlayers matrices of the u / v wind components
//   nrow, ncol    grid dimensions
//   layers        for each time step (in iteration order), 0-based column of U/V to use
//   hours         for each time step, the value stored in the `nforecast` output column
//   start_col/row 1-based starting cell
//   nsim          number of replicates
//   cellsize      cell size in metres, used by modes 0 and 1 only
//   lat_top       latitude of the centre of row 1 (degrees), used by modes 2 and 3
//   dlat, dlon    cell height / width in degrees,           used by modes 2 and 3
//   backwards     TRUE: direction + 180 (hindcast)
//   full          TRUE: also return every visited cell
//   mode          0 legacy, 1 move, 2 budget, 3 fixed (see top of file)
//
// Returns list(counts = integer[ncell], n_exit, and if `full`: col, row, hour, sim).
//   n_exit = number of replicates that left the grid (and were dropped)
// ---------------------------------------------------------------------------

// [[Rcpp::export]]
List ca_simulate_cpp(const NumericMatrix &U, const NumericMatrix &V,
                     int nrow, int ncol,
                     const IntegerVector &layers, const IntegerVector &hours,
                     int start_col, int start_row,
                     int nsim, double cellsize,
                     double lat_top, double dlat, double dlon,
                     bool backwards = false, bool full = false,
                     int mode = 3) {   // 3 = MODE_FIXED

  const R_xlen_t ncell = (R_xlen_t)nrow * ncol;
  const int nsteps_total = layers.size();

  // ---- input checks -------------------------------------------------------
  if (U.nrow() != ncell || V.nrow() != ncell || U.ncol() != V.ncol())
    stop("U and V must both have nrow*ncol rows and the same number of columns");
  if (hours.size() != nsteps_total)
    stop("`hours` and `layers` must have the same length");
  for (int k = 0; k < nsteps_total; ++k)
    if (layers[k] < 0 || layers[k] >= U.ncol()) stop("`layers` index out of range");
  if (start_col < 1 || start_col > ncol || start_row < 1 || start_row > nrow)
    stop("starting cell is outside the grid");
  if (mode < MODE_LEGACY || mode > MODE_FIXED) stop("mode must be 0, 1, 2 or 3");
  const bool geo = (mode >= MODE_BUDGET);
  if (geo) {
    if (ISNAN(lat_top) || ISNAN(dlat) || ISNAN(dlon) || dlat <= 0 || dlon <= 0)
      stop("modes 'budget' and 'fixed' need lat_top, dlat and dlon (a lon/lat grid)");
  } else if (cellsize <= 0) {
    stop("cellsize must be positive");
  }

  IntegerVector counts(ncell);   // zero-initialised
  int n_exit = 0;                // replicates that left the grid
  std::vector<int> o_col, o_row, o_hour, o_sim;
  if (full) {
    size_t guess = (size_t)nsim * (size_t)(nsteps_total * 3 + 1);
    o_col.reserve(guess); o_row.reserve(guess);
    o_hour.reserve(guess); o_sim.reserve(guess);
  }

  for (int s = 0; s < nsim; ++s) {
    if ((s & 255) == 0) Rcpp::checkUserInterrupt();

    int x = start_col, y = start_row;
    if (full) { o_col.push_back(x); o_row.push_back(y); o_hour.push_back(0); o_sim.push_back(s + 1); }

    bool alive = true;

    for (int k = 0; k < nsteps_total && alive; ++k) {
      const double *u = U.begin() + (R_xlen_t)layers[k] * ncell;
      const double *v = V.begin() + (R_xlen_t)layers[k] * ncell;
      const int f = hours[k];

      // wind speed in the particle's current cell
      const R_xlen_t c0 = (R_xlen_t)(y - 1) * ncol + (x - 1);
      const double spd_c = std::sqrt(v[c0] * v[c0] + u[c0] * u[c0]);
      if (std::isnan(spd_c)) continue;            // no wind data here: particle waits

      int ax = x, ay = y;   // cell the neighbourhood is centred on
      int lx = x, ly = y;   // last cell visited this hour

      if (geo) {
        // ---- distance budget with probabilistic rounding of the last move -------------
        double budget = spd_c * 3.6;               // km this hour = m/s * 3600 s / 1000
        for (int guard = 0; budget > 0.0 && guard < 100000; ++guard) {
          double heading;
          if (pick_heading(u, v, ax, ay, ncol, nrow, backwards, heading) != MOVE_OK) break;

          int nx, ny;
          if (mode == MODE_FIXED) {
            choose_move_geo(heading, ax, ay, lat_top, dlat, dlon, nx, ny);
          } else if (!next_cell_impl(heading, ax, ay, nx, ny)) {
            break;
          }

          const double cost = move_cost_km(ax, ay, nx, ny, lat_top, dlat, dlon);
          if (!(cost > 0.0)) break;
          if (cost > budget && unif_rand() >= budget / cost) break;   // last, partial move not taken
          budget -= cost;

          if (nx < 1 || nx > ncol || ny < 1 || ny > nrow) { alive = false; break; }   // left the grid

          counts[(R_xlen_t)(ny - 1) * ncol + (nx - 1)] += 1;
          if (full) { o_col.push_back(nx); o_row.push_back(ny); o_hour.push_back(f); o_sim.push_back(s + 1); }
          lx = nx; ly = ny; ax = nx; ay = ny;
        }
      } else {
        // ---- legacy / move: number of steps = max(coin flip, ceiling(speed*3600/cellsize)) ----
        const int coin = (int)(unif_rand() * 2.0);
        const double need = std::ceil(spd_c * 3600.0 / cellsize);
        const int steps = (int)std::max((double)coin, need);
        if (steps < 1) continue;

        for (int e = 0; e < steps; ++e) {
          double heading;
          if (pick_heading(u, v, ax, ay, ncol, nrow, backwards, heading) != MOVE_OK) break;

          int nx, ny;
          if (!next_cell_impl(heading, ax, ay, nx, ny)) break;
          if (nx < 1 || nx > ncol || ny < 1 || ny > nrow) { alive = false; break; }

          counts[(R_xlen_t)(ny - 1) * ncol + (nx - 1)] += 1;
          if (full) { o_col.push_back(nx); o_row.push_back(ny); o_hour.push_back(f); o_sim.push_back(s + 1); }

          lx = nx; ly = ny;
          if (mode == MODE_MOVE) { ax = nx; ay = ny; }   // legacy keeps the hour-start anchor
        }
      }

      x = lx; y = ly;   // the next hour starts from the last cell visited
    }
    if (!alive) ++n_exit;
  }

  List out = List::create(_["counts"] = counts, _["n_exit"] = n_exit);
  if (full) {
    out["col"]  = IntegerVector(o_col.begin(),  o_col.end());
    out["row"]  = IntegerVector(o_row.begin(),  o_row.end());
    out["hour"] = IntegerVector(o_hour.begin(), o_hour.end());
    out["sim"]  = IntegerVector(o_sim.begin(),  o_sim.end());
  }
  return out;
}
