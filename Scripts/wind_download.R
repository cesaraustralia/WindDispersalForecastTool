# Roozbeh Valavi
# 2022-05-24

# download 0.25 degree GFS wind forecast from NOAA API
# the portal only provides wind for the past few days
# the name of the files are like "gfs_vgrd_850mb_20220521_t00z_f000":
# gfs, wind component, atmosphere level, data of forecast, forecast start time, forecast number
download_wind <- function(dates,
                          time = c("00", "06", "12", "18"),
                          nforecast = 1,
                          components = c('VGRD', 'UGRD'),
                          level = 850,
                          outdir = "",
                          dlmethod = NULL, # null select based on the OS
                          xleft = 100,
                          xright = 160,
                          ytop = 1,
                          ydown = -50){

  require(stringr)

  components <- toupper(components)

  # choose download method based on operating system
  if(is.null(dlmethod)){
    if(.Platform$OS.type == "windows"){
      dlmethod <- "curl"
    } else if(.Platform$OS.type == "unix"){
      dlmethod <- "wget"
    }
  }

  xleft <- as.character(xleft)
  xright <- as.character(xright)
  ytop <- as.character(ytop)
  ydown <- as.character(ydown)

  # convert to string and pad 0s if necessary
  time <- stringr::str_pad(string = as.character(time),
                           width = 2,
                           side = "left",
                           pad = "0")
  time <- match.arg(time)

  # it could be 385?
  if(nforecast > 72){
    nforecast <- 72
  }

  if(nchar(outdir) < 1){
    outdir <- getwd()
  }


  base = "https://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl?file=gfs.t%sz.pgrb2.0p25.f%s&lev_%s_mb=on&var_%s=on&subregion=&leftlon=%s&rightlon=%s&toplat=%s&bottomlat=%s&dir=%%2Fgfs.%s%%2F%s%%2Fatmos"

  # add another for loop if you want more than one level
  for(i in  dates){
    for(j in seq_len(nforecast)){
      print(paste("forecast:", j))
      j <- j - 1
      for(k in components){
        nf <- stringr::str_pad(string = as.character(j),
                               width = 3,
                               side = "left",
                               pad = "0")

        name <- sprintf("gfs_%s_%smb_%s_t%sz_f%s", tolower(k), level, i, time, nf)
        out <- file.path(outdir, name)

        url <- sprintf(base, time, nf, level, k, xleft, xright, ytop, ydown, i, time)
        # write the file to disk
        download.file(url, destfile = out, method = dlmethod)

      }
    }
  }
}

