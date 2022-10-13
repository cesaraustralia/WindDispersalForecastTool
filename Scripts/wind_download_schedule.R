# read the wind functions
source("R/wind_scr.R")
source("R/wind_ca_model.R")

# delete wind data older than 14 days
maindir <- "wind-data"
datename <- format(lubridate::today() - 15, "%Y%m%d")
datedir <- file.path(maindir, datename)
unlink(datedir, recursive = T)

# download wind data for previous day
maindir <- "wind-data"
datename <- format(lubridate::today() - 1, "%Y%m%d")
datedir <- file.path(maindir, datename)
dir.create(datedir)

for(i in  c("00", "06", "12", "18")){

  dir.create(file.path(datedir, i))

  download_wind(
    dates = datename,
    time = i,
    nforecast = 48,
    level_mb = 850,
    outdir = file.path(datedir, i)
  )

  download_wind(
    dates = datename,
    time = i,
    nforecast = 48,
    level_mb = 950,
    outdir = file.path(datedir, i)
  )
}


