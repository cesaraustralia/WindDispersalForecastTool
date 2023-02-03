source("Scripts/wind_download.R")

# download wind data for previous day
maindir <- "wind-data"
datename <- format(lubridate::today() - 1, "%Y%m%d")
datedir <- file.path(maindir, datename)
dir.create(datedir)

for(k in c(850, 950)){
  for(i in  c("00", "06", "12", "18")){

    dir.create(file.path(datedir, i))

    Sys.sleep(1)

    download_wind(
      dates = datename,
      time = i,
      nforecast = 48,
      level_mb = k,
      outdir = file.path(datedir, i)
    )

    Sys.sleep(3)

  }
}

# delete wind data from previous week
lastweek <- format(lubridate::today() - 8, "%Y%m%d")

unlink(file.path(maindir, lastweek),
       recursive = T,
       force = T)


