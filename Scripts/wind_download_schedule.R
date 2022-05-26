# download wind data day
maindir <- "~/wind-data"
datename <- format(lubridate::today() - 1, "%Y%m%d")
datedir <- file.path(maindir, datename)
dir.create(datedir)

for(i in  c("00", "06", "12", "18")){

  dir.create(file.path(datedir, i))

  Sys.sleep(1)

  download_wind(
    dates = datename,
    time = i,
    nforecast = 48,
    level_mb = 850,
    outdir = file.path(datedir, i)
  )

  Sys.sleep(3)

}

