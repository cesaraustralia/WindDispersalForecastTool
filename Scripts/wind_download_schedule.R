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

# r <-  terra::rast("https://s3-ap-southeast-2.amazonaws.com/silo-open-data/daily/max_temp/2018/20180103.max_temp.tif")
# inMemory(r)
# terra::sources(r)
