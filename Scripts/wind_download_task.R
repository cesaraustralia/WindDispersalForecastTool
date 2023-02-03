library(cronR)

## Run every day at the same time on 03:00, starting from tomorrow
daily_wind_download <- cron_rscript(rscript = "Scripts/wind_download_schedule.R")

cron_add(daily_wind_download, frequency = 'daily', id = 'daily_wind_download', at = '03:00')

## delete the tasks
# cron_clear()
