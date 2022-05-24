# extract the component from names
gfs_names <- function(path = "Data/"){
  files <- list.files(path, pattern = "^gfs_")

  gfs_names <- strsplit(files, "_") %>%
    map(function(x){
      matrix(x, ncol = 5, byrow = FALSE) %>%
        as.data.frame() %>%
        setNames(c("gfs", "comp", "date", "start", "forecast")) %>%
        dplyr::select(-gfs)
    }) %>%
    do.call(rbind.data.frame, .) %>%
    mutate(file = files) %>%
    relocate(file)

  return(gfs_names)
}

wind_direction <- function(u, v){
  at <- 180 + (atan2(u, v) * 180 / pi)
  ad <- terra::app(at, function(x) x %% 360)
  return(ad)
}

wind_speed <- function(u, v){
  r <- sqrt(v*v + u*u)
  return(r)
}


# read the u component
read_u <- function(path = "Data/", files_list, fcast){
  files_list %>%
    dplyr::filter(comp == "ugrd") %>%
    dplyr::filter(forecast == fcast) %>%
    pull(file) %>%
    file.path(path, .) %>%
    terra::rast()
}
# read the u component
read_v <- function(path = "Data/", files_list, fcast){
  files_list %>%
    dplyr::filter(comp == "vgrd") %>%
    dplyr::filter(forecast == fcast) %>%
    pull(file) %>%
    file.path(path, .) %>%
    terra::rast()
}


next_cell <- function(x, i, j){
  inc <- 1
  # define the direction
  if (x <= 22.5 || x > 337.5){
    ## N
    return(c(i, j + inc))
  } else if (x > 22.5 && x <= 67.5){
    ## NE
    return(c(i - inc, j + inc))
  } else if (x > 67.5 && x <= 112.5){
    ## E
    return(c(i - inc, j))
  } else if (x > 112.5 && x <= 157.5){
    ## SE
    return(c(i - inc, j - inc))
  } else if (x > 157.5 && x <= 202.5){
    ## S
    return(c(i, j - inc))
  } else if (x > 202.5 && x <= 247.5){
    ## SW
    return(c(i + inc, j - inc))
  } else if (x > 247.5 && x <= 292.5){
    ## W
    return(c(i + inc, j))
  } else if (x > 292.5 && x <= 337.5){
    ## NW
    return(c(i + inc, j + inc))
  }
}
