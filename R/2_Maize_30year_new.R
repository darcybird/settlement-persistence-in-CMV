library(terra)
library(magrittr)
library(tidyverse)

gc()


#prep the maize -----
maizeNiche <- terra::rast(here::here("data/data-raw/paleocar/mfn_paleocar.tif"))
names(maizeNiche)  <- stringr::str_replace_all(names(maizeNiche), "X", "") 

maizeNiche_1 <- maizeNiche[[120:361]]
maizeNiche_2 <- maizeNiche[[(361-30):601]]
maizeNiche_3 <- maizeNiche[[(601-30):771]]
maizeNiche_4 <- maizeNiche[[(771-30):851]]

remove(maizeNiche)

gc()

#calculate average years in maize niche in past n years
movingRasterAverage <- function(rasterBrick, n){
  out <- rasterBrick[[n-1]] #this is used to mark the starter year for all subsequent calculations
  values(out) <- 0
  
  purrr::map(.x = n:(terra::nlyr(rasterBrick)), .f = function(i){
    currentyear <- terra::mean(rasterBrick[[(i-n):(i-1)]]) #consider n years prior to current year (i-n) through the last year (i-1)
    names(currentyear) <- names(rasterBrick[[i]]) #rename this year 
    out <<- c(out, currentyear) #combine with previous years
  })
  
  out <- out[[2:nlyr(out)]] #remove the starter year
  return(out)
}

#produce and export
maizeNiche_810_30yrAverage <- movingRasterAverage(rasterBrick = maizeNiche_1, n=30)
terra::writeRaster(maizeNiche_810_30yrAverage, 
                   here::here("data/data-derived/maizeNiche/maizeNiche_810_30yrAverage.tif"))

remove(maizeNiche_810_30yrAverage)
remove(maizeNiche_1)
gc()

maizeNiche_1050_30yrAverage <- movingRasterAverage(rasterBrick = maizeNiche_2, n=30)
terra::writeRaster(maizeNiche_1050_30yrAverage, 
                   here::here("data/data-derived/maizeNiche/maizeNiche_1050_30yrAverage.tif"))
remove(maizeNiche_1050_30yrAverage)
remove(maizeNiche_2)
gc()


maizeNiche_1220_30yrAverage <- movingRasterAverage(rasterBrick = maizeNiche_3, n=30)
terra::writeRaster(maizeNiche_1220_30yrAverage, 
                   here::here("data/data-derived/maizeNiche/maizeNiche_1220_30yrAverage.tif"))
remove(maizeNiche_1220_30yrAverage)
remove(maizeNiche_3)
gc()


maizeNiche_1300_30yrAverage <- movingRasterAverage(rasterBrick = maizeNiche_4, n=30)
terra::writeRaster(maizeNiche_1300_30yrAverage, 
                   here::here("data/data-derived/maizeNiche/maizeNiche_1300_30yrAverage.tif"))
remove(maizeNiche_1300_30yrAverage)
remove(maizeNiche_4)
gc()


#load maize and combine ----

maizeNiche_810_30yrAverage <- terra::rast("data/data-derived/maizeNiche/maizeNiche_810_30yrAverage.tif")
maizeNiche_1050_30yrAverage <- terra::rast("data/data-derived/maizeNiche/maizeNiche_1050_30yrAverage.tif")
maizeNiche_1220_30yrAverage <- terra::rast("data/data-derived/maizeNiche/maizeNiche_1220_30yrAverage.tif")
maizeNiche_1300_30yrAverage <- terra::rast("data/data-derived/maizeNiche/maizeNiche_1300_30yrAverage.tif")

maizeNiche_810_30yrAverage
maizeNiche_810_30yrAverage <- maizeNiche_810_30yrAverage[[3:213]]
maizeNiche_810_30yrAverage[[213]]

maizeNiche_1050_30yrAverage
maizeNiche_1050_30yrAverage <- maizeNiche_1050_30yrAverage[[3:242]]
maizeNiche_1050_30yrAverage[[240]]

maizeNiche_1220_30yrAverage
maizeNiche_1220_30yrAverage <- maizeNiche_1220_30yrAverage[[3:172]]
maizeNiche_1220_30yrAverage[[170]]

maizeNiche_1300_30yrAverage
maizeNiche_1300_30yrAverage <- maizeNiche_1300_30yrAverage[[3:82]]
maizeNiche_1300_30yrAverage[[80]]

maizeNiche_30yrAverage <- c(maizeNiche_810_30yrAverage,maizeNiche_1050_30yrAverage,maizeNiche_1220_30yrAverage,maizeNiche_1300_30yrAverage)
maizeNiche_30yrAverage
maizeNiche_30yrAverage[[701]]

## ACCOMODATE SOILS ----

maizeNiche_30yrAverage[['600']] %>% plot()

# make catchment baselines
handplant <- terra::rast("data/data-derived/soils/vepiin_hand_planting_200.tif") 
handplant %>% plot()

# remove bad soils by multiplying by handplant 
maizeWts <- maizeNiche_30yrAverage * handplant 

maizeWts[['600']] %>% plot()

#export
terra::writeRaster(maizeWts, 
                   here::here("data/data-derived/maizeNiche/maizeWts.tif"),
                   overwrite = TRUE)
