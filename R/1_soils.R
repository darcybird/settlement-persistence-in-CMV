library(magrittr)
library(tidyverse)
library(terra)
library(robinson2020)
library(tidyterra)
library(fasterize)
library(FedData)
library(soilDB)

 
study_area_grid  <-
  robinson2020::vep_study_areas %>%
  dplyr::filter(`Study Area` == "CMV") %>%
  terra::vect() %>% 
  terra::rasterize(
    y =  terra::rast(xmin = 672800, xmax = 740000, ymin = 4102000, ymax = 4170000,
                                    crs = "EPSG:26912",
                                    resolution = 200
  ), values = 0) %>%
  terra::extend(y = 15)

values(study_area_grid) <- 0

##### SSURGO CODE ----

#load in soils data
ssurgo <- 
  FedData::get_ssurgo(template = c("CO670",
                                   "CO671",
                                   "CO672"),
                      label = "survey_areas",
                      extraction.dir = "data/data-downloaded/ssurgo",
                      raw.dir = "data/data-downloaded/ssurgo")



# Get Reservoirs from NHD, and Dams from SSURGO
# Buffer each at 100m
nhd_reservoirs <- 
  FedData::get_nhd(template = study_area_grid,
                   label = "study_area",
                   extraction.dir = "data/data-downloaded/nhd") %$%
  Waterbody %>% 
  dplyr::filter(
    stringr::str_starts(FCODE, stringr::fixed("43")) | 
      stringr::str_detect(stringr::str_to_lower(GNIS_NAME), "reservoir") |
      GNIS_NAME %in% c("Totten Lake", "Bauer Lake")) %>%
  sf::st_buffer(100) %>%
  sf::st_geometry()

ssurgo_dams <-
  ssurgo$spatial %>%
  dplyr::mutate(MUKEY = as.integer(MUKEY)) %>%
  dplyr::left_join(ssurgo$tabular$mapunit,
                   by = c("MUKEY" = "mukey")) %>%
  dplyr::filter(muname == "Dam") %>%
  sf::st_buffer(100) %>%
  sf::st_geometry()

dams_reservoirs <-
  c(nhd_reservoirs, ssurgo_dams) %>%
  sf::st_union()

## create a spatial interpolation of handplant data where reservoirs and poor data exist

#make names easier and standardize
handplantable <- 
  soilDB::get_SDA_interpretation(
    rulename = c("FOR - Hand Planting Suitability"),
    method = "none",
    areasymbols = c("CO670",
                    "CO671",
                    "CO672")) %>% 
  tibble::as_tibble() %>%
  #keep only moderately or well-suited handplanting quality
  dplyr::mutate(suitable_percent = ifelse(class_FORHandPlantingSuitability %in% 
                                            c("Moderately suited", 
                                              "Well suited"), 
                                          comppct_r, 0)) %>%
  
  dplyr::group_by(mukey, muname) %>%
  dplyr::summarise(suitable_percent = sum(suitable_percent))


## Use the VEP Drained DEM and fill edges with undrained DEM
vep_drained <- terra::rast(here::here("data/data-raw/ned/VEPIIN_NED_1_drained.tif"))
study_area_dem <- terra::rast(here::here("data/data-raw/ned/study_area_NED_1.tif"))

study_area_drained_dem <-
  vep_drained %>%
  terra::project(study_area_dem)

study_area_drained_dem[is.na(study_area_drained_dem)] <- #remove the NAs
  study_area_dem[is.na(study_area_drained_dem)]

study_area_drained_dem %>% plot()


# assign spatial coordinations and Rasterize the mukeys
mukey_rast <- 
  ssurgo$spatial %>% 
  dplyr::mutate(mukey = as.integer(MUKEY)) %>% 
  dplyr::left_join(handplantable, by = c("mukey" = "mukey")) %>%
  dplyr::select(mukey, muname, suitable_percent) %>% 
  sf::st_transform(crs= crs(study_area_drained_dem) ) %>% 
  # dplyr::select(suitable_percent) %>% 
  dplyr::select(mukey) %>% 
  fasterize::fasterize(., 
                       raster::raster(study_area_drained_dem), 
                       field = "mukey") %>% 
  terra::rast()

names(mukey_rast) <- c("mukey")

mukey_rast %<>% #remove reservoirs and dams from the handplant suitability 
  terra::mask(dams_reservoirs %>%
                sf::st_transform(crs = terra::crs(mukey_rast)) %>%
                terra::vect(),
              inverse = TRUE)

## Estimate soils under reservoir using naive Bayes
ssurgo_model <-
  list(mukey_rast,
       study_area_drained_dem, 
       terra::terrain(study_area_drained_dem,
                      v = "TPI"),
       terra::terrain(study_area_drained_dem,
                      v = "aspect")
       ) %>%
  terra::rast() %>%
  terra::as.data.frame(xy = TRUE) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(mukey = as.factor(mukey)) %>%
  e1071::naiveBayes(mukey ~ x + y + VEPIIN_NED_1_drained, + TPI, 
                    data = .)

#fill all the NAs with the modelled values
mukey_rast[is.na(mukey_rast)] <-
  predict(ssurgo_model,
          newdata = list(mukey_rast,
                         study_area_drained_dem, 
                         terra::terrain(study_area_drained_dem,
                                        v = "TPI"),
                         terra::terrain(study_area_drained_dem,
                                        v = "aspect")
          ) %>%
            terra::rast() %>%
            terra::as.data.frame(xy = TRUE) %>%
            tibble::as_tibble() %>%
            dplyr::filter(is.na(mukey))) %>%
  as.character() %>%
  as.integer()

mukey_rast %>% plot()

# Substitute the hand planting numbers
handplanting_proportion <-
  terra::subst(mukey_rast,
               from = handplantable$mukey,
               to = handplantable$suitable_percent)

handplanting_proportion %>% plot()

#resample to match other data
handplanting_200 <- terra::resample(
  handplanting_proportion %>% 
    terra::project(y=crs(study_area_grid)), 
  study_area_grid, 
  method = "bilinear")

handplanting_200 %>% plot()

names(handplanting_200) <- "handplant"

handplanting_200 <- handplanting_200 / 100 # make a proportion between 0 and 1

handplanting_200 %>% 
  terra::writeRaster("data/data-derived/soils/vepiin_hand_planting_200.tif",
                     overwrite = TRUE)
