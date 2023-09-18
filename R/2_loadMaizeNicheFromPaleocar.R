library(magrittr)
library(paleocar)
library(ggplot2)
# library(villager)

study_area <-
  robinson2020::vep_study_areas %>%
  dplyr::filter(`Study Area` == "CMV") %>%
  sf::st_transform(26912)
# villager::vepii_cmv_boundary

# Get the ITRDB for a 300km buffer around the VEPII N study area
itrdb <-
  FedData::get_itrdb(template =
                       study_area %>%
                       sf::st_buffer(300000) %>%
                       as("Spatial"),
                     label = "vepii_cmv",
                     recon.years = 1:2000,
                     calib.years = 1924:1983,
                     measurement.type = "Ring Width",
                     chronology.type = "ARSTND",
                     raw.dir = "./data/data-raw/itrdb",
                     extraction.dir = "./data/data-derived")

jja_pdsi <-
  tidyr::expand_grid(Year = 1920:2012,
                     Month = 1:12) %>%
  dplyr::mutate(PDSI = raster::brick("data/data-raw/vepiin_raster_pdsi_monthly_1920-2012.tif") %>%
                  raster::readAll() %>%
                  raster::as.list()) %>%
  dplyr::filter(Month %in% 6:8, 
                Year %in% 1924:1983) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(PDSI = list(raster::mean(raster::stack(PDSI)))) %$%
  PDSI %>%
  raster::stack() %>%
  raster::brick() %>%
  raster::readAll()

#this doesn't work anymore
jja_pdsi_paleocar <- 
  paleocar::paleocar(predictands = jja_pdsi,
                     label = "jja_pdsi",
                     chronologies = itrdb,
                     calibration.years = 1924:1983,
                     prediction.years = 450:1300,
                     verbose = T,
                     out.dir = "data/data-derived/")


#read in pdsi results
jja_pdsi_paleocar <- readr::read_rds(
  here::here("data/data-derived/jja_pdsi.prediction.Rds")
)

pecos <- 
  tibble::tibble(
    pecos = c("BMII", "BMIII", "PI", "EPII", "LPII", "EPIII", "LPIII"), #Bocinsky 2016 exploration/Exploitation ages
    StartDate = c(-500, 500, 700, 890, 1035, 1145, 1200),
    EndDate = c(500, 700, 890, 1035, 1145, 1200, 1285)) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(Year = list(StartDate:(EndDate - 1))) %>%
  tidyr::unnest(Year) %>%
  dplyr::select(pecos, Year) %>%
  dplyr::mutate(pecos = factor(pecos,
                               levels = unique(pecos),
                               ordered = TRUE))



movingAverage <- function(x,n){ 
  stats::filter(x = x, 
                filter = rep(1/n, n), 
                method = "convolution", 
                sides = 1, 
                circular = FALSE)
}

cmv_pdsi <- 
  jja_pdsi_paleocar$`Prediction (scaled)` %>% 
  raster::cellStats(stat = 'mean') %>% 
  tibble::as_tibble(
    rownames = c("Year")
  ) %>% 
  dplyr::rename(`Mean CMV PDSI` = value) %>% 
  dplyr::mutate(Year = as.numeric(stringr::str_remove(Year, "X")),
                `10 year Mean CMV PDSI` = movingAverage(`Mean CMV PDSI`, 10),
                `5 year Mean CMV PDSI` = movingAverage(`Mean CMV PDSI`, 5)) %>% 
  dplyr::left_join(pecos, by = "Year") 

violence <- 
  tibble::tibble(
    beginViolence = c(870, 1140, 1270),
    endViolence = c(879, 1160,1279)
  )

readr::write_rds(cmv_pdsi,
                 here::here("data/data-derived/cmv_pdsi.rds"))

ggplot(aes(x=Year, y=`10 year Mean CMV PDSI`), data = cmv_pdsi) +
  geom_line(group = 1)

ggplot() +
  geom_rect(
    aes(xmin = beginViolence, 
        xmax = endViolence, 
        ymin  = -Inf, 
        ymax = Inf),
    fill = c("red"),
    alpha = 0.2,
    data = violence  )+
  geom_line(group = 1, aes(x = Year, y=`5 year Mean CMV PDSI`),
            data = cmv_pdsi) +
  geom_vline(aes(xintercept = Year), 
             linetype = "dashed", 
             data = cmv_pdsi %>% 
               dplyr::group_by(pecos) %>% 
               dplyr::summarise(Year = max(Year)))+
  geom_text(aes(x = midYear, y = Inf, label = pecos), 
            vjust = 2,
            size = 5,
            data = cmv_pdsi %>% 
              dplyr::group_by(pecos) %>% 
              dplyr::summarise(midYear = mean(Year)))+
  theme_bw()+
  theme(axis.text = element_text(size =16),
        axis.title = element_text(size = 18))

ggsave(filename = "images/regional_pdsi.jpg", width = 10, height = 5)

cmv_pdsi %>% 
  dplyr::group_by(pecos) %>% 
  dplyr::summarise(max(Year))

#graph one year
raster::plot(jja_pdsi_paleocar$`Prediction (scaled)`[['X1280']])

plot(y = raster::cellStats(jja_pdsi_paleocar$`Prediction (scaled)`, mean, na.rm = TRUE),
     x = 450:1300,
     type = "l")

## PaleoCAR V3 maize niche:
# test <- 
#   terra::rast("/Volumes/GoogleDrive-106929513169623498168/My\ Drive/RESEARCH/SKOPE\ II/datasets/paleocar_v3/data-derived/paleocar/ppt_water_year.103-2000.vrt") %>%
#   terra::crop(study_area %>%
#                 sf::st_transform("EPSG:4326"))


