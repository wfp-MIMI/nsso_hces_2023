# Author: Gabriel Battcock
# Created: 21 August 2024
# Last updated: 

rq_packages <- c("tidyverse","dplyr","readr","srvyr","ggplot2", "tidyr",
                 "ggridges", "gt", "haven","foreign",
                 "tmap","sf","rmapshaper","readxl","hrbrthemes",
                 "wesanderson","treemap","treemapify", "biscale", "cowplot")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))


# read in data -----------------------------------------------------------------


file_list = list.files("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/")
haven::read_dta("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/level06.dta")
data_list <- lapply(paste0("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/",file_list), haven::read_dta)
names(data_list) <- tools::file_path_sans_ext(file_list)


ind_state <- sf::st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp")
ind_admin2 <- st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Nutrition analysis/shapefiles/ind_lss1819_adm2.shp")
plot(ind_state$geometry)


# find  ##############################################################

# hoseuholds from UP
level01_UP <- data_list$level01 %>% 
  filter(state %in% c("09"))


up_districts <-   nss2223_shp_dictionary %>%
  filter(adm1_code == 9) %>% 
  mutate(DISTRICT_L = as.character(DISTRICT_L)) %>%
  select(State_LGD, DISTRICT_L) %>%
  left_join(new_shapefile , by= c("State_LGD", "DISTRICT_L")) 
 

up_district_centroid <- up_districts %>% 
  mutate(geometry = st_centroid(geometry))

x <- level01_UP %>% 
  left_join(up_district_centroid, by  = c("district" = "DISTRICT_L"))

plot(x$geometry)

