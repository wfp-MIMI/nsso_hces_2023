

# 
#   
# new_shapefile <- st_read("C:/Users/gabriel.battcock/Downloads/india_adm2_shp/DISTRICT_BOUNDARY.shp")
# # 
# # ind_sahpefile_names <- new_shapefile %>% 
# #   select(District, STATE,State_LGD, DISTRICT_L) %>% 
# #   st_drop_geometry() %>% 
# #   filter(State_LGD == 07)
# Author: Gabriel Battcock
# Created: 
# Last updated: 18 August 2024

rq_packages <- c("tidyverse","dplyr","readr","srvyr","ggplot2", "tidyr",
                 "ggridges", "gt", "haven","foreign",
                 "tmap","sf","rmapshaper","readxl","hrbrthemes",
                 "wesanderson","treemap","treemapify")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))



#
# delhi <- new_shapefile%>%
#   select(District, STATE,State_LGD, DISTRICT_L) %>%
#   filter(State_LGD == 07)
# plot(delhi$geometry)

nss2223_shp_dictionary <- read.csv("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/ind_new_shapefile.csv")

nss_region_shapefile <-   level01 %>%
  distinct(state,nss_region,district) %>%
  mutate(district = as.numeric(paste0(state, district)),
         state = as.numeric(state),
         nss_region = as.numeric(nss_region)) %>%
  left_join(nss2223_shp_dictionary, by=c("district" = "adm2_code")) %>%
  mutate(DISTRICT_L = as.character(DISTRICT_L)) %>%
  select(state,nss_region,district,State_LGD, DISTRICT_L) %>%
  left_join(new_shapefile , by= c("State_LGD", "DISTRICT_L")) %>%
  group_by(nss_region) %>%
  summarise(geometry = sf::st_union(geometry))

plot(nss_region_shapefile$geometry,col = "red")

# sf::st_write(nss_region_shapefile, "C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/ind_nss2223_nssregion.shp")

state <- new_shapefile %>%
  group_by(State_LGD) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup()


plot(state$geometry, col = 'red')

# write.csv(x, "C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/ind_new_nss_shapefile.csv")