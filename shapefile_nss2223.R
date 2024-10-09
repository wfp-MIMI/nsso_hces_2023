
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

#-------------------------------------------------------------------------------

file_list = list.files("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/")
haven::read_dta("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/level06.dta")
data_list <- lapply(paste0("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/",file_list), haven::read_dta)
names(data_list) <- tools::file_path_sans_ext(file_list)


ind_state <- sf::st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp")
ind_admin2 <- st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Nutrition analysis/shapefiles/ind_lss1819_adm2.shp")
plot(ind_state$geometry)



#-------------------------------------------------------------------------------
# delhi <-
# ind_districts <-  new_shapefile %>%
#    select(District, STATE,State_LGD, DISTRICT_L) %>% 
#     st_drop_geometry()
# 
# ind_codes_nss <- data_list$level01 %>%
#   distinct(state,nss_region,district) %>%
#   mutate(district = as.numeric(paste0(state, district)),
#          state = as.numeric(state),
#          nss_region = as.numeric(nss_region)) %>%
#   left_join(nss2223_shp_dictionary, by=c("district" = "adm2_code"))

# write_csv(ind_codes_nss, 'ind_codes_nss.csv')

#   filter(State_LGD == 07)
# plot(delhi$geometry)

# read in csv that converts code from shapefile to nss region codes
nss2223_shp_dictionary <- read.csv("ind_codes_nss.csv")

nss_region_shapefile <-   data_list$level01 %>%
  distinct(state,nss_region,district) %>%
  mutate(district = as.numeric(paste0(state, district)),
         state = as.numeric(state),
         nss_region = as.numeric(nss_region)) %>%
  left_join(nss2223_shp_dictionary, by = c("state", "nss_region","district")) %>%
  mutate(DISTRICT_L = as.character(DISTRICT_L)) %>%
  select(state,nss_region,district,State_LGD, DISTRICT_L) %>%
  left_join(new_shapefile , by= c("State_LGD", "DISTRICT_L")) %>%
  group_by(nss_region) %>%
  summarise(geometry = sf::st_union(geometry))

plot(nss_region_shapefile$geometry,col = "blue")

assam <- nss_region_shapefile %>% 
  filter(nss_region>180 & nss_region<190) 

plot(assam$geometry)

# sf::st_write(nss_region_shapefile, "C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/ind_nss2223_nssregion.shp")

state <- new_shapefile %>%
  group_by(State_LGD) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup()


plot(state$geometry, col = 'red')




# 
#   
# new_shapefile <- st_read("C:/Users/gabriel.battcock/Downloads/india_adm2_shp/DISTRICT_BOUNDARY.shp")
# # 
# # ind_sahpefile_names <- new_shapefile %>% 
# #   select(District, STATE,State_LGD, DISTRICT_L) %>% 
# #   st_drop_geometry() %>% 
# #   filter(State_LGD == 07)
# write.csv(x, "C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/ind_new_nss_shapefile.csv")