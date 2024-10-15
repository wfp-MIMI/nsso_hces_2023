#########################################
#      HCES 2022-23 INDIA               #
#          inadequacy maps              #
#########################################


# Author: Gabriel Battcock
# Created: 
# Last updated: 29 April 24

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


# sorce iron full probability functions

source(here::here("../MIMI1_archive/universal_functions/iron_full_probability/src/iron_inad_prev.R"))

################################################################################

food_consumption_daily_afe <- readRDS("ind_nss2223_food_consumption.rds")
hh_mn_intake <- readRDS("ind_nss2223_base_case.rds")
# read in the fct
ind_202223_fct <-  read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/nsso_202223_fct.xlsx")


ind_state <- sf::st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp")
ind_admin2 <- st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Nutrition analysis/shapefiles/ind_lss1819_adm2.shp")
nss_region_shapefile <- sf::st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/ind_nss2223_nssregion.shp")


################################################################################



# map function 

inadequacy_map <- function(micronutrient,
                           title = ""
){
  
  # creates a map of risk of inadequate intake for chosen mn and scenario
  # without a legend
  tm_shape(ind_state) +
    tm_fill(col = "grey77") +
    tm_shape(nss_region_inad_sp) +
    tm_fill(col = {{micronutrient}}, style = "cont", breaks = seq(0,100,by=10),
            palette = (wesanderson::wes_palette("Zissou1Continuous")),
            title = "Prevalence of inadequacy" ,
            legend.is.portrait = FALSE
    ) +
    tm_layout(main.title = {{title}} , frame = F,
              main.title.size = 0.8,
              legend.outside.position = "bottom",
              legend.outside.size = 0.35
    ) +
    # tm_borders(col = "black", lwd = 0.2) +
    tm_shape(ind_state) +
    tm_text("State_Name", size = 0.6, remove.overlap = TRUE)+
    # tm_fill(col = "state") +
    tm_borders(col = "black", lwd = 1.5)+
    tm_legend(show = F)
}


map_list <- function(input_list){
  #takes in a list of column names of MNs and plots inadequacy maps
  output_list <- list()
  
  j <- 1
  for(i in input_list){
    p <- inadequacy_map(i[1],paste(i[2], "- Rice"))
    
    output_list[[j]] <- p
    j <- j+1
  }
  return(output_list)
}

### Inadequacy Maps ############################################################

#create a shapefile at nss_region level
nss_region_inad_sp <- nss_region_inad %>% 
  mutate(nss_region = as.numeric(nss_region)) %>% 
  left_join(nss_region_shapefile, by= 'nss_region') %>% 
  st_as_sf()





base_rice <- list(
  c("folate_inad", "Folate base"),
  c("vitb12_inad", "Vitamin B12 base"),
  c("fe_inad", "Iron base"),
  c("vita_inad", "Vitamin A base"),
  c("thia_inad", "Thiamin base"),
  c("niac_inad", "Niacin base"),
  c("vitb6_inad", "Vitamin B6 base"),
  c("zn_inad", "Zinc base")
)


current_rice <- list(
  c("folate_inad_fort", "Folate current mandatory"),
  c("vitb12_inad_fort", "Vitamin B12 current mandatory"),
  c("fe_inad_fort", "Iron current mandatory"),
  c("vita_inad_fort", "Vitamin A current voluntary"),
  c("thia_inad_fort", "Thiamin current voluntary"),
  c("niac_inad_fort", "Niacin current voluntary"),
  c("vitb6_inad_fort", "Vitamin B6 current voluntary"),
  c("zn_inad_fort", "Zinc current voluntary")
)

improved_rice <- list(
  c("folate_inad_fort_wfp", "Folate improved standard"),
  c("vitb12_inad_fort_wfp", "Vitamin B12 improved standard"),
  c("fe_inad_fort_wfp", "Iron improved standard"),
  c("vita_inad_fort_wfp", "Vitamin A improved standard"),
  c("thia_inad_fort_wfp", "Thiamin improved standard"),
  c("niac_inad_fort_wfp", "Niacin improved standard"),
  c("vitb6_inad_fort_wfp", "Vitamin B6 improved standard"),
  c("zn_inad_fort_wfp", "Zinc improved standard")
)





maps_rice_base <- map_list(base_rice)
maps_rice_current <- map_list(current_rice)
maps_rice_wfp <- map_list(improved_rice)



################################################################################
# reach

calculate_reach

reach_rice  <- level05_30day %>% 
  mutate(consumed_pds_or_free = ifelse(Item_Code %in% c(61,101),1,0),
         consumed_pds = ifelse(Item_Code == 101,1,0),
         consumed_free = ifelse(Item_Code == 61,1,0)) %>% 
  group_by(common_id) %>% 
  summarise(consumed_pds_or_free = ifelse(sum(consumed_pds_or_free)==0,0,1),
            consumed_pds= ifelse(sum(consumed_pds)==0,0,1),
            consumed_free = ifelse(sum(consumed_free)==0,0,1)) %>% 
  left_join(level01 %>% 
              mutate(multiplier = as.numeric(multiplier)), by = "common_id") %>% 
  as_survey_design(ids = common_id, strata = sector, weights = multiplier) %>% 
  srvyr::group_by(nss_region) %>% 
  summarise(
    consumed_pds_or_free =  survey_mean(consumed_pds_or_free == 1, proportion = TRUE)*100,
    consumed_pds =  survey_mean(consumed_pds ==1, proportion = TRUE)*100,
    consumed_free = survey_mean(consumed_free ==1, proportion = TRUE)*100
    
  )


reach_rice_state  <- level05_30day %>% 
  mutate(consumed_pds_or_free = ifelse(Item_Code %in% c(61,101),1,0),
         consumed_pds = ifelse(Item_Code == 101,1,0),
         consumed_free = ifelse(Item_Code == 61,1,0)) %>% 
  group_by(common_id) %>% 
  summarise(consumed_pds_or_free = ifelse(sum(consumed_pds_or_free)==0,0,1),
            consumed_pds= ifelse(sum(consumed_pds)==0,0,1),
            consumed_free = ifelse(sum(consumed_free)==0,0,1)) %>% 
  left_join(level01 %>% 
              mutate(multiplier = as.numeric(multiplier)), by = "common_id") %>% 
  as_survey_design(ids = common_id, strata = sector, weights = multiplier) %>% 
  srvyr::group_by(state) %>% 
  summarise(
    consumed_pds_or_free =  survey_mean(consumed_pds_or_free == 1, proportion = TRUE)*100,
    consumed_pds =  survey_mean(consumed_pds ==1, proportion = TRUE)*100,
    consumed_free = survey_mean(consumed_free ==1, proportion = TRUE)*100
    
  )

# pc by state
intake_rice_state  <- level05_30day %>% 
  filter(Item_Code %in% c(101,61))%>% 
  group_by(common_id) %>% 
  mutate(free = ifelse(Item_Code == 61, Total_Consumption_Quantity, 0),
         pds = ifelse(Item_Code == 101, Total_Consumption_Quantity, 0),
         combind = sum(Total_Consumption_Quantity)) %>% 
  summarise(free = sum(free),
            pds = sum(pds),
            combind = combind) %>% 
  
  slice(1) %>% 
  ungroup() %>% 
  left_join(level01 %>% 
              mutate(multiplier = as.numeric(multiplier)), by = "common_id") %>% 
  as_survey_design(ids = common_id, strata = sector, weights = multiplier) %>% 
  srvyr::group_by(state) %>% 
  summarise(
    
    pc_pds_or_free =  survey_mean(combind,na.rm = T),
    free = survey_mean(free,na.rm = T),
    pds = survey_mean(pds, na.rm = T)
  )

intake_and_reach = inner_join(intake_rice_state,reach_rice_state,by  = "state")


reach_rice_state %>% 
  mutate(state = as.numeric(state)) %>% 
  left_join(ind_admin2 %>% 
              st_drop_geometry() %>% 
              group_by(adm1_code,adm1_name) %>% 
              slice(1) %>% 
              select(adm1_code,adm1_name), by= c('state' = 'adm1_code'))




reach_rice_sp <- reach_rice %>% 
  mutate(nss_region = as.numeric(nss_region)) %>% 
  left_join(nss_region_shapefile, by= 'nss_region') %>% 
  st_as_sf()

tm_shape(ind_state) +
  tm_fill(col = "grey77") +
  tm_shape(reach_rice_sp) +
  tm_fill(col = "consumed_pds_or_free", style = "cont", breaks = seq(0,100,by=10),
          # palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of inadequacy" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "PDS or Free Rice", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
  ) +
  # tm_borders(col = "black", lwd = 0.2) +
  tm_shape() +
  tm_text("State_Name", siind_stateze = 0.8, remove.overlap = TRUE)+
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 2)+
  tm_legend(show = F)


tm_shape(ind_state) +
  tm_fill(col = "grey77") +
  tm_shape(reach_rice_sp) +
  tm_fill(col = "consumed_pds", style = "cont", breaks = seq(0,100,by=10),
          # palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of inadequacy" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "PDS Rice", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
  ) +
  # tm_borders(col = "black", lwd = 0.2) +
  tm_shape(ind_state) +
  tm_text("State_Name", size = 0.8, remove.overlap = TRUE)+
  tm_borders(col = "black", lwd = 2)+
  tm_legend(show = F)

tm_shape(ind_state) +
  tm_fill(col = "grey77") +
  tm_shape(reach_rice_sp) +
  tm_fill(col = "consumed_pds_or_free", style = "cont", breaks = seq(0,100,by=10),
          # palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of inadequacy" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Free Rice", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
  ) +
  # tm_borders(col = "black", lwd = 0.2) +
  tm_shape(ind_state) +
  tm_text("State_Name", size = 0.8, remove.overlap = T)+
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 2)+
  tm_legend(show = F)


