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
                 "wesanderson","treemap","treemapify", 
                "biscale")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))


# sorce iron full probability functions

source(here::here("../MIMI1_archive/universal_functions/iron_full_probability/src/iron_inad_prev.R"))

################################################################################

# rm(list = ls())

# set paths
figure_path <- "figures/"
raw_path <- "data/raw/"
processed_path <- "data/processed/"

# read data

food_consumption_daily_afe <- readRDS("ind_nss2223_food_consumption.rds")
hh_mn_intake <- readRDS("ind_nss2223_base_case.rds")
# read in the fct
ind_202223_fct <-  read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/nsso_202223_fct.xlsx")


ind_state <- sf::st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp")
# ind_admin2 <- st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Nutrition analysis/shapefiles/ind_lss1819_adm2.shp")
nss_region_shapefile <- sf::st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/ind_nss2223_nssregion.shp")

nss_region_inad_rice <- readRDS("nss_region_inad_rice.rds")
nss_region_inad_wf <- readRDS("nss_region_inad_wf.rds" )
food_consumption_daily_afe <- readRDS("ind_nss2223_food_consumption.rds")


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
    # tm_text("State_Name", size = 0.6, remove.overlap = TRUE)+
    # tm_fill(col = "state") +
    tm_borders(col = "black", lwd = 1.5)+
    tm_legend(show = F)
}


inadequacy_map()

map_list <- function(input_list){
  #takes in a list of column names of MNs and plots inadequacy maps
  output_list <- list()
  
  
  for(i in input_list){
    p <- inadequacy_map(i[1],paste(i[2], "- "))
    
    output_list[[i[1]]] <- p
    
  }
  return(output_list)
}

### Inadequacy Maps ############################################################

#create a shapefile at nss_region level
nss_region_inad_sp <- nss_region_inad_rice %>% 
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

base_rice[[2]][1]
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


for(i in 1:8){
  map <- maps_rice_base[[i]]
  tmap_save(map, paste0(figure_path,"base/", base_rice[[i]][1] , ".png"),
         width = 8, height = 9, units = "in")

}


for(i in 1:8){
  map <- maps_rice_current[[i]]
  tmap_save(map, paste0(figure_path,"current_rice/", current_rice[[i]][1] , ".png"),
            width = 8, height = 9, units = "in")
  
}

for(i in 1:8){
  map <- maps_rice_wfp[[i]]
  tmap_save(map, paste0(figure_path,"improved_rice/", improved_rice[[i]][1] , ".png"),
            width = 8, height = 9, units = "in")
  
}



## wheat flour 


#create a shapefile at nss_region level
nss_region_inad_sp <-  nss_region_inad_wf %>% 
  mutate(nss_region = as.numeric(nss_region)) %>% 
  left_join(nss_region_shapefile, by= 'nss_region') %>% 
  st_as_sf()



current_wf <- list(
  c("folate_inad_fort", "Folate current mandatory"),
  c("vitb12_inad_fort", "Vitamin B12 current mandatory"),
  c("fe_inad_fort", "Iron current mandatory"),
  c("vita_inad_fort", "Vitamin A current voluntary"),
  c("thia_inad_fort", "Thiamin current voluntary"),
  c("niac_inad_fort", "Niacin current voluntary"),
  c("vitb6_inad_fort", "Vitamin B6 current voluntary"),
  c("zn_inad_fort", "Zinc current voluntary")
)

improved_wf <- list(
  c("folate_inad_fort_wfp", "Folate improved standard"),
  c("vitb12_inad_fort_wfp", "Vitamin B12 improved standard"),
  c("fe_inad_fort_wfp", "Iron improved standard"),
  c("vita_inad_fort_wfp", "Vitamin A improved standard"),
  c("thia_inad_fort_wfp", "Thiamin improved standard"),
  c("niac_inad_fort_wfp", "Niacin improved standard"),
  c("vitb6_inad_fort_wfp", "Vitamin B6 improved standard"),
  c("zn_inad_fort_wfp", "Zinc improved standard")
)






maps_wf_current <- map_list(current_wf)
maps_wf_wfp <- map_list(improved_wf)





for(i in 1:8){
  map <- maps_wf_current[[i]]
  tmap_save(map, paste0(figure_path,"current_wf/", current_wf[[i]][1] , ".png"),
            width = 8, height = 9, units = "in")
  
}

for(i in 1:8){
  map <- maps_wf_wfp[[i]]
  tmap_save(map, paste0(figure_path,"improved_wf/", improved_wf[[i]][1] , ".png"),
            width = 8, height = 9, units = "in")
  
}

################################################################################
# reach/coverage of vehciles

### RICE

# calucalte the reach of rice
reach_rice  <- food_consumption_daily_afe %>% 
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

# calcaulte the per-capita consumotion
intake_rice  <- food_consumption_daily_afe %>% 
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
  srvyr::group_by(nss_region) %>% 
  summarise(
    
    pc_pds_or_free =  survey_mean(combind,na.rm = T),
    free = survey_mean(free,na.rm = T),
    pds = survey_mean(pds, na.rm = T)
  ) 


# have a joined df of reach and pc-consumption
reach_intake <- 
  reach_rice %>% 
  left_join(intake_rice) %>% 
    mutate(
      across(
        everything(),
        ~ifelse(is.na(.),0,.)
      )
    ) %>% 
  mutate(
    reach_bins = cut(
      # for bivariate maps, you need to break into classes
      # I have chosen quartiles for reach
      consumed_pds_or_free, breaks = c(0,25,50,75,100),include.lowest = T
    ),
    # For pc-consumption I spit into 0-50, 50-100, 100-150, and 150-300 g/afe/day
    intake_bins = cut(pc_pds_or_free, breaks = c(0,50,100,150, 300), include.lowest = T)
  ) %>% 
    select(nss_region,pc_pds_or_free, reach_bins, intake_bins) %>% 
  mutate(nss_region = as.numeric(nss_region)) %>% 
  left_join(nss_region_shapefile, by= 'nss_region') %>% 
  st_as_sf() 


##### Bi-variate mapping #######

# create a bi classs
data_rice <- bi_class(reach_intake, x =reach_bins , y = intake_bins, dim = 4 )


# using ggplot and bi_scale, create a bivariate map
bi_map_rice <- ggplot() + 
  geom_sf(data = data_rice, mapping = aes(fill = bi_class), color = NA,show.legend = F)+
  bi_scale_fill(pal = "DkBlue2",dim = 4)+
  bi_theme()+
  geom_sf(data = ind_state, fill= NA, color = 'black', lwd = 1) + 
  labs(subtitle = "PDS Rice - coverage and consumption", )

# create a df of the breaks for each exis
break_vals <- bi_class_breaks(reach_intake, x =reach_bins , y = intake_bins, dim = 4 )

#create a bivariate legend
legend_rice <- bi_legend(pal = "DkBlue2",
                    dim = 4,
                    xlab = "Higher Reach (%) ",
                    ylab = "Higher Consumption (g) ",
                    size = 8, 
                    breaks = break_vals)

# put legend and map together
rice_bivariate <- ggdraw() +
  draw_plot(bi_map_rice, 0, 0, 1, 1) +
  draw_plot(legend_rice, 0.65, .2, 0.2, 0.2)

rice_bivariate


# WHEAT FLOUR

# create a df of reach of wf
reach_wf  <- food_consumption_daily_afe %>% 
  mutate(consumed_pds_or_free = ifelse(Item_Code %in% c(62,107),1,0),
         consumed_pds = ifelse(Item_Code == 107,1,0),
         consumed_free = ifelse(Item_Code == 62,1,0)) %>% 
  group_by(common_id) %>% 
  summarise(consumed_pds_or_free = ifelse(sum(consumed_pds_or_free)==0,0,1),
            consumed_pds= ifelse(sum(consumed_pds)==0,0,1),
            consumed_free = ifelse(sum(consumed_free)==0,0,1)) %>% 
  ungroup() %>% 
  left_join(level01 %>% 
              mutate(multiplier = as.numeric(multiplier)), by = "common_id") %>% 
  as_survey_design(ids = common_id, strata = sector, weights = multiplier) %>% 
  srvyr::group_by(nss_region) %>% 
  summarise(
    consumed_pds_or_free =  survey_mean(consumed_pds_or_free == 1, proportion = TRUE)*100,
    consumed_pds =  survey_mean(consumed_pds ==1, proportion = TRUE)*100,
    consumed_free = survey_mean(consumed_free ==1, proportion = TRUE)*100
    
  )

#calculate pc-consumtion of wf
intake_wf  <- food_consumption_daily_afe %>% 
  filter(Item_Code %in% c(107,62))%>% 
  group_by(common_id) %>% 
  mutate(free = ifelse(Item_Code == 62, Total_Consumption_Quantity, 0),
         pds = ifelse(Item_Code == 107, Total_Consumption_Quantity, 0),
         combind = sum(Total_Consumption_Quantity)) %>% 
  summarise(free = sum(free),
            pds = sum(pds),
            combind = combind) %>% 
  
  slice(1) %>% 
  ungroup() %>% 
  left_join(level01 %>% 
              mutate(multiplier = as.numeric(multiplier)), by = "common_id") %>% 
  as_survey_design(ids = common_id, strata = sector, weights = multiplier) %>% 
  srvyr::group_by(nss_region) %>% 
  summarise(
    
    pc_pds_or_free =  survey_mean(combind,na.rm = T),
    free = survey_mean(free,na.rm = T),
    pds = survey_mean(pds, na.rm = T)
  )


# join reach and pc-consumtption
reach_intake_wf <- reach_wf %>% 
  left_join(intake_wf) %>% 
  mutate(
    across(
      everything(),
      ~ifelse(is.na(.),0,.)
    )
  ) %>% 
  mutate(
    reach_bins = cut(
      consumed_pds_or_free, breaks = c(0,25,50,75,100),include.lowest = T
    ),
    intake_bins = cut(pc_pds_or_free, breaks = c(0,50,100,150, 300), include.lowest = T)
  ) %>% 
  select(nss_region,pc_pds_or_free, reach_bins, intake_bins) %>% 
  mutate(nss_region = as.numeric(nss_region)) %>% 
  left_join(nss_region_shapefile, by= 'nss_region') %>% 
  st_as_sf() 


#### BIVARIATE MAP #####

### Create bi-class
data_wf <- bi_class(reach_intake_wf, x =reach_bins , y = intake_bins, dim = 4 )

# plot the map
bi_map_wf <- ggplot() + 
  geom_sf(data = data_wf, mapping = aes(fill = bi_class), color = NA,show.legend = F)+
  bi_scale_fill(pal = "DkBlue2",dim = 4)+
  bi_theme()+
  geom_sf(data = ind_state, fill= NA, color = 'black', lwd = 1)+
  labs(subtitle = "PDS Wheat - coverage and consumption", )

# new legend
legend_wf <- bi_legend(pal = "DkBlue2",
                         dim = 4,
                       xlab = "Higher Coverage (%) ",
                       ylab = "Higher Consumption (g) ",
                       size = 8, 
                       breaks = break_vals)


wf_bivariate <- ggdraw() +
  draw_plot(bi_map_wf, 0, 0, 1, 1) +
  draw_plot(legend_wf, 0.65, .2, 0.2, 0.2)

wf_bivariate


# save the bivaraite maps


ggsave(paste0(figure_path,"bi_legend.png"), plot= legend_wf,   width = 2, height = 2, units = 'in')

ggsave(paste0(figure_path,"bimap_wf.png"),bi_map_wf, width = 8, height= 9, units = 'in')

ggsave(paste0(figure_path,"bimap_rice.png"),bi_map_rice, width = 8, height= 9, units = 'in')


#####################################################################################################
# calculating reach and pc-consumption at different levels



reach_rice_state  <-food_consumption_daily_afe %>% 
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






wf_rice_state  <-food_consumption_daily_afe %>% 
  mutate(consumed_pds_or_free = ifelse(Item_Code %in% c(62,107),1,0),
         consumed_pds = ifelse(Item_Code == 107,1,0),
         consumed_free = ifelse(Item_Code == 62,1,0)) %>% 
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
intake_rice_state  <- food_consumption_daily_afe %>% 
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

rice_reach_map <- tm_shape(ind_state) +
  tm_fill(col = "grey77") +
  tm_shape(reach_intake) +
  tm_fill(col = "pc_pds_or_free", style = "cont",
          # breaks = seq(0,100,by=10),
          # palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Reach of PDS and free rice" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "PDS or Free Rice", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
  ) +
  # tm_borders(col = "black", lwd = 0) +
  tm_shape(ind_state) +
  tm_text("State_Name", size = 0.8, remove.overlap = TRUE)+
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 2)+
  tm_legend(show = F)


tmap_save(rice_reach_map, paste0(figure_path,"rice_reach.png"), width = 8, height = 9, units = "in")

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


