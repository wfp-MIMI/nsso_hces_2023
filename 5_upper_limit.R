#########################################
#      HCES 2022-23 INDIA               #
#          inadequacy maps              #
#########################################


# Author: Gabriel Battcock
# Created: 
# Last updated: 09 May 2025

# package loading

rm(list = ls())

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
# data loading
# rm(list = ls())

# set paths
figure_path <- "figures/"
raw_path <- "data/raw/"
processed_path <- "data/processed/"

# read data

food_consumption_daily_afe <- readRDS(paste0(processed_path,"ind_nss2223_food_consumption.rds"))
hh_mn_intake <- readRDS(paste0(processed_path,"ind_nss2223_base_case.rds"))



# ind_admin2 <- st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Nutrition analysis/shapefiles/ind_lss1819_adm2.shp")

# nss_region_inad_rice <- readRDS("nss_region_inad_rice.rds")
# nss_region_inad_wf <- readRDS("nss_region_inad_wf.rds" )
food_consumption_daily_afe <- readRDS(paste0(processed_path,"ind_nss2223_food_consumption.rds"))

# readRDS("ind_fort_wf.rds")
hh_mn_intake_fort_wf <-  readRDS("ind_fort_wf_v2.rds")
hh_mn_intake_fort_rice <- readRDS( "ind_fort_rice.rds")
hh_mn_intake_fort_rice_comm <-   readRDS("ind_fort_rice_com.rds")
hh_mn_intake_fort_wf_comm <-  readRDS("ind_fort_wf_com.rds")
all_vehicles <- readRDS("ind_fort_all.rds")

################################################################################
# load upp limit
nin_ear <- data.frame(
  nutrient = c("energy_kcal",
               "vita_rae_mcg",
               "thia_mg",
               "ribo_mg",
               "niac_mg",
               "vitb6_mg",
               "folate_mcg",
               "vitb12_mcg",
               "fe_mg",
               "ca_mg",
               "zn_mg"
  ),
  ear_value = c(
    2130,
    390,
    1.4,
    2.0,
    12,
    1.6,
    180,
    2,
    15,
    800,
    11
  ),
  tul_value = c(
    NA,
    3000,
    NA,
    NA,
    35,
    100,
    1000,
    NA,
    45,
    2500,
    40
  )
)

# Step 1: look how many households are above UL before fortification 
base_excess_iron<- hh_mn_intake_fort_wf %>% 
  filter(iron_mg>45)

# look at overall energy distribution of population 
hh_mn_intake_fort_wf %>% 
  ggplot(aes(x = energy_kcal))+
  geom_histogram()

# calculate the 
hh_mn_intake_fort_rice %>% 
  summarise(
    mean = mean(energy_kcal),
    sd = sd(energy_kcal),
    plus_2sd = mean +2*sd,
    minus_2sd = mean -1*sd
  )

# excess energy
excess_energy <- hh_mn_intake_fort_wf %>% 
  filter(energy_kcal>3120)

# look at distribution of vehicle intake 

all_vehicles %>% 
  mutate(total_vehicle_quantity = wf_pds_quantity+wf_comm_quantity+rice_comm_quantity +rice_pds_quantity) %>% 
  summarise(mean = median(total_vehicle_quantity),
            sd =sd(total_vehicle_quantity),
            plus_2sd = mean+1.96*sd)

# excess vehicle consumption
excess_vehicle <- all_vehicles %>% 
  mutate(total_vehicle_quantity = wf_pds_quantity+wf_comm_quantity+rice_comm_quantity +rice_pds_quantity) %>% 
  filter(total_vehicle_quantity>453)



## choose which to exclude
# exclude <- excess_energy


# using the goldber cut off


hh_mn_intake_fort_wf <- hh_mn_intake_fort_wf %>% 
  mutate(
    BMR = 14.818*55 +  486.6,
    EI_BMR = energy_kcal/BMR
    )

pal <- 1.55
SD_TEE <- 0.2

goldberg_lower <- pal* exp(-2*SD_TEE)
goldberg_upper <- pal* exp(2*SD_TEE)

excess_energy <- hh_mn_intake_fort_wf %>% 
  mutate(
    classification = case_when(
      EI_BMR < goldberg_lower ~ "Under-reporter",
      EI_BMR > goldberg_upper ~ "Over-reporter",
      TRUE ~ "Plausible"
    )
  ) %>% 
  filter(classification == "Over-reporter")


exclude <-  excess_vehicle %>% bind_rows(excess_energy) %>%
  distinct(common_id)

min(excess_energy$energy_kcal)


hh_mn_intake_fort_rice %>% 
  filter(iron_mg>45 &
           !(common_id %in%exclude$common_id) )  %>% 
  summarise(
    n(),
    mean_energy = mean(energy_kcal),
    median_energy = median(energy_kcal),
    energy_25 = quantile(energy_kcal, 0.25),
    energy_75 = quantile(energy_kcal,0.75),
    mean_quantity = mean(Total_Consumption_Quantity),
    median_quantity = median(Total_Consumption_Quantity),
    quantity_25 = quantile(Total_Consumption_Quantity, 0.25),
    quantity_75 = quantile(Total_Consumption_Quantity,0.75)
    
  )


hh_mn_intake_fort_rice %>% 
  filter(fe_mg_fort>45  &
           !(common_id %in%exclude$common_id))   %>% 
  summarise(
    n(),
    mean_energy = mean(energy_kcal),
    median_energy = median(energy_kcal),
    energy_25 = quantile(energy_kcal, 0.25),
    energy_75 = quantile(energy_kcal,0.75),
    mean_quantity = mean(Total_Consumption_Quantity),
    median_quantity = median(Total_Consumption_Quantity),
    quantity_25 = quantile(Total_Consumption_Quantity, 0.25),
    quantity_75 = quantile(Total_Consumption_Quantity,0.75),
    mean_iron = mean(fe_mg_fort)        
    )
  

x <- food_consumption_daily_afe %>% filter(common_id %in% rice$common_id)

hh_mn_intake_fort_rice %>% 
  filter(fe_mg_fort_wfp>45 &
           !(common_id %in%exclude$common_id))   %>% 
  summarise(
    n(),
    mean_energy = mean(energy_kcal),
    median_energy = median(energy_kcal),
    energy_25 = quantile(energy_kcal, 0.25),
    energy_75 = quantile(energy_kcal,0.75),
    mean_quantity = mean(Total_Consumption_Quantity),
    median = median(Total_Consumption_Quantity),
    quantity_25 = quantile(Total_Consumption_Quantity, 0.25),
    quantity_75 = quantile(Total_Consumption_Quantity,0.75),
    mean_iron = mean(fe_mg_fort_wfp)   
    
  
  )


hh_mn_intake_fort_wf %>% 
  filter(fe_mg_fort>45 &
           !(common_id %in%exclude$common_id))   %>% 
  summarise(
    n(),
    mean_energy = mean(energy_kcal),
    median_energy = median(energy_kcal),
    energy_25 = quantile(energy_kcal, 0.25),
    energy_75 = quantile(energy_kcal,0.75),
    mean_quantity = mean(Total_Consumption_Quantity.y+Total_Consumption_Quantity.x),
    median_quantity = median(Total_Consumption_Quantity.y+Total_Consumption_Quantity.x),
    quantity_25 = quantile(Total_Consumption_Quantity.y+Total_Consumption_Quantity.x, 0.25),
    quantity_75 = quantile(Total_Consumption_Quantity.y+Total_Consumption_Quantity.x,0.75),
    mean_iron = mean(fe_mg_fort)   
    
  )


hh_mn_intake_fort_wf %>% 
  filter(fe_mg_fort_wfp>45 &
           !(common_id %in%exclude$common_id))   %>% 
  summarise(
    n(),
    mean_energy = mean(energy_kcal),
    median_energy = median(energy_kcal),
    energy_25 = quantile(energy_kcal, 0.25),
    energy_75 = quantile(energy_kcal,0.75),
    mean_quantity = mean(Total_Consumption_Quantity.y+Total_Consumption_Quantity.x),
    median_quantity = median(Total_Consumption_Quantity.y+Total_Consumption_Quantity.x),
    quantity_25 = quantile(Total_Consumption_Quantity.y+Total_Consumption_Quantity.x, 0.25),
    quantity_75 = quantile(Total_Consumption_Quantity.y+Total_Consumption_Quantity.x,0.75),
    mean_iron = mean(fe_mg_fort_wfp)   
    
  )

hh_mn_intake_fort_rice_comm %>% 
  filter(fe_mg_fort>45 &
           !(common_id %in%exclude$common_id))   %>% 
  summarise(
    n(),
    mean_energy = mean(energy_kcal),
    median_energy = median(energy_kcal),
    energy_25 = quantile(energy_kcal, 0.25),
    energy_75 = quantile(energy_kcal,0.75),
    mean_quantity = mean(Total_Consumption_Quantity+Purchased_Quantity),
    median_quantity = median(Total_Consumption_Quantity+Purchased_Quantity),
    quantity_25 = quantile(Total_Consumption_Quantity+Purchased_Quantity, 0.25),
    quantity_75 = quantile(Total_Consumption_Quantity+Purchased_Quantity,0.75),
    mean_iron = mean(fe_mg_fort)   
    
  )


hh_mn_intake_fort_rice_comm %>% 
  filter(fe_mg_fort_wfp>45 &
           !(common_id %in%exclude$common_id))   %>% 
  summarise(
    n(),
    mean_energy = mean(energy_kcal),
    median_energy = median(energy_kcal),
    energy_25 = quantile(energy_kcal, 0.25),
    energy_75 = quantile(energy_kcal,0.75),
    mean_quantity = mean(Total_Consumption_Quantity+Purchased_Quantity),
    median_quantity = median(Total_Consumption_Quantity+Purchased_Quantity),
    quantity_25 = quantile(Total_Consumption_Quantity+Purchased_Quantity, 0.25),
    quantity_75 = quantile(Total_Consumption_Quantity+Purchased_Quantity,0.75),
    mean_iron = mean(fe_mg_fort_wfp)   
    
  )


hh_mn_intake_fort_wf_comm %>% 
  filter(fe_mg_fort>45 &
           !(common_id %in% exclude$common_id))   %>% 
  summarise(
    n(),
    mean_energy = mean(energy_kcal),
    median_energy = median(energy_kcal),
    energy_25 = quantile(energy_kcal, 0.25),
    energy_75 = quantile(energy_kcal,0.75),
    mean_quantity = mean(Total_Consumption_Quantity+Purchased_Quantity),
    median_quantity = median(Total_Consumption_Quantity+Purchased_Quantity),
    quantity_25 = quantile(Total_Consumption_Quantity+Purchased_Quantity, 0.25),
    quantity_75 = quantile(Total_Consumption_Quantity+Purchased_Quantity,0.75),
    mean_iron = mean(fe_mg_fort)   
    
  )


hh_mn_intake_fort_wf_comm %>% 
  filter(fe_mg_fort_wfp>45 &
         !(common_id %in% exclude$common_id))   %>% 
  summarise(
    n(),
    mean_energy = mean(energy_kcal),
    median_energy = median(energy_kcal),
    energy_25 = quantile(energy_kcal, 0.25),
    energy_75 = quantile(energy_kcal,0.75),
    mean_quantity = mean(Total_Consumption_Quantity+Purchased_Quantity),
    median_quantity = median(Total_Consumption_Quantity+Purchased_Quantity),
    quantity_25 = quantile(Total_Consumption_Quantity+Purchased_Quantity, 0.25),
    quantity_75 = quantile(Total_Consumption_Quantity+Purchased_Quantity,0.75),
    mean_iron = mean(fe_mg_fort_wfp)   
    
  )


### ALL VEHICLE CASE ###

all_vehicles %>% 
  filter(fe_mg_fort_wfp>45 &
           !(common_id %in%exclude$common_id))   %>% 
  summarise(
    n(),
    mean_energy = mean(energy_kcal),
    median_energy = median(energy_kcal),
    energy_25 = quantile(energy_kcal, 0.25),
    energy_75 = quantile(energy_kcal,0.75),
    mean_quantity = mean(wf_pds_quantity+wf_comm_quantity+rice_comm_quantity +rice_pds_quantity ),
    median_quantity = median(wf_pds_quantity+wf_comm_quantity+rice_comm_quantity +rice_pds_quantity),
    quantity_25 = quantile(wf_pds_quantity+wf_comm_quantity+rice_comm_quantity +rice_pds_quantity, 0.25),
    quantity_75 = quantile(wf_pds_quantity+wf_comm_quantity+rice_comm_quantity+rice_pds_quantity,0.75),
    mean_iron = mean(fe_mg_fort_wfp)   
  )

all_vehicles %>% 
  filter(fe_mg_fort>45 &
           !(common_id %in%exclude$common_id))   %>% 
  summarise(
    n(),
    mean_energy = mean(energy_kcal),
    median_energy = median(energy_kcal),
    energy_25 = quantile(energy_kcal, 0.25),
    energy_75 = quantile(energy_kcal,0.75),
    mean_quantity = mean(wf_pds_quantity+wf_comm_quantity+rice_comm_quantity +rice_pds_quantity ),
    median_quantity = median(wf_pds_quantity+wf_comm_quantity+rice_comm_quantity +rice_pds_quantity),
    quantity_25 = quantile(wf_pds_quantity+wf_comm_quantity+rice_comm_quantity +rice_pds_quantity, 0.25),
    quantity_75 = quantile(wf_pds_quantity+wf_comm_quantity+rice_comm_quantity+rice_pds_quantity,0.75),
    mean_iron = mean(fe_mg_fort_wfp)   
  )



# look at 


all_vehicles %>% 
  filter(fe_mg_fort_wfp>45 &
           !(common_id %in%exclude$common_id))   %>% 
  summarise(
    n(),
    
    mean_energy = mean(energy_kcal),
    median_energy = median(energy_kcal),
    energy_25 = quantile(energy_kcal, 0.25),
    energy_75 = quantile(energy_kcal,0.75),
    mean_quantity = mean(wf_pds_quantity+wf_comm_quantity+rice_comm_quantity +rice_pds_quantity ),
    median_quantity = median(wf_pds_quantity+wf_comm_quantity+rice_comm_quantity +rice_pds_quantity),
    quantity_25 = quantile(wf_pds_quantity+wf_comm_quantity+rice_comm_quantity +rice_pds_quantity, 0.25),
    quantity_75 = quantile(wf_pds_quantity+wf_comm_quantity+rice_comm_quantity+rice_pds_quantity,0.75),
    mean_iron = mean(fe_mg_fort_wfp)   
  )


all_vehicles %>% 
  filter(fe_mg_fort_wfp>45)   %>% 
  summarise(
    n()/261082)

all_vehicles %>% 
  filter(fe_mg_fort_wfp<29)   %>% 
  summarise(
    n()/261082)

all_vehicles %>% 
  filter(fe_mg_fort_wfp<15)   %>% 
  summarise(
    n()/261082)

## plots

all_vehicles %>% 
  filter(!(common_id %in%exclude$common_id)) %>% 
  ggplot(aes(x = fe_mg_fort))+
  geom_histogram()+ 
  geom_vline(xintercept = 15, color = 'red')+
  geom_vline(xintercept = 29, color = 'blue')+
  geom_vline(xintercept = 45, color = 'red')+
  xlim(0,75)+
  ylim(0,60000)

all_vehicles %>% 
  filter(!(common_id %in%exclude$common_id)) %>% 
  ggplot(aes(x = fe_mg_fort_wfp))+
  geom_histogram()+ 
  geom_vline(xintercept = 15, color = 'red')+
  geom_vline(xintercept = 29, color = 'blue')+
  geom_vline(xintercept = 45, color = 'red')+
  xlim(0,75)+
  ylim(0,60000)

hh_mn_intake_fort_rice %>% 
  filter(!(common_id %in%exclude$common_id)) %>% 
  ggplot(aes(x = iron_mg))+
  geom_histogram()+ 
  geom_vline(xintercept = 15, color = 'red')+
  geom_vline(xintercept = 29, color = 'blue')+
  geom_vline(xintercept = 45, color = 'red')+
  xlim(0,75)+
  ylim(0,60000)

hh_mn_intake_fort_rice %>% summarise(n())
