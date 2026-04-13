# Last updated: 10 March 2026
rm(list = ls())
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

path_to_data <- 'data/processed/'
path_to_save <- 'C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Workstream 5/database_planning/PPG-N analytics team/processed_data/'

# read the data in
ind_202223_fct <- readxl::read_xlsx("data/raw/nsso_202223_fct.xlsx")
ind_nss2223_food_consumption <- readRDS(paste0(path_to_data, "ind_nss2223_food_consumption.rds"))

hh_mn_intake_lucia <- ind_nss2223_food_consumption %>% 
    inner_join(ind_202223_fct , by=c("Item_Code" ="item_code" )) %>%
    mutate(quantity_100g = Total_Consumption_Quantity/100,
           across(c(energy_kcal,folate_ug,iron_mg, zinc_mg ),
                  ~.x*quantity_100g)
    ) %>%
    rename(hhid = common_id, code = Item_Code,folate_mcg = folate_ug, fe_mg = iron_mg, zn_mg = zinc_mg) %>% 
    select(c(hhid, code, item_name,quantity_100g, energy_kcal,folate_mcg,fe_mg, zn_mg))
    
write.csv(hh_mn_intake_lucia, "nsso_subset_lucia.csv")
