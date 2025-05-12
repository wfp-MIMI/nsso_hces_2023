#########################################
#      HCES 2022-23 INDIA               #
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

################################################################################

path_to_data <- 'data/processed/'
path_to_save <- 'C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Workstream 5/database_planning/PPG-N analytics team/processed_data/'

# read the data in

ind_nss2223_base_ai <- readRDS(paste0(path_to_data, "ind_nss2223_base_case.rds"))



# food consumption 
ind_nss2223_food_consumption <- readRDS(paste0(path_to_data, "ind_nss2223_food_consumption.rds"))

ind_nss2223_food_consumption <- ind_nss2223_food_consumption %>% 
  select(common_id, Item_Code, Total_Consumption_Quantity) %>% 
  rename(hhid = common_id,
         item_code = Item_Code,
         quantity_g = Total_Consumption_Quantity) %>% 
  mutate(quantity_100g = quantity_g/100,
         item_code = as.character(item_code),
         iso3 = "IND",
         survey = 'nss2223'
         ) %>% 
  select(iso3,survey,hhid,item_code,quantity_g, quantity_100g)



# fct 
ind_202223_fct <-  read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/nsso_202223_fct.xlsx")

ind_nss2223_fct <-  ind_202223_fct%>% 
  select(item_code, item_name,
         ends_with(c("_mg","kcal", "mcg", "ug", "_g"))) %>% 
  mutate(item_code = as.character(item_code),
         iso3 = 'IND',
         survey = 'nss2223',
         zone = NA_character_) %>% 
  rename(vita_rae_mcg = vita_mcg,
         thia_mg = vitb1_mg,
         ribo_mg = vitb2_mg,
         niac_mg = vitb3_mg,
         vitb6_mg = vitb6_mg,
         vitb7_mg = vitb7_ug,
         folate_mcg = folate_ug,
         vitb12_mcg = vitaminb12_in_mcg,
         fe_mg = iron_mg,
         zn_mg = zinc_mg,
         ca_mg = calcium_mg) %>% 
  select(item_code,
         iso3,
         survey,
         zone,
         item_name,
         energy_kcal,
         vita_rae_mcg,
         thia_mg,
         ribo_mg,
         niac_mg,
         vitb6_mg,
         vitb7_mg,
         folate_mcg,
         vitb12_mcg,
         fe_mg,
         zn_mg,
         ca_mg,
         protein_g,
         fat_g,
         carb_g,
         vitc_mg
         )
  

# hh_info


nss_raw_level1 <- haven::read_dta("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/level01.dta")


nss_raw_level1 <- nss_raw_level1 %>% 
  select(common_id, year, fsu_serial_no, state, district, multiplier)



ind_nss2223_afe <- readRDS(paste0(path_to_data, "ind_nss2223_afe.rds"))

ind_nss2223_hh_expenditure <- readRDS(paste0(path_to_data, "ind_nss2223_hh_expenditure.rds"))

ind_nss2223_hh_info <- ind_nss2223_hh_expenditure %>% 
  left_join(nss_raw_level1, by = 'common_id') %>% 
  left_join(ind_nss2223_afe, by = 'common_id') %>% 
  mutate(survey = 'nss2223',
         iso3 = "IND",
         zone = NA_character_,
         month = NA_integer_,
         res = factor(ifelse(sector == 1, "Rural", "Urban")),
         sep_quintile = as.integer(sep_quintile),
         res_quintile = as.integer(res_quintile),
         survey_wgt = as.numeric(multiplier)) %>% 
  rename(hhid = common_id,
         adm1 = state,
         adm2 = district,
         ea = fsu_serial_no,
         pc_expenditure = per_capita_expenditure,
        ) %>% 
  select(survey, 
         hhid,
         iso3,
         zone,
         adm1,
         adm2,
         ea,
         res,
         sep_quintile,
         res_quintile,
         year,
         month,
         survey_wgt,
         afe,
         pc_expenditure) %>% 
  filter( hhid %in% ind_nss2223_base_ai$common_id)

rm(ind_nss2223_afe, nss_raw_level1)

# food groups


ind_nss_mddw <- read_xlsx("data/raw/ind_nss2223_hdds.xlsx",sheet = 2)

ind_nss_mddw <- ind_nss_mddw%>% 
  tidyr::pivot_longer(cols = -c(item_code, item_name)) %>% 
  filter(value == 1) %>% 
  rename(food_group = name) %>% 
  mutate(iso3 = "IND",
         survey = "nss2223") %>% 
  select(item_code, iso3, survey,food_group)




# vehilce qunatites
rice = ind_nss2223_food_consumption %>% 
  filter(item_code %in% c(61,101,102)) %>% 
  group_by(iso3, survey,hhid) %>% 
  summarise(quantity_100g = sum(quantity_100g)) %>% 
  mutate(vehicle = 'rice') %>% 
  ungroup()

wheatflour = ind_nss2223_food_consumption %>% 
  filter(item_code %in% c(62,107,108)) %>% 
  group_by(iso3, survey,hhid) %>% 
  summarise(quantity_100g = sum(quantity_100g)) %>% 
  mutate(vehicle = 'wheatflour')%>% 
  ungroup()

salt = ind_nss2223_food_consumption %>% 
  filter(item_code %in% c(73,170,178)) %>% 
  group_by(iso3, survey,hhid) %>% 
  summarise(quantity_100g = sum(quantity_100g)) %>% 
  mutate(vehicle = 'salt')%>% 
  ungroup()

ind_nss2223_vehicle_quantities <- bind_rows(rice,wheatflour,salt)


################################################################################

write_csv(ind_nss2223_base_ai, paste0(path_to_save, "ind_nss2223_base_ai.csv"))
write_csv(ind_nss2223_food_consumption, paste0(path_to_save, "ind_nss2223_food_consumption.csv"))
write_csv(ind_nss2223_fct, paste0(path_to_save, "ind_nss2223_fct.csv"))
write_csv(ind_nss2223_hh_info, paste0(path_to_save, "ind_nss2223_hh_info.csv"))
write_csv(ind_nss2223_vehicle_quantities, paste0(path_to_save, "ind_nss2223_hh_info.csv"))

#


       