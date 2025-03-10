#########################################
#      HCES 2022-23 INDIA    
#          fortification                #
#########################################


# Author: Gabriel Battcock
# Created: 
# Last updated: 26 Feb 2025

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
# set paths
figure_path <- "figures/"
raw_path <- "data/raw/"
processed_path <- "data/processed/"



file_list = list.files("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/")
# haven::read_dta("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/level06.dta")
data_list <- lapply(paste0("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/",file_list), haven::read_dta)
names(data_list) <- tools::file_path_sans_ext(file_list)
level01 <- data_list$level01


food_consumption_daily_afe <- readRDS(paste0("ind_nss2223_food_consumption.rds"))
hh_mn_intake <- readRDS(paste0("ind_nss2223_base_case.rds"))
hh_expenditure <- readRDS(paste0("ind_nss2223_hh_expenditure.rds"))
# read in the fct
ind_202223_fct <-  read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/nsso_202223_fct.xlsx")

# IFPRI data

gdqs_nss <- read_dta("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/India/MIMI-IFPRI GDQS Collaboration/GDQS_data_IFPRI.dta")



################################################################################
# set ear cut point and UL values
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



# ------------------------------------------------------------------------------
# create a new id that is unique
gdqs_nss <- gdqs_nss %>% 
  mutate(new_id = 
           paste0(fsu_sno,sector,state,nss_region,district,sec_stage_strat,
                  # stratum,
                  # substratum,
                 common_id))
unique(gdqs_nss$new_id)



#create unique id that matches with IFPRI data frame 
level01_new <- level01 %>% 
  mutate(
    across(c(fsu_serial_no,sector, state ,nss_region,district,sample_stage_stratum_no,sample_hhld_no),
    as.numeric
  )) %>% 
  mutate(new_id = 
           paste0(
             fsu_serial_no,sector,state,nss_region,district,sample_stage_stratum_no,sample_hhld_no
           )) %>% 
  select(new_id, common_id)

unique(level01_new$new_id)

# check that they all match 
sum(gdqs_nss$new_id %in% level01_new$new_id == TRUE)
sum(level01_new$new_id %in% gdqs_nss$new_id == TRUE)


# rename some columns 
hh_mn_intake <- hh_mn_intake %>% 
  rename(
    vitb12_mcg = vitaminb12_in_mcg, 
    vita_rae_mcg = vita_mcg,
    folate_mcg = folate_ug
  )

  
#create final dataframe


final_gdqs_mimi <- gdqs_nss %>% 
  #filter out common id (sample hh no) so merge will work
  select(-common_id) %>% 
  inner_join(level01_new, by = "new_id") %>% 
  left_join(hh_mn_intake, by = "common_id") %>% 
  select(-common_id) %>%
  left_join(gdqs_nss %>% select(new_id, common_id), by = 'new_id') %>%
  relocate(common_id, .before = sector)


## test that the data is in working order and looks to have the correct distributions

for (item in colnames(final_gdqs_mimi)[71:80]) {
  print(item)  # Print column name to check
  
  p <- ggplot(final_gdqs_mimi, aes(x = .data[[item]])) +  # Use .data[[item]]
    geom_histogram(fill = "lightblue", color = "black", bins = 30) +
    ggtitle(paste("Histogram of", item)) +
    theme_minimal()
  
  print(p)  # Ensure the plot is displayed inside the loop
}


# save the file

haven::write_dta(final_gdqs_mimi,paste0(processed_path, "GDQS_data_IFPRI_MIMI.dta" ))


