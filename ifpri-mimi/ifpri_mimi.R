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

gdqs_nss <- read_dta("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Workstream 2/Policy research/India/MIMI-IFPRI GDQS Collaboration/GDQS_data_IFPRI.dta")



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

gdqs_nss <- gdqs_nss %>% 
  mutate(new_id = 
           paste0(svy_name,year,fsu_sno,sector,state,nss_region,district,stratum,substratum,
                 panel," ", subsample,fod_sub_reg,sample_su_no," ", sec_stage_strat,common_id))


gdqs_nss %>% 
  left_join(
    level01, by = c(
      "svy_name" = "survey_name","year" = "year","fsu_sno" = "fsu_serial_no" ,
       "sector" = "sector","state" = "state","nss_region" = "nss_region","district" = "district","stratum" = "stratum",
      "substratum" = "sub_stratum_no","panel" = "panel", "subsample" = "sub_sample"
      # "fod_sub_reg" = "fod_sub_region","sample_su_no" = "sample_su_no","sec_stage_strat" = "fod_sub_region",
      # "common_id" = "sample_hhld_no ")
      # 
    ))
  
