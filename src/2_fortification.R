#########################################
#      HCES 2022-23 INDIA    
#          fortification                #
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

source(here::here("../git/MIMI1_archive/universal_functions/iron_full_probability/src/iron_inad_prev.R"))

################################################################################
# set paths
figure_path <- "figures/"
raw_path <- "data/raw/"
processed_path <- "data/processed/"



file_list = list.files("data/raw/HCES_2022_23/")
# haven::read_dta("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/level06.dta")
data_list <- lapply(paste0("data/raw/HCES_2022_23/",file_list), haven::read_dta)
names(data_list) <- tools::file_path_sans_ext(file_list)
level01 <- data_list$level01


food_consumption_daily_afe <- readRDS(paste0(processed_path,"ind_nss2223_food_consumption.rds"))
hh_mn_intake <- readRDS(paste0(processed_path,"ind_nss2223_base_case.rds"))
hh_expenditure <- readRDS(paste0(processed_path,"ind_nss2223_hh_expenditure.rds"))
# read in the fct
ind_202223_fct <-  read_xlsx("data/raw/nsso_202223_fct.xlsx")


## Constants ###################################################################

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


# Current Indian fortification specification matched to rice and wheat flour codes 
ind_fort_spec <- 
  data.frame(
    # items all per 100g

    Item_Code = c(101,61,102,107,62,108),
    vita_rae_mcg_fort = c(62.5,62.5,62.5,62.5*0.79,62.5*0.79,62.5*0.79),
    thia_mg_fort = c(0.125,0.125,0.125,0.125*0.69,0.125*0.69,0.125*0.69),
    ribo_mg_fort = c(NA,NA,NA,0.15*0.85,0.15*0.85,0.15*0.85),
    niac_mg_fort = c(1.575,1.575,1.575,1.575*0.85,1.575*0.85,1.575*0.85),
    vitb6_mg_fort = c(0.2,0.2,0.2,0.2*0.83,0.2*0.83,0.2*0.83),
    folate_mcg_fort = c(10,10,10,10*0.83,10*0.83,10*0.83),
    vitb12_mcg_fort = c(0.1,0.1,0.1,0.1*0.85,0.1*0.85,0.1*0.85),
    fe_mg_fort = c(3.525,3.525,3.525,1.7625,1.7625,1.7625),
    zn_mg_fort = c(1.25,1.25,1.25,1.25,1.25,1.25),
    
    # #wheat flour assumed 75-149g/day
    # # for the wheat flour, multiply by expected losses
    # vita_rae_mcg_fort_wfp = c(150,150, 150,300*0.79,300*0.79,300*0.79),
    # thia_mg_fort_wfp = c(0.5,0.5,0.5,0.3*0.69,0.3*0.69,0.3*0.69),
    # ribo_mg_fort_wfp = c(NA,NA,NA,0.2*0.85,0.2*0.85,0.2*0.85),
    # niac_mg_fort_wfp = c(7,7,7,4*0.85,4*0.85,4*0.85),
    # vitb6_mg_fort_wfp = c(0.6,0.6,0.6,0.2*0.83,0.2*0.83,0.2*0.83),
    # folate_mcg_fort_wfp = c(130,130,130,260*0.83,260*0.83,260*0.83),
    # vitb12_mcg_fort_wfp = c(1,1,1,2*0.85,2*0.85,2*0.85),
    # fe_mg_fort_wfp = c(7,7,7,4,4,4),#wf = NaFeEDPT
    # zn_mg_fort_wfp = c(6,6,6,5.5, 5.5,5.5)
    # 
    
    #wheat flour assumed 150-300g/day
    # for the wheat flour, multiply by expected losses
    vita_rae_mcg_fort_wfp = c(150,150, 150,150*0.79,150*0.79,150*0.79),
    thia_mg_fort_wfp = c(0.5,0.5,0.5,0.3*0.69,0.3*0.69,0.3*0.69),
    ribo_mg_fort_wfp = c(NA,NA,NA,0.2*0.85,0.2*0.85,0.2*0.85),
    niac_mg_fort_wfp = c(7,7,7,4*0.85,4*0.85,4*0.85),
    vitb6_mg_fort_wfp = c(0.6,0.6,0.6,0.2*0.83,0.2*0.83,0.2*0.83),
    folate_mcg_fort_wfp = c(130,130,130,130*0.83,130*0.83,130*0.83),
    vitb12_mcg_fort_wfp = c(1,1,1,1*0.85,1*0.85,1*0.85),
    fe_mg_fort_wfp = c(7,7,7,2,2,2),#wf = NaFeEDPT
    zn_mg_fort_wfp = c(6,6,6,4, 4,4)

  )

## Functions ###################################################################


source("functions/aggregated_inadequacy.R")

source("functions/contributions.R")

## Analysis ####################################################################

# add contributions to fortified pds rice

# contributions on from PDS
rice_contributions <- contributions('rice')


food_consumption_daily_afe %>% filter(Item_Code%in%c(102)) %>% 
  select(source, Total_Consumption_Quantity, Home_Produce_Quantity) %>% 
  filter(source == '1')

rice_conttributions_comerical <- food_consumption_daily_afe %>% 
  
  select(
    common_id,Item_Code,Home_Produce_Quantity, Total_Consumption_Quantity, source) %>% 
 
  #filter only 'other rice' 
  filter(Item_Code %in% c(102)&
           #source is purchased,  or home or purchsed only
         source %in% c(1,3)) %>% 
 mutate(Home_Produce_Quantity = 
          ifelse(is.na(as.numeric(Home_Produce_Quantity)),0,as.numeric(Home_Produce_Quantity)),
        # remove any of the home produced quantity
        Purchased_Quantity = Total_Consumption_Quantity - Home_Produce_Quantity) %>%
  left_join(ind_fort_spec, by = "Item_Code") %>%
  mutate(
    across(
      ends_with("_fort"),
      ~.x*(Purchased_Quantity/100), 
      .names = "{.col}_2"
    ),
    across(
      ends_with("_wfp"),
      ~.x*(Purchased_Quantity/100),
      .names = "{.col}_2"
    )
  ) %>%
  select(common_id,Item_Code, Purchased_Quantity, ends_with("_2")) %>% 
  group_by(common_id) %>%
  summarise(
    across(-c(Item_Code),
           ~sum(., na.rm = TRUE))
  )

# 
# 
# wf_contributions <-  food_consumption_daily_afe %>% 
#   select(
#     common_id,Item_Code, Total_Consumption_Quantity) %>% 
#   filter(Item_Code %in%  c(062,107)) %>% 
#   # mutate(Home_Produce_Quantity = as.numeric(Home_Produce_Quantity),
#   #        # remove any of the home produced quantity
#   #        Purchased_Quantity = Total_Consumption_Quantity - Home_Produce_Quantity) %>%
#   left_join(ind_fort_spec, by = "Item_Code") %>%
#   mutate(
#     across(
#       ends_with("_fort"),
#       ~.x*(Total_Consumption_Quantity/100),
#       .names = "{.col}_wf"
#     ),
#     across(
#       ends_with("_wfp"),
#       ~.x*(Total_Consumption_Quantity/100),
#       .names = "{.col}_wf"
#     )
#   ) %>%
#   group_by(common_id) %>%
#   select(common_id,Item_Code, Total_Consumption_Quantity, ends_with("_wf")) %>% 
#   summarise(
#     across(-c(Item_Code),
#            ~sum(., na.rm = TRUE))
#   )


# wheat flour commercially purchased
# wf_conttributions_comerical <- food_consumption_daily_afe %>% 
#   
#   select(
#     common_id,Item_Code,Home_Produce_Quantity, Total_Consumption_Quantity, source) %>% 
#   mutate(source = as.numeric(source)) %>% 
#   #filter only 'other wheat ' 
#   filter(Item_Code %in% c(108) &
#            #source is purchased,  or home or purchsed only
#            source %in% c(1,3)) %>% 
#   mutate(Home_Produce_Quantity = 
#            ifelse(is.na(as.numeric(Home_Produce_Quantity)),0,as.numeric(Home_Produce_Quantity)),
#          # remove any of the home produced quantity
#          Purchased_Quantity = (Total_Consumption_Quantity - Home_Produce_Quantity) )%>%
#   left_join(ind_fort_spec, by = "Item_Code") %>% 
#   mutate(
#     across(
#       ends_with("_fort"),
#       ~.x*(Purchased_Quantity/100),
#       .names = "{.col}_2"
#     ),
#     across(
#       ends_with("_wfp"),
#       ~.x*(Purchased_Quantity/100),
#       .names = "{.col}_2"
#     )
#   ) %>%
#   select(common_id,Item_Code, Purchased_Quantity, ends_with("_2")) %>% 
#   group_by(common_id) %>%
#   summarise(
#     across(-c(Item_Code),
#            ~sum(., na.rm = TRUE))
#   )



# add contributions from rice
hh_mn_intake_fort_rice <- hh_mn_intake%>% 
  left_join(rice_contributions, by= 'common_id') %>% 
  mutate(across(everything(), ~ifelse(is.na(.),0,.))) %>% 
  
  mutate(folate_mcg_fort  = folate_ug+folate_mcg_fort ,
         fe_mg_fort = iron_mg+fe_mg_fort,
         thia_mg_fort = vitb1_mg+thia_mg_fort,
         ribo_mg_fort = vitb2_mg+ribo_mg_fort,
         vitb12_mcg_fort = vitaminb12_in_mcg+vitb12_mcg_fort,
         niac_mg_fort = vitb3_mg + niac_mg_fort,
         vitb6_mg_fort = vitb6_mg+ vitb6_mg_fort,
         vita_rae_mcg_fort = vita_mcg + vita_rae_mcg_fort,
         zn_mg_fort = zinc_mg + zn_mg_fort,
         
         folate_mcg_fort_wfp = folate_ug+folate_mcg_fort_wfp,
         fe_mg_fort_wfp = iron_mg+fe_mg_fort_wfp,
         thia_mg_fort_wfp = vitb1_mg+thia_mg_fort_wfp,
         ribo_mg_fort_wfp = vitb2_mg+ribo_mg_fort_wfp,
         vitb12_mcg_fort_wfp = vitaminb12_in_mcg+vitb12_mcg_fort_wfp,
         niac_mg_fort_wfp = vitb3_mg + niac_mg_fort_wfp,
         vitb6_mg_fort_wfp = vitb6_mg+ vitb6_mg_fort_wfp, 
         vita_rae_mcg_fort_wfp = vita_mcg + vita_rae_mcg_fort_wfp,
         zn_mg_fort_wfp = zinc_mg + zn_mg_fort_wfp
  )

# delete rice contributions df
# rm(rice_contributions)


## Wheat flour 
# 
# hh_mn_intake_fort_wf <- hh_mn_intake_fort_rice %>% 
#   # take the fortification scenarios of rice and add on top 
#   left_join(wf_contributions, by= 'common_id') %>% 
#   mutate(across(everything(), ~ifelse(is.na(.),0,.))) %>% 
#   
#   mutate(folate_mcg_fort  = folate_mcg_fort+folate_mcg_fort_wf ,
#          fe_mg_fort = fe_mg_fort+fe_mg_fort_wf,
#          thia_mg_fort = thia_mg_fort+thia_mg_fort_wf,
#          ribo_mg_fort = ribo_mg_fort+ribo_mg_fort_wf,
#          vitb12_mcg_fort = vitb12_mcg_fort+vitb12_mcg_fort_wf,
#          niac_mg_fort = niac_mg_fort + niac_mg_fort_wf,
#          vitb6_mg_fort = vitb6_mg_fort+ vitb6_mg_fort_wf, 
#          vita_rae_mcg_fort = vita_rae_mcg_fort + vita_rae_mcg_fort_wf,
#          zn_mg_fort = zn_mg_fort + zn_mg_fort_wf,
#          
# 
#          
#          folate_mcg_fort_wfp = folate_mcg_fort_wfp+folate_mcg_fort_wfp_wf,
#          fe_mg_fort_wfp = fe_mg_fort_wfp+fe_mg_fort_wfp_wf,
#          thia_mg_fort_wfp = thia_mg_fort_wfp+thia_mg_fort_wfp_wf,
#          ribo_mg_fort_wfp = ribo_mg_fort_wfp+ribo_mg_fort_wfp_wf,
#          vitb12_mcg_fort_wfp = vitb12_mcg_fort_wfp+vitb12_mcg_fort_wfp_wf,
#          niac_mg_fort_wfp = niac_mg_fort_wfp + niac_mg_fort_wfp_wf,
#          vitb6_mg_fort_wfp = vitb6_mg_fort_wfp+ vitb6_mg_fort_wfp_wf, 
#          vita_rae_mcg_fort_wfp = vita_rae_mcg_fort_wfp + vita_rae_mcg_fort_wfp_wf,
#          zn_mg_fort_wfp = zn_mg_fort_wfp + zn_mg_fort_wfp_wf
#   )

# rm(wf_contributions)

# commercial contriubtions

# 
# 
# hh_mn_intake_fort_rice_comm <- hh_mn_intake_fort_rice%>% 
#   # take the fortification scenarios of rice and add on top 
#   left_join(rice_conttributions_comerical, by= 'common_id') %>% 
#   mutate(across(everything(), ~ifelse(is.na(.),0,.))) %>% 
#   
#   mutate(folate_mcg_fort  = folate_mcg_fort+folate_mcg_fort_2 ,
#          fe_mg_fort = fe_mg_fort+fe_mg_fort_2,
#          thia_mg_fort = thia_mg_fort+thia_mg_fort_2,
#          ribo_mg_fort = ribo_mg_fort+ribo_mg_fort_2,
#          vitb12_mcg_fort = vitb12_mcg_fort+vitb12_mcg_fort_2,
#          niac_mg_fort = niac_mg_fort + niac_mg_fort_2,
#          vitb6_mg_fort = vitb6_mg_fort+ vitb6_mg_fort_2, 
#          vita_rae_mcg_fort = vita_rae_mcg_fort + vita_rae_mcg_fort_2,
#          zn_mg_fort = zn_mg_fort + zn_mg_fort_2,
#          
#          
#          
#          folate_mcg_fort_wfp = folate_mcg_fort_wfp+folate_mcg_fort_wfp_2,
#          fe_mg_fort_wfp = fe_mg_fort_wfp+fe_mg_fort_wfp_2,
#          thia_mg_fort_wfp = thia_mg_fort_wfp+thia_mg_fort_wfp_2,
#          ribo_mg_fort_wfp = ribo_mg_fort_wfp+ribo_mg_fort_wfp_2,
#          vitb12_mcg_fort_wfp = vitb12_mcg_fort_wfp+vitb12_mcg_fort_wfp_2,
#          niac_mg_fort_wfp = niac_mg_fort_wfp + niac_mg_fort_wfp_2,
#          vitb6_mg_fort_wfp = vitb6_mg_fort_wfp+ vitb6_mg_fort_wfp_2, 
#          vita_rae_mcg_fort_wfp = vita_rae_mcg_fort_wfp + vita_rae_mcg_fort_wfp_2,
#          zn_mg_fort_wfp = zn_mg_fort_wfp + zn_mg_fort_wfp_2
#   )
# 
# 
# hh_mn_intake_fort_wf_comm <- hh_mn_intake_fort_rice%>% 
#   # take the fortification scenarios of rice and add on top 
#   left_join(wf_conttributions_comerical, by= 'common_id') %>% 
#   mutate(across(everything(), ~ifelse(is.na(.),0,.))) %>% 
#   
#   mutate(folate_mcg_fort  = folate_mcg_fort+folate_mcg_fort_2 ,
#          fe_mg_fort = fe_mg_fort+fe_mg_fort_2,
#          thia_mg_fort = thia_mg_fort+thia_mg_fort_2,
#          ribo_mg_fort = ribo_mg_fort+ribo_mg_fort_2,
#          vitb12_mcg_fort = vitb12_mcg_fort+vitb12_mcg_fort_2,
#          niac_mg_fort = niac_mg_fort + niac_mg_fort_2,
#          vitb6_mg_fort = vitb6_mg_fort+ vitb6_mg_fort_2, 
#          vita_rae_mcg_fort = vita_rae_mcg_fort + vita_rae_mcg_fort_2,
#          zn_mg_fort = zn_mg_fort + zn_mg_fort_2,
#          
#          
#          
#          folate_mcg_fort_wfp = folate_mcg_fort_wfp+folate_mcg_fort_wfp_2,
#          fe_mg_fort_wfp = fe_mg_fort_wfp+fe_mg_fort_wfp_2,
#          thia_mg_fort_wfp = thia_mg_fort_wfp+thia_mg_fort_wfp_2,
#          ribo_mg_fort_wfp = ribo_mg_fort_wfp+ribo_mg_fort_wfp_2,
#          vitb12_mcg_fort_wfp = vitb12_mcg_fort_wfp+vitb12_mcg_fort_wfp_2,
#          niac_mg_fort_wfp = niac_mg_fort_wfp + niac_mg_fort_wfp_2,
#          vitb6_mg_fort_wfp = vitb6_mg_fort_wfp+ vitb6_mg_fort_wfp_2, 
#          vita_rae_mcg_fort_wfp = vita_rae_mcg_fort_wfp + vita_rae_mcg_fort_wfp_2,
#          zn_mg_fort_wfp = zn_mg_fort_wfp + zn_mg_fort_wfp_2
#   )
# 
# # maximum case (all vehicles fortified)
# all_vehicles <- hh_mn_intake_fort_rice %>% 
#   left_join(wf_conttributions_comerical, by= 'common_id') %>% 
#   mutate(across(everything(), ~ifelse(is.na(.),0,.))) %>% 
#   
#   mutate(folate_mcg_fort  = folate_mcg_fort+folate_mcg_fort_2 ,
#          fe_mg_fort = fe_mg_fort+fe_mg_fort_2,
#          thia_mg_fort = thia_mg_fort+thia_mg_fort_2,
#          ribo_mg_fort = ribo_mg_fort+ribo_mg_fort_2,
#          vitb12_mcg_fort = vitb12_mcg_fort+vitb12_mcg_fort_2,
#          niac_mg_fort = niac_mg_fort + niac_mg_fort_2,
#          vitb6_mg_fort = vitb6_mg_fort+ vitb6_mg_fort_2, 
#          vita_rae_mcg_fort = vita_rae_mcg_fort + vita_rae_mcg_fort_2,
#          zn_mg_fort = zn_mg_fort + zn_mg_fort_2,
#          
#          
#          
#          folate_mcg_fort_wfp = folate_mcg_fort_wfp+folate_mcg_fort_wfp_2,
#          fe_mg_fort_wfp = fe_mg_fort_wfp+fe_mg_fort_wfp_2,
#          thia_mg_fort_wfp = thia_mg_fort_wfp+thia_mg_fort_wfp_2,
#          ribo_mg_fort_wfp = ribo_mg_fort_wfp+ribo_mg_fort_wfp_2,
#          vitb12_mcg_fort_wfp = vitb12_mcg_fort_wfp+vitb12_mcg_fort_wfp_2,
#          niac_mg_fort_wfp = niac_mg_fort_wfp + niac_mg_fort_wfp_2,
#          vitb6_mg_fort_wfp = vitb6_mg_fort_wfp+ vitb6_mg_fort_wfp_2, 
#          vita_rae_mcg_fort_wfp = vita_rae_mcg_fort_wfp + vita_rae_mcg_fort_wfp_2,
#          zn_mg_fort_wfp = zn_mg_fort_wfp + zn_mg_fort_wfp_2
#   ) %>% 
#   select(
#     common_id, 
#     energy_kcal,
#     Total_Consumption_Quantity,
#     Purchased_Quantity ,
#     ends_with("_fort"),
#     ends_with("_wfp")
#   ) %>% 
#   rename(wf_comm_quantity = Purchased_Quantity,
#          rice_pds_quantity = Total_Consumption_Quantity) %>% 
#   left_join(rice_conttributions_comerical, by= 'common_id') %>% 
#   mutate(across(everything(), ~ifelse(is.na(.),0,.))) %>% 
#   
#   mutate(folate_mcg_fort  = folate_mcg_fort+folate_mcg_fort_2 ,
#          fe_mg_fort = fe_mg_fort+fe_mg_fort_2,
#          thia_mg_fort = thia_mg_fort+thia_mg_fort_2,
#          ribo_mg_fort = ribo_mg_fort+ribo_mg_fort_2,
#          vitb12_mcg_fort = vitb12_mcg_fort+vitb12_mcg_fort_2,
#          niac_mg_fort = niac_mg_fort + niac_mg_fort_2,
#          vitb6_mg_fort = vitb6_mg_fort+ vitb6_mg_fort_2, 
#          vita_rae_mcg_fort = vita_rae_mcg_fort + vita_rae_mcg_fort_2,
#          zn_mg_fort = zn_mg_fort + zn_mg_fort_2,
#          
#          
#          
#          folate_mcg_fort_wfp = folate_mcg_fort_wfp+folate_mcg_fort_wfp_2,
#          fe_mg_fort_wfp = fe_mg_fort_wfp+fe_mg_fort_wfp_2,
#          thia_mg_fort_wfp = thia_mg_fort_wfp+thia_mg_fort_wfp_2,
#          ribo_mg_fort_wfp = ribo_mg_fort_wfp+ribo_mg_fort_wfp_2,
#          vitb12_mcg_fort_wfp = vitb12_mcg_fort_wfp+vitb12_mcg_fort_wfp_2,
#          niac_mg_fort_wfp = niac_mg_fort_wfp + niac_mg_fort_wfp_2,
#          vitb6_mg_fort_wfp = vitb6_mg_fort_wfp+ vitb6_mg_fort_wfp_2, 
#          vita_rae_mcg_fort_wfp = vita_rae_mcg_fort_wfp + vita_rae_mcg_fort_wfp_2,
#          zn_mg_fort_wfp = zn_mg_fort_wfp + zn_mg_fort_wfp_2
#   ) %>% 
#   select(
#     common_id,
#     energy_kcal,
#     Purchased_Quantity , 
#     rice_pds_quantity,
#     wf_comm_quantity,
#     ends_with("_fort"),
#     ends_with("_wfp")
#   ) %>% 
#   rename(rice_comm_quantity = Purchased_Quantity) %>% 
#   left_join(wf_contributions, by= 'common_id') %>% 
#   mutate(across(everything(), ~ifelse(is.na(.),0,.))) %>% 
#   
#   mutate(folate_mcg_fort  = folate_mcg_fort+folate_mcg_fort_wf ,
#          fe_mg_fort = fe_mg_fort+fe_mg_fort_wf,
#          thia_mg_fort = thia_mg_fort+thia_mg_fort_wf,
#          ribo_mg_fort = ribo_mg_fort+ribo_mg_fort_wf,
#          vitb12_mcg_fort = vitb12_mcg_fort+vitb12_mcg_fort_wf,
#          niac_mg_fort = niac_mg_fort + niac_mg_fort_wf,
#          vitb6_mg_fort = vitb6_mg_fort+ vitb6_mg_fort_wf, 
#          vita_rae_mcg_fort = vita_rae_mcg_fort + vita_rae_mcg_fort_wf,
#          zn_mg_fort = zn_mg_fort + zn_mg_fort_wf,
#          
#          
#          
#          folate_mcg_fort_wfp = folate_mcg_fort_wfp+folate_mcg_fort_wfp_wf,
#          fe_mg_fort_wfp = fe_mg_fort_wfp+fe_mg_fort_wfp_wf,
#          thia_mg_fort_wfp = thia_mg_fort_wfp+thia_mg_fort_wfp_wf,
#          ribo_mg_fort_wfp = ribo_mg_fort_wfp+ribo_mg_fort_wfp_wf,
#          vitb12_mcg_fort_wfp = vitb12_mcg_fort_wfp+vitb12_mcg_fort_wfp_wf,
#          niac_mg_fort_wfp = niac_mg_fort_wfp + niac_mg_fort_wfp_wf,
#          vitb6_mg_fort_wfp = vitb6_mg_fort_wfp+ vitb6_mg_fort_wfp_wf, 
#          vita_rae_mcg_fort_wfp = vita_rae_mcg_fort_wfp + vita_rae_mcg_fort_wfp_wf,
#          zn_mg_fort_wfp = zn_mg_fort_wfp + zn_mg_fort_wfp_wf
#   ) %>% 
#   select(
#     common_id, Total_Consumption_Quantity , wf_comm_quantity, rice_comm_quantity,
#     rice_pds_quantity,
#     energy_kcal,
#     ends_with("_fort"),
#     ends_with("_wfp")
#   ) %>% 
#   rename(wf_pds_quantity = Total_Consumption_Quantity)
# 

hh_expenditure <- hh_expenditure%>%
  mutate(
    sector = ifelse(sector == '1', "Rural","Urban"),
    res_quintile = paste(sector, res_quintile))


## Create df with aggregated data

# # rice pds
nss_region_inad <- aggregated_inadequacy(hh_mn_intake_fort_rice,`nss_region`)
# state_inad <- aggregated_inadequacy(hh_mn_intake_fort_rice,`state`)
# sep_inad <- aggregated_inadequacy(hh_mn_intake_fort_rice,`sep_quintile`) 
# sector_inad <- aggregated_inadequacy(hh_mn_intake_fort_rice,`sector`) 
# res_quin_inad <- aggregated_inadequacy(hh_mn_intake_fort_rice,`res_quintile`)
# national_inad <- aggregated_inadequacy(hh_mn_intake_fort_rice,`national`)
# 
# all_inad_rice <- national_inad %>% 
#   rename(category = national) %>% 
#   mutate(category = "national") %>% 
#   bind_rows(state_inad %>% rename(category = state)) %>% 
#   bind_rows(sep_inad %>% rename(category = sep_quintile) %>% 
#               mutate(category = paste("quntile", category))) %>% 
#   bind_rows(sector_inad %>% rename(category = sector)) %>% 
#   bind_rows(res_quin_inad %>% rename(category = res_quintile))

## wheat flour 

# nss_region_inad_wf<- aggregated_inadequacy(hh_mn_intake_fort_wf, `nss_region`)
# national_inad_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf, `national`)
# state_inad_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf, `state`)
# sep_inad_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf, `sep_quintile`)
# res_inad_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf, `res_quintile`)
# sector_inad_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf, `sector`)
# 
# all_inad_wf <- national_inad_wf %>% 
#   rename(category = national) %>% 
#   mutate(category = "national") %>% 
#   bind_rows(state_inad_wf %>% rename(category = state)) %>% 
#   bind_rows(sep_inad_wf %>% rename(category = sep_quintile) %>% 
#               mutate(category = paste("quntile", category))) %>% 
#   bind_rows(sector_inad_wf %>% rename(category = sector)%>% mutate(category = ifelse(category == 1, 
#                                                                                      "Urban", "Rural"))) %>% 
#   bind_rows(res_inad_wf %>% rename(category = res_quintile) )


# rice commercial 



# nss_region_inad_com_rice<- aggregated_inadequacy(hh_mn_intake_fort_rice_comm, `nss_region`)
# national_inad_com_rice <- aggregated_inadequacy(hh_mn_intake_fort_rice_comm, `national`)
# state_inad_com_rice <- aggregated_inadequacy(hh_mn_intake_fort_rice_comm, `state`)
# sep_inad_com_rice <- aggregated_inadequacy(hh_mn_intake_fort_rice_comm, `sep_quintile`)
# res_inad_com_rice <- aggregated_inadequacy(hh_mn_intake_fort_rice_comm, `res_quintile`)
# sector_inad_com_rice <- aggregated_inadequacy(hh_mn_intake_fort_rice_comm, `sector`)
# 
# all_inad_com_rice <- national_inad_com_rice %>%
#   rename(category = national) %>%
#   mutate(category = "national") %>%
#   bind_rows(state_inad_com_rice %>% rename(category = state)) %>%
#   bind_rows(sep_inad_com_rice %>% rename(category = sep_quintile) %>%
#               mutate(category = paste("quntile", category))) %>%
#   bind_rows(sector_inad_com_rice %>% rename(category = sector)%>% mutate(category = ifelse(category == 1,
#                                                                                      "Urban", "Rural"))) %>%
#   bind_rows(res_inad_com_rice %>% rename(category = res_quintile) )



# wheat commercial 

# nss_region_inad_com_wf<- aggregated_inadequacy(hh_mn_intake_fort_wf_comm, `nss_region`)
# national_inad_com_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf_comm, `national`)
# state_inad_com_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf_comm, `state`)
# sep_inad_com_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf_comm, `sep_quintile`)
# res_inad_com_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf_comm, `res_quintile`)
# sector_inad_com_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf_comm, `sector`)
# 
# all_inad_com_wf <- national_inad_com_wf %>%
#   rename(category = national) %>%
#   mutate(category = "national") %>%
#   bind_rows(state_inad_com_wf %>% rename(category = state)) %>%
#   bind_rows(sep_inad_com_wf %>% rename(category = sep_quintile) %>%
#               mutate(category = paste("quntile", category))) %>%
#   bind_rows(sector_inad_com_wf %>% rename(category = sector)%>% mutate(category = ifelse(category == 1,
#                                                                                            "Urban", "Rural"))) %>%
#   bind_rows(res_inad_com_wf %>% rename(category = res_quintile) )

### just ribo flavin for wheat
# hh_mn_intake %>% 
#   select(common_id, vitb2_mg) %>% 
#   mutate(
#     ribo_inad_fort_wfp = ifelse(vitb2_mg<2,1, 0)
# ) %>%
#   left_join(level01 %>%
#               mutate(multiplier = as.numeric(multiplier)), by = "common_id") %>%
#   left_join(hh_expenditure %>% select(common_id, sep_quintile,res_quintile), by = "common_id") %>% 
#   mutate(national  = '1') %>% 
#   as_survey_design(ids = common_id, 
#                    # strata = sector, 
#                    weights = multiplier) %>%
#   srvyr::group_by(national) %>%
#   # srvyr::group_by(sep_quintile) %>%
#   summarise(
#     across(contains("inad"),~survey_mean(. == 1, proportion = T, na.rm = T)*100)
#   )



## Save #############################s##########################################



# summary csvs
# write.csv(all_inad_wf,paste(processed_path,"india_wf_inad_v2.csv"))
write.csv(all_inad_rice,paste(processed_path,"india_rice_inad.csv" ))
# write.csv(all_inad_com_rice, paste(processed_path,'india_rice_com_inad.csv'))
# write.csv(all_inad_com_wf, paste(processed_path,'india_wf_com_inad.csv'))

# r data for further analysis
# saveRDS(hh_mn_intake_fort_wf, "ind_fort_wf_v2.rds")
saveRDS(hh_mn_intake_fort_rice, paste(processed_path, "ind_fort_rice.rds"))
# saveRDS(hh_mn_intake_fort_rice_comm, paste(processed_path,"ind_fort_rice_com.rds"))
# saveRDS(hh_mn_intake_fort_wf_comm, paste(processed_path,"ind_fort_wf_com.rds"))
# saveRDS(all_vehicles,paste(processed_path, "ind_fort_all.rds"))

saveRDS(nss_region_inad, paste0(processed_path,"nss_region_inad_rice.rds" ))
# saveRDS(nss_region_inad_wf, paste(processed_path,"nss_region_inad_wf_v2.rds" ))
# saveRDS(nss_region_inad_com_rice, paste(processed_path,"nss_region_inad_rice_com.rds" ))
# saveRDS(nss_region_inad_com_wf,paste(processed_path, "nss_region_inad_wf_com.rds" ))
rm(list = ls())
