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


food_consumption_daily_afe <- readRDS(paste0(processed_path,"ind_nss2223_food_consumption.rds"))
hh_mn_intake <- readRDS(paste0(processed_path,"ind_nss2223_base_case.rds"))
hh_expenditure <- readRDS(paste0(processed_path,"ind_nss2223_hh_expenditure.rds"))
# read in the fct
ind_202223_fct <-  read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/nsso_202223_fct.xlsx")


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


# Current Indian fortification specification matched to PDS 
ind_fort_spec <- 
  data.frame(
    # items all per 100g
    Item_Code = c(101,61,107,62),
    vita_rae_mcg_fort = c(62.5,62.5,62.5,62.5),
    thia_mg_fort = c(0.125,0.125,0.125,0.125),
    ribo_mg_fort = c(NA,NA,0.15,0.15),
    niac_mg_fort = c(1.575,1.575,1.575,1.575),
    vitb6_mg_fort = c(0.2,0.2,0.2,0.2),
    folate_mcg_fort = c(10,10,10,10),
    vitb12_mcg_fort = c(0.1,0.1,0.1,0.1),
    fe_mg_fort = c(3.525,3.525,1.7625,1.7625),
    zn_mg_fort = c(1.25,1.25,1.25,1.25),
    
    #wheat flour assumed 75-149g/day
    vita_rae_mcg_fort_wfp = c(150,150, 300,300),
    thia_mg_fort_wfp = c(0.5,0.5,0.3,0.3),
    ribo_mg_fort_wfp = c(NA,NA,0.2,0.2),
    niac_mg_fort_wfp = c(7,7,4,4),
    vitb6_mg_fort_wfp = c(0.6,0.6,0.2,0.2),
    folate_mcg_fort_wfp = c(130,130,260,260),
    vitb12_mcg_fort_wfp = c(1,1,2,2),
    fe_mg_fort_wfp = c(7,7,4,4),#wf = NaFeEDPT
    zn_mg_fort_wfp = c(6,6, 5.5,5.5)
    
    # #wheat flour assumed 150-300g/day
    # vita_rae_mcg_fort_wfp = c(150,150, 150,150),
    # thia_mg_fort_wfp = c(0.5,0.5,0.3,0.3),
    # ribo_mg_fort_wfp = c(NA,NA,0.2,0.2),
    # niac_mg_fort_wfp = c(7,7,4,4),
    # vitb6_mg_fort_wfp = c(0.6,0.6,0.2,0.2),
    # folate_mcg_fort_wfp = c(130,130,130,130),
    # vitb12_mcg_fort_wfp = c(1,1,1,1),
    # fe_mg_fort_wfp = c(7,7,2,2),#wf = NaFeEDPT
    # zn_mg_fort_wfp = c(6,6, 5.5,5.5)
    
  )

## Functions ###################################################################



calculate_inadequacy <- function(micronutrient, ear_cut){return(ifelse(micronutrient<ear_cut,1,0))}

aggregated_inadequacy <- function(data,group){
  # for each micronutrient and fortification scenario, we compare to the EAR value
  # then calculate a population risk of inadequacy
  #
  # print({{group}})
  group_sym <- ensym(group) 
  
  data %>%
    mutate(
      #compare to the estimated average requirement (EAR)
      folate_inad = ifelse(folate_ug<180,1,0),
      vitb12_inad = ifelse(vitaminb12_in_mcg< 2, 1,0),
      thia_inad = ifelse(vitb1_mg<0.9, 1,0),
      ribo_inad = ifelse(vitb2_mg<2,1, 0),
      niac_inad = ifelse(vitb3_mg<nin_ear$ear_value[nin_ear$nutrient == "niac_mg"], 1, 0 ),
      vitb6_inad = ifelse(vitb6_mg <nin_ear$ear_value[nin_ear$nutrient == "vitb6_mg"],1,0 ),
      vita_inad = ifelse(vita_mcg< nin_ear$ear_value[nin_ear$nutrient == "vita_rae_mcg"],1,0),
      zn_inad = ifelse(zinc_mg<nin_ear$ear_value[nin_ear$nutrient == "zn_mg"],1,0),
      
      folate_inad_fort = ifelse(folate_mcg_fort < 180, 1,0),
      vitb12_inad_fort = ifelse(vitb12_mcg_fort<2,1,0),
      thia_inad_fort = ifelse(thia_mg_fort<0.9, 1,0),
      ribo_inad_fort = ifelse(ribo_mg_fort<2,1, 0),
      niac_inad_fort = ifelse(niac_mg_fort<nin_ear$ear_value[nin_ear$nutrient == "niac_mg"], 1, 0 ),
      vitb6_inad_fort = ifelse(vitb6_mg_fort <nin_ear$ear_value[nin_ear$nutrient == "vitb6_mg"],1,0 ),
      vita_inad_fort = ifelse(vita_rae_mcg_fort< nin_ear$ear_value[nin_ear$nutrient == "vita_rae_mcg"],1,0),
      zn_inad_fort = ifelse(zn_mg_fort<nin_ear$ear_value[nin_ear$nutrient == "zn_mg"],1,0),
      
      folate_inad_fort_wfp = ifelse(folate_mcg_fort_wfp < 180, 1,0),
      vitb12_inad_fort_wfp = ifelse(vitb12_mcg_fort_wfp<2,1,0),
      thia_inad_fort_wfp = ifelse(thia_mg_fort_wfp<0.9, 1,0),
      ribo_inad_fort_wfp = ifelse(ribo_mg_fort_wfp<2,1, 0),
      niac_inad_fort_wfp = ifelse(niac_mg_fort_wfp<nin_ear$ear_value[nin_ear$nutrient == "niac_mg"], 1, 0 ),
      vitb6_inad_fort_wfp = ifelse(vitb6_mg_fort_wfp <nin_ear$ear_value[nin_ear$nutrient == "vitb6_mg"],1,0 ),
      vita_inad_fort_wfp = ifelse(vita_rae_mcg_fort_wfp< nin_ear$ear_value[nin_ear$nutrient == "vita_rae_mcg"],1,0),
      zn_inad_fort_wfp = ifelse(zn_mg_fort_wfp<nin_ear$ear_value[nin_ear$nutrient == "zn_mg"],1,0)
      
      
      
      # #mar
      # fol_nar = ifelse(folate_ug < 180, folate_ug/180,1),
      # vb12_nar = ifelse(vitaminb12_in_mcg < 2, vitaminb12_in_mcg/2,1),
      # iron_nar = ifelse(iron_mg < 15, folate_ug/15,1),
    ) %>%
    left_join(level01 %>%
                mutate(multiplier = as.numeric(multiplier)), by = "common_id") %>%
    left_join(hh_expenditure %>% select(common_id, sep_quintile,res_quintile), by = "common_id") %>% 
    mutate(national  = '1') %>% 
    as_survey_design(ids = common_id, 
                     # strata = sector, 
                     weights = multiplier) %>%
    srvyr::group_by({{group}}) %>%
    # srvyr::group_by(sep_quintile) %>%
    summarise(
      across(contains("inad"),~survey_mean(. == 1, proportion = T, na.rm = T)*100)
    ) %>%
    left_join(
      fe_full_prob(data %>%
                     rename(ai_afe = iron_mg) %>%
                     left_join(level01, by="common_id") %>% 
                     left_join(hh_expenditure %>% select(common_id, sep_quintile,res_quintile), by = "common_id") %>% 
                     mutate(national = '1')           ,
                   group1 = {{group}}, bio_avail = 10) %>%
        rename(!!group_sym := subpopulation,
               fe_inad = prev_inad)
      
    ) %>%
    left_join(
      fe_full_prob(data %>%
                     rename(ai_afe = fe_mg_fort) %>%
                     left_join(level01, by="common_id")%>% 
                     left_join(hh_expenditure %>% select(common_id, sep_quintile,res_quintile), by = "common_id") %>% 
                     mutate(national = '1') ,
                   group1 = {{group}}, bio_avail = 10) %>%
        rename(!!group_sym := subpopulation,
               fe_inad_fort = prev_inad)
    ) %>%
    left_join(
      fe_full_prob(data %>%
                     rename(ai_afe = fe_mg_fort_wfp) %>%
                     left_join(level01, by="common_id")%>% 
                     left_join(hh_expenditure %>% select(common_id, sep_quintile,res_quintile), by = "common_id") %>%
                     mutate(national = '1') , 
                   group1 = {{group}},
                   # group1 = `state`,
                   bio_avail = 10) %>%
        rename(!!group_sym := subpopulation,
               fe_inad_fort_wfp = prev_inad)
    )
}

## Analysis ####################################################################

# add contributions to fortified pds rice

rice_contributions <- food_consumption_daily_afe %>% 
  # create a data frame that has the potential contributions from fortifying pds rice
  select(
    common_id,Item_Code,Total_Consumption_Quantity) %>% 
  #filter only pds rice and free rice and other sources
  filter(Item_Code %in% c(061,101)) %>% 
  left_join(ind_fort_spec, by = "Item_Code") %>% 
  mutate(
    across(
      ends_with("_fort"),
      ~.x*(Total_Consumption_Quantity/100)
    ),
    across(
      ends_with("_wfp"),
      ~.x*(Total_Consumption_Quantity/100)
    )
  ) %>% 
  group_by(common_id) %>% 
  summarise(
    across(-c(Item_Code, Total_Consumption_Quantity),
           ~sum(., na.rm = TRUE))
  )



wf_contributions <- food_consumption_daily_afe %>% 
  select(
    common_id,Item_Code,Total_Consumption_Quantity) %>% 
  #filter only pds rice and free rice and other sources
  filter(Item_Code %in% c(062,107)) %>% 
  left_join(ind_fort_spec, by = "Item_Code") %>% 
  mutate(
    across(
      ends_with("_fort"),
      ~.x*(Total_Consumption_Quantity/100),
      .names = "{.col}_wf"
    ),
    across(
      ends_with("_wfp"),
      ~.x*(Total_Consumption_Quantity/100),
      .names = "{.col}_wf"
    )
  ) %>% 
  select(common_id,Item_Code, Total_Consumption_Quantity, ends_with("_wf")) %>% 
  group_by(common_id) %>% 
  summarise(
    across(-c(Item_Code, Total_Consumption_Quantity),
           ~sum(., na.rm = TRUE))
  )

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
rm(rice_contributions)
# x <- level01 %>% filter(state == "08") %>% 
#   left_join(hh_mn_intake_fort_rice, by= 'common_id')

## Wheat flour 

hh_mn_intake_fort_wf <- hh_mn_intake_fort_rice%>% 
  # take the fortification scenarios of rice and add on top 
  left_join(wf_contributions, by= 'common_id') %>% 
  mutate(across(everything(), ~ifelse(is.na(.),0,.))) %>% 
  
  mutate(folate_mcg_fort  = folate_mcg_fort+folate_mcg_fort_wf ,
         fe_mg_fort = fe_mg_fort+fe_mg_fort_wf,
         thia_mg_fort = thia_mg_fort+thia_mg_fort_wf,
         ribo_mg_fort = ribo_mg_fort+ribo_mg_fort_wf,
         vitb12_mcg_fort = vitb12_mcg_fort+vitb12_mcg_fort_wf,
         niac_mg_fort = niac_mg_fort + niac_mg_fort_wf,
         vitb6_mg_fort = vitb6_mg_fort+ vitb6_mg_fort_wf, 
         vita_rae_mcg_fort = vita_rae_mcg_fort + vita_rae_mcg_fort_wf,
         zn_mg_fort = zn_mg_fort + zn_mg_fort_wf,
         

         
         folate_mcg_fort_wfp = folate_mcg_fort_wfp+folate_mcg_fort_wfp_wf,
         fe_mg_fort_wfp = fe_mg_fort_wfp+fe_mg_fort_wfp_wf,
         thia_mg_fort_wfp = thia_mg_fort_wfp+thia_mg_fort_wfp_wf,
         ribo_mg_fort_wfp = ribo_mg_fort_wfp+ribo_mg_fort_wfp_wf,
         vitb12_mcg_fort_wfp = vitb12_mcg_fort_wfp+vitb12_mcg_fort_wfp_wf,
         niac_mg_fort_wfp = niac_mg_fort_wfp + niac_mg_fort_wfp_wf,
         vitb6_mg_fort_wfp = vitb6_mg_fort_wfp+ vitb6_mg_fort_wfp_wf, 
         vita_rae_mcg_fort_wfp = vita_rae_mcg_fort_wfp + vita_rae_mcg_fort_wfp_wf,
         zn_mg_fort_wfp = zn_mg_fort_wfp + zn_mg_fort_wfp_wf
  )

rm(wf_contributions)


hh_expenditure <- hh_expenditure%>%
  mutate(
    sector = ifelse(sector == '1', "Rural","Urban"),
    res_quintile = paste(sector, res_quintile))


## Create df with aggregated data

# rice pds

# nss_region inadequacy
nss_region_inad <- aggregated_inadequacy(hh_mn_intake_fort_rice,`nss_region`)


# state differences
state_inad <- aggregated_inadequacy(hh_mn_intake_fort_rice,`state`)


# sep 

sep_inad <- aggregated_inadequacy(hh_mn_intake_fort_rice,`sep_quintile`) 

#sector

sector_inad <- aggregated_inadequacy(hh_mn_intake_fort_rice,`sector`) 

# res quintile
res_quin_inad <- aggregated_inadequacy(hh_mn_intake_fort_rice,`res_quintile`)

# national 
national_inad <- aggregated_inadequacy(hh_mn_intake_fort_rice,`national`)


#total inadequacy 

all_inad_rice <- national_inad %>% 
  rename(category = national) %>% 
  mutate(category = "national") %>% 
  bind_rows(state_inad %>% rename(category = state)) %>% 
  bind_rows(sep_inad %>% rename(category = sep_quintile) %>% 
              mutate(category = paste("quntile", category))) %>% 
  bind_rows(sector_inad %>% rename(category = sector)) %>% 
  bind_rows(res_quin_inad %>% rename(category = res_quintile))





## wheat flour 

nss_region_inad_wf<- aggregated_inadequacy(hh_mn_intake_fort_wf, `nss_region`)


national_inad_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf, `national`)

##  
state_inad_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf, `state`)

sep_inad_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf, `sep_quintile`)

res_inad_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf, `res_quintile`)

sector_inad_wf <- aggregated_inadequacy(hh_mn_intake_fort_wf, `sector`)



all_inad_wf <- national_inad_wf %>% 
  rename(category = national) %>% 
  mutate(category = "national") %>% 
  bind_rows(state_inad_wf %>% rename(category = state)) %>% 
  bind_rows(sep_inad_wf %>% rename(category = sep_quintile) %>% 
              mutate(category = paste("quntile", category))) %>% 
  bind_rows(sector_inad_wf %>% rename(category = sector)%>% mutate(category = ifelse(category == 1, 
                                                                                     "Urban", "Rural"))) %>% 
  bind_rows(res_inad_wf %>% rename(category = res_quintile) )


### just ribo flavin for wheat

hh_mn_intake_fort_wf %>% 
  select(common_id, vitb2_mg,ribo_mg_fort,ribo_mg_fort_wfp, ribo_mg_fort_wf, ribo_mg_fort_wfp_wf) %>% 
  mean(ribo_mg_fort_wfp)

## Save #######################################################################


# summary csvs
write.csv(all_inad_wf,"india_wf_inad.csv")
write.csv(all_inad_rice,"india_rice_inad.csv" )

# r data for further analysis
saveRDS(hh_mn_intake_fort_wf, "ind_fort_wf.rds")
saveRDS(hh_mn_intake_fort_rice, "ind_fort_rice.rds")
saveRDS(nss_region_inad, "nss_region_inad_rice.rds" )
saveRDS(nss_region_inad_wf, "nss_region_inad_wf.rds" )

rm(list = ls())
