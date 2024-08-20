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

#-------------------------------------------------------------------------------

file_list = list.files("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/")
haven::read_dta("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/level06.dta")
data_list <- lapply(paste0("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/",file_list), haven::read_dta)
names(data_list) <- tools::file_path_sans_ext(file_list)


ind_state <- sf::st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp")
ind_admin2 <- st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Nutrition analysis/shapefiles/ind_lss1819_adm2.shp")
plot(ind_state$geometry)




# AFE CALCUTATION ##############################################################

# filter only 6 states
level01 <- data_list$level01 %>% 
  filter(state %in% c("02", "03","06","07", "08","09", "10","19","20","21", "22","23","28","34","36"))
# 
level02 <- data_list$level02 %>% 
  filter(common_id %in% level01$common_id)

summary(factor(data_list$level01$sector))#1 = rural, 2 = urban
summary(factor(level02$gender))

# find households with under 2 years olds (assumed women to be lactating)
children_under_2 <- level02 %>% 
  dplyr::group_by(common_id) %>% 
  dplyr::summarise(
    under_2 = factor(ifelse(
      sum(age_years < 2) >= 1,
      1,
      0
    )
    )
  )

summary(children_under_2)

# constant from NIN requirements
adult_female_requirement <- 2130


# gender 1=male, 2=female, 3=transgender
# transgender energy = mean of male and female here ()

level02 <- level02 %>%
  dplyr::mutate(age_years = as.numeric(age_years)) %>% 
  dplyr::left_join(children_under_2, by = "common_id") %>% 
  dplyr::mutate(
    energy_requirement = 
      dplyr::case_when(
        age_years < 1 ~ 0, #
        age_years < 4 & age_years>=2~ 1070,
        age_years < 7 ~ 1360,
        age_years < 10 ~ 1700,
        age_years < 13 ~ ifelse(gender == "1", 2220, ifelse(gender == "2",2060, 2140)),
        age_years < 16 ~ ifelse(gender == "1", 2860, ifelse(gender == "2",2400, 2630)),
        age_years < 18 ~ ifelse(gender == "1", 3320, ifelse(gender == "2",2500, 3322)),
        age_years >= 18 ~ ifelse(gender == "1", 2710,
                                 ifelse(age_years<50, 2130,
                                        ifelse(under_2 == 0, 
                                               2130,
                                               ifelse(gender == "2", 2690, 2420))))
      ) 
  ) %>% 
  dplyr::mutate(
    afe = round(energy_requirement/adult_female_requirement,
                2)
  ) 

# sum for afe per household
hh_afe <- level02 %>% 
  group_by(common_id) %>% 
  summarise(
    afe = sum(afe),
    pc = n()
  ) %>% 
  select(common_id, afe, pc)

# check that it is roughly around the y = x line
hh_afe %>% 
  ggplot(aes(x = pc,  y = afe))+ 
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = 'red')


# FOOD CONSUMPTION #############################################################

level05 <- data_list$level05 %>% 
  filter(common_id %in% level01$common_id)


# level 5 is 30 day recall  all reported in kg

level05_30day <- 
  level05 %>% 
  mutate(Item_Code = as.numeric(Item_Code)) %>% 
  filter((Item_Code >100 & Item_Code<160) | 
           (Item_Code>169 &Item_Code<180) | 
           Item_Code %in% c(
             073,074,071,072,061,062,070,001,002,55,56,57,58,59,60,63,64,65,66,67,68
           )) %>% 
  left_join(ind_202223_fct %>% select(item_code, edible_portion), by = c("Item_Code" = "item_code")) %>% 
  mutate(Total_Consumption_Quantity = (as.numeric(Total_Consumption_Quantity)/30)*1000*edible_portion) %>% 
  select(-edible_portion) %>% 
  left_join(hh_afe, by = "common_id") %>% 
  mutate(Total_Consumption_Quantity = Total_Consumption_Quantity/afe) %>% 
  group_by(Item_Code) %>% 
  mutate(Total_Consumption_Quantity = ifelse(Total_Consumption_Quantity>mean(Total_Consumption_Quantity)+
                                               3*sd(Total_Consumption_Quantity),
                                             median(Total_Consumption_Quantity),
                                             Total_Consumption_Quantity
  )
  )


# 7 day recall 
conversion_factor <- read_csv("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/conversion_factors.csv")

level05_7day <- level05 %>% 
  mutate(Item_Code = as.numeric(Item_Code)) %>% 
  filter(!(Item_Code %in% level05_30day$Item_Code)) %>% 
  
  left_join(conversion_factor, by= 'Item_Code') %>% 
  left_join(ind_202223_fct %>% select(item_code, edible_portion), by = c("Item_Code" = "item_code")) %>% 
  mutate(Total_Consumption_Quantity = 
           ifelse(is.na(conversion_factor_to_kg), as.numeric(Total_Consumption_Quantity)*edible_portion, as.numeric(Total_Consumption_Quantity)*conversion_factor_to_kg*edible_portion)) %>% 
  select(-c(item_name, conversion_factor_to_kg, edible_portion)) %>% 
  mutate(Total_Consumption_Quantity = (Total_Consumption_Quantity/7)*1000) %>% 
  left_join(hh_afe, by = "common_id") %>% 
  mutate(Total_Consumption_Quantity = Total_Consumption_Quantity/afe) %>% 
  group_by(Item_Code) %>% 
  mutate(Total_Consumption_Quantity = ifelse(Total_Consumption_Quantity>mean(Total_Consumption_Quantity)+
                                               3*sd(Total_Consumption_Quantity),
                                             median(Total_Consumption_Quantity),
                                             Total_Consumption_Quantity
  )
  )



# read in the fct
ind_202223_fct <-  read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/nsso_202223_fct.xlsx")

# check whether drop down items are counted twice (they are)
x <- level05_30day %>% 
  filter(Item_Code == 116)

y = level05_30day %>% 
  filter(common_id == "HCES2022653801030311623025 203121  315")

rm(x,y)

################################################################################

food_consumption_daily_afe <- 
  bind_rows(level05_7day %>% mutate(Item_Code = as.numeric(Item_Code)),
            level05_30day)


unmerged <- anti_join(food_consumption_daily_afe, ind_202223_fct, by=c("Item_Code" ="item_code" )) %>% 
  distinct(Item_Code)

# all unmerged are on purpose as they are sub-totals of other food items


hh_mn_intake <- food_consumption_daily_afe %>% 
  inner_join(ind_202223_fct , by=c("Item_Code" ="item_code" )) %>% 
  mutate(quantity_100g = Total_Consumption_Quantity/100,
         energy_kcal = energy_kcal*quantity_100g,
         folate_ug = folate_ug*quantity_100g,
         iron_mg = iron_mg*quantity_100g,
         vitaminb12_in_mcg = vitaminb12_in_mcg*quantity_100g,
         vitb1_mg  = vitb1_mg* quantity_100g) %>% 
  select(common_id, energy_kcal,folate_ug,iron_mg,vitaminb12_in_mcg, vitb1_mg) %>% 
  group_by(common_id) %>% 
  summarise(
    across(
      everything(),
      ~sum(., na.rm = T)
    )
  )

# look at the energy distribution
hh_mn_intake %>% 
  ggplot(aes(x = energy_kcal))+
  geom_histogram()

summary(hh_mn_intake$energy_kcal)


# join to hh state information


nss_region_shapefile <- sf::st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/ind_nss2223_nssregion.shp")




################################################################################
# BASE CASE
source(here::here("../MIMI1_archive/universal_functions/iron_full_probability/src/iron_inad_prev.R"))

################################################################################
# fortification

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
    ribo_mg_fort = c(0.15,0.15,0.15,0.15),
    niac_mg_fort = c(1.575,1.575,1.575,1.575),
    vitb6_mg_fort = c(0.2,0.2,0.2,0.2),
    folate_mcg_fort = c(10,10,10,10),
    vitb12_mcg_fort = c(0.1,0.1,0.1,0.1),
    fe_mg_fort = c(3.525,3.525,3.525,3.525),
    zn_mg_fort = c(1.25,1.25,1.25,1.25),
    
    vita_rae_mcg_fort_wfp = c(150,150),
    thia_mg_fort_wfp = c(0.5,0.5),
    ribo_mg_fort_wfp = c(NA,NA),
    niac_mg_fort_wfp = c(7,7),
    vitb6_mg_fort_wfp = c(0.6,0.6),
    folate_mcg_fort_wfp = c(130,130),
    vitb12_mcg_fort_wfp = c(1,1),
    fe_mg_fort_wfp = c(7,7),
    zn_mg_fort_wfp = c(6,6)
    
  )



# add contributions to fortified pds rice

rice_contributions <- level05_30day %>% 
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


# add contributions from rice
hh_mn_intake_fort <- hh_mn_intake%>% 
  left_join(rice_contributions, by= 'common_id') %>% 
  mutate(across(everything(), ~ifelse(is.na(.),0,.))) %>% 

  mutate(folate_mcg_fort  = folate_ug+folate_mcg_fort ,
         fe_mg_fort = iron_mg+fe_mg_fort,
         thia_mg_fort = vitb1_mg+thia_mg_fort,
         vitb12_mcg_fort = vitaminb12_in_mcg+vitb12_mcg_fort,
         
         ## TODO complete for other micronutrients!!!
         
         folate_mcg_fort_wfp = folate_ug+folate_mcg_fort_wfp,
         fe_mg_fort_wfp = iron_mg+fe_mg_fort_wfp,
         thia_mg_fort_wfp = vitb1_mg+thia_mg_fort_wfp,
         vitb12_mcg_fort_wfp = vitaminb12_in_mcg+vitb12_mcg_fort_wfp)



## TO DO ##
## add in wheat flour


# create prevalences at regional level

aggregated_inadequacy <- function(group){
  # print({{group}})
  
  hh_mn_intake_fort %>%
    mutate(
      folate_inad = ifelse(folate_ug  < 180, 1,0),
      vitb12_inad = ifelse(vitaminb12_in_mcg< 2, 1,0),
      thia_inad = ifelse(vitb1_mg<0.9, 1,0),

      folate_inad_fort = ifelse(folate_mcg_fort < 180, 1,0),
      vitb12_inad_fort = ifelse(vitb12_mcg_fort<2,1,0),
      thia_inad_fort = ifelse(thia_mg_fort<0.9, 1,0),

      folate_inad_fort_wfp = ifelse(folate_mcg_fort_wfp < 180, 1,0),
      vitb12_inad_fort_wfp = ifelse(vitb12_mcg_fort_wfp<2,1,0),
      thia_inad_fort_wfp = ifelse(thia_mg_fort_wfp<0.9, 1,0),

      #mar
      fol_nar = ifelse(folate_ug < 180, folate_ug/180,1),
      vb12_nar = ifelse(vitaminb12_in_mcg < 2, vitaminb12_in_mcg/2,1),
      iron_nar = ifelse(iron_mg < 15, folate_ug/15,1),
    ) %>%
    left_join(level01 %>%
                mutate(multiplier = as.numeric(multiplier)), by = "common_id") %>%
    as_survey_design(ids = common_id, strata = sector, weights = multiplier) %>%
    srvyr::group_by({{group}}) %>%
    summarise(
      across(contains("inad"),~survey_mean(. == 1, proportion = T, na.rm = T)*100)
    ) %>%
    left_join(
      fe_full_prob(hh_mn_intake %>%
                     rename(ai_afe = iron_mg) %>%
                     left_join(level01, by="common_id"), group1 = {{group}}, bio_avail = 10) %>%
        rename(nss_region = subpopulation,
               fe_inad = prev_inad)
  
    ) %>%
    left_join(
      fe_full_prob(hh_mn_intake_fort %>%
                     rename(ai_afe = fe_mg_fort) %>%
                     left_join(level01, by="common_id"), group1 = {{group}}, bio_avail = 10) %>%
        rename(nss_region = subpopulation,
               fe_inad_fort = prev_inad)
    ) %>%
    left_join(
      fe_full_prob(hh_mn_intake_fort %>%
                     rename(ai_afe = fe_mg_fort_wfp) %>%
                     left_join(level01, by="common_id"), group1 = {{group}}, bio_avail = 10) %>%
        rename(nss_region = subpopulation,
               fe_inad_fort_wfp = prev_inad)
    )
}


# nss_region inadequacy
nss_region_inad <- aggregated_inadequacy(`nss_region`)


# state differences
state_inad <- aggregated_inadequacy(`state`)
  
  
# create an output csv

state_inadequacy <- state_inad %>% 
  mutate(state = case_when(
    state == "02" ~ "Himachal Pradesh",
    state == "03" ~ "Punjab",
    state == "06" ~ "Haryana",
    state == "07" ~ "Delhi",
    state == "08" ~ "Rajasthan",
    state == "09" ~ "Uttar Pradesh",
    state == "10" ~ "Bihar",
    state == "19" ~ "West Bengal",
    state == "20" ~ "Jharkhand",
    state == "21" ~ "Odisha",
    state == "22" ~ "Chhattisgarh",
    state == "23" ~ "Madhya Pradesh",
    state == "28" ~  "Andhra Pradesh",
    state == "34" ~ "Puducherry",
    state == "36" ~ "Telangana"
  )) %>% 
  select(state,
         ends_with("_inad"),
         ends_with("_fort"),
         ends_with("_wfp")) 


write.csv(state_inadequacy, "state_inadequacy.csv")


################################################################################
# Inadequacy Maps

#create a shapefile at nss_region level
nss_region_inad_sp <- nss_region_inad %>% 
  mutate(nss_region = as.numeric(nss_region)) %>% 
  left_join(nss_region_shapefile, by= 'nss_region') %>% 
  st_as_sf()



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
    tm_legend(show =F)
}

# folate

folate_map <- inadequacy_map("folate_inad", "Folate base")
folate_fort_map <-inadequacy_map("folate_inad_fort", "Folate fortified - current specs")
folate_fort_map_wfp <- inadequacy_map("folate_inad_fort_wfp", "Folate fortified - WFP specs")
  

#vb12

vitb12_map <- inadequacy_map("vitb12_inad", "Vitamin B12 base")
vitb12_fort_map <- inadequacy_map("vitb12_inad_fort", "Vitamin B12 fortified - current specs")
vitb12_fort_map_wfp <- inadequacy_map("vitb12_inad_fort_wfp", "Vitamin B12 fortified - WFP specs")
 
#iron
iron_map <- inadequacy_map("fe_inad", "Iron base")
iron_fort_map <- inadequacy_map("fe_inad_fort", "Iron fortified - current specs")
iron_fort_map_wfp <-inadequacy_map("fe_inad_fort_wfp", "Iron fortified - WFP specs")
  
# thiamin
thia_map <-inadequacy_map("thia_inad", "Thiamin base")
thia_fort_map <- inadequacy_map("thia_inad_fort", "Thiamin fortified - current specs")
thia_fort_map_wfp <- inadequacy_map("thia_inad_fort_wfp", "Thiamin fortified - WFP specs")
 


mimi_ind <- ind_state %>% 
  mutate(index = case_when(
    State_Name == "Chhattishgarh" ~ 2,
    State_Name == "Bihar" ~ 2,
    State_Name == "Uttar Pradesh" ~2,
    State_Name== "Himachal Pradesh"~ 1,
    State_Name == "Punjab"~1,
    State_Name == "Haryana"~1,
    State_Name == "Delhi"~1,
    State_Name =="Rajasthan"~1,
    State_Name ==  "West Bengal"~1,
    State_Name == "Jharkhand"~1,
    State_Name == "Odisha"~1,
    State_Name ==  "Madhya Pradesh"~1,
    State_Name ==   "Andhra Pradesh"~1,
    State_Name ==  "Puducherry"~1,
    State_Name ==  "Telengana"~1,
    .default = 0
  )) %>% 
  mutate(State_Name = ifelse(State_Name== "Chhattishgarh", "Chhattisgarh", State_Name))

tm_shape(mimi_ind) +
  tm_fill("index")+
  tm_text("State_Name", size = 0.8, remove.overlap = TRUE)+
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)


################################################################################
# reach

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



######## sep quintile

hh_expenditure <- 
  data_list$level15 %>% 
  filter(common_id %in% level01$common_id) %>% 
  mutate(hh_size = as.numeric(hh_size)) %>% 
  group_by(common_id,hh_size ) %>% 
  summarise(total = sum(as.numeric(hh_usual_monthly_consumption),na.rm = T)
  ) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(per_capita_expenditure = total/hh_size) %>% 
  left_join(level01, by= 'common_id') %>%
  group_by(sector) %>% 
  mutate(res_quintile =
           case_when(
             per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[2]]~
               "1",
             per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[3]]~
               "2",
             per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[4]]~
               "3",
             per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[5]]~
               "4",
             per_capita_expenditure<=quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[6]]~
               "5",
           )) %>% 
  select(common_id,hh_size, total,per_capita_expenditure, sector,res_quintile)



#res
res_quintile_db <- hh_mn_intake_fort %>% 
  mutate(
    folate_inad = ifelse(folate_ug < 180, 1,0),
    vitb12_inad = ifelse(vitaminb12_in_mcg< 2, 1,0),
    thia_inad = ifelse(vitb1_mg<0.9, 1,0),
    
    folate_inad_fort = ifelse(folate_ug_fort < 180, 1,0),
    vitb12_inad_fort = ifelse(vitb12_mcg_fort<2,1,0),
    thia_inad_fort = ifelse(thia_mg_fort<0.9, 1,0),
    
    folate_inad_fort_wfp = ifelse(folate_ug_fort_wfp < 180, 1,0),
    vitb12_inad_fort_wfp = ifelse(vitb12_mcg_fort_wfp<2,1,0),
    thia_inad_fort_wfp = ifelse(thia_mg_fort_wfp<0.9, 1,0),
  ) %>% 
  left_join(level01 %>% 
              mutate(multiplier = as.numeric(multiplier)), by = "common_id") %>% 
  left_join(hh_expenditure ) %>% 
  as_survey_design(ids = common_id, strata = sector, weights = multiplier) %>% 
  srvyr::group_by(sector, res_quintile) %>% 
  summarise(
    folate_inad = srvyr::survey_mean(folate_inad == 1, proportion=TRUE,na.rm = T)*100,
    vitb12_inad = srvyr::survey_mean(vitb12_inad == 1, proportion=TRUE,na.rm = T)*100,
    thia_inad = srvyr::survey_mean(thia_inad == 1, proportion=TRUE,na.rm = T)*100,
    
    folate_inad_fort = srvyr::survey_mean(folate_inad_fort == 1, proportion=TRUE, na.rm = T)*100,
    vitb12_inad_fort = srvyr::survey_mean(vitb12_inad_fort == 1, proportion=TRUE,na.rm = T)*100,
    thia_inad_fort = srvyr::survey_mean(thia_inad_fort == 1, proportion=TRUE,na.rm = T)*100,
    
    folate_inad_fort_wfp = srvyr::survey_mean(folate_inad_fort_wfp == 1, proportion=TRUE, na.rm = T)*100,
    vitb12_inad_fort_wfp = srvyr::survey_mean(vitb12_inad_fort_wfp == 1, proportion=TRUE,na.rm = T)*100,
    thia_inad_fort_wfp = srvyr::survey_mean(thia_inad_fort_wfp == 1, proportion=TRUE,na.rm = T)*100
  )%>% 
  mutate(res_quintile = paste0(sector,"_",res_quintile)) %>% 
  left_join(
    fe_full_prob(hh_mn_intake %>% 
                   rename(ai_afe = iron_mg) %>% 
                   left_join(level01, by="common_id") %>% 
                   left_join(hh_expenditure), group1 = sector, group2 = res_quintile, bio_avail = 10) %>% 
      rename(res_quintile = subpopulation,
             fe_inad = prev_inad),
    by = 'res_quintile'
  ) %>% 
  left_join(
    fe_full_prob(hh_mn_intake_fort %>% 
                   rename(ai_afe = iron_mg_fort) %>% 
                   left_join(level01, by="common_id")%>% 
                   left_join(hh_expenditure), group1 = sector, group2 = res_quintile, bio_avail = 10)%>% 
      rename(res_quintile = subpopulation,
             fe_inad_fort = prev_inad),
    by = 'res_quintile'
  )%>% 
  left_join(
    fe_full_prob(hh_mn_intake_fort %>% 
                   rename(ai_afe = iron_mg_fort_wfp) %>% 
                   left_join(level01, by="common_id")%>% 
                   left_join(hh_expenditure), group1 = sector, group2 = res_quintile, bio_avail = 10)%>% 
      rename(res_quintile = subpopulation,
             fe_inad_fort = prev_inad),
    by = 'res_quintile'
  )

# dumbell plots


min_max_iron <- res_quintile_db %>% 
  group_by(sector, res_quintile) %>%
  summarise(low = min(fe_inad, fe_inad_fort.x),
            hi = max(fe_inad, fe_inad_fort.x))

res_quintile_db_iron <-  res_quintile_db %>% 
  tidyr::pivot_longer(cols = c(fe_inad, fe_inad_fort.x)) %>% 
  select(res_quintile, name, value)

res_quintile_db_iron <- res_quintile_db_iron %>% 
  left_join(min_max_iron)


res_quintile_db_iron %>% 
  ungroup() %>% 
  mutate(res_quintile = factor(case_when(
    res_quintile == "1_1" ~ "Rural Poorest",
    res_quintile == "1_2" ~ "Rural Poor",
    res_quintile == "1_3"~ "Rural Middle",
    res_quintile == "1_4" ~ "Rural Rich",
    res_quintile == "1_5" ~ "Rural Richest",
    res_quintile == "2_1" ~ "Urban Poorest",
    res_quintile == "2_2" ~ "Urban Poor",
    res_quintile == "2_3" ~ "Urban Middle",
    res_quintile == "2_4" ~ "Urban Rich",
    res_quintile == "2_5" ~ "Urban Richest"
  ), levels = c("Rural Poorest","Rural Poor","Rural Middle","Rural Rich","Rural Richest",
                "Urban Poorest", "Urban Poor","Urban Middle","Urban Rich","Urban Richest")),
  scenario = ifelse(name== "fe_inad", "Base", "Current standards"),
  Residence = ifelse(sector == 1, "Rural", "Urban")
  ) %>% 
  
  ggplot()+
  geom_pointrange(aes(x = res_quintile, y =value, ymin = low, ymax = hi, color = scenario, shape = Residence)) +
  theme_bw()+
  coord_flip(ylim = c(0, 100)) + 
  ylim(0,100)+
  theme(legend.position = "bottom") +
  labs(y = "Prevelence of inadequacy",
       x = ""
  ) +
  labs(
    title = "Iron"
  )

#


min_max_fol <- res_quintile_db %>% 
  group_by(sector, res_quintile) %>%
  summarise(low = min(folate_inad, folate_inad_fort),
            hi = max(folate_inad, folate_inad_fort))

res_quintile_db_fol <-  res_quintile_db %>% 
  tidyr::pivot_longer(cols = c(folate_inad, folate_inad_fort)) %>% 
  select(res_quintile, name, value)

res_quintile_db_fol <- res_quintile_db_fol %>% 
  left_join(min_max)


res_quintile_db_fol %>% 
  ungroup() %>% 
  mutate(res_quintile = factor(case_when(
    res_quintile == "1_1" ~ "Rural Poorest",
    res_quintile == "1_2" ~ "Rural Poor",
    res_quintile == "1_3"~ "Rural Middle",
    res_quintile == "1_4" ~ "Rural Rich",
    res_quintile == "1_5" ~ "Rural Richest",
    res_quintile == "2_1" ~ "Urban Poorest",
    res_quintile == "2_2" ~ "Urban Poor",
    res_quintile == "2_3" ~ "Urban Middle",
    res_quintile == "2_4" ~ "Urban Rich",
    res_quintile == "2_5" ~ "Urban Richest"
  ), levels = c("Rural Poorest","Rural Poor","Rural Middle","Rural Rich","Rural Richest",
                "Urban Poorest", "Urban Poor","Urban Middle","Urban Rich","Urban Richest")),
  scenario = ifelse(name== "folate_inad", "Base", "Current standards"),
  Residence = ifelse(sector == 1, "Rural", "Urban")
  ) %>% 
  
  ggplot()+
  geom_pointrange(aes(x = res_quintile, y =value, ymin = low, ymax = hi, color = scenario, shape = Residence))+
  theme_bw()+
  coord_flip(ylim = c(0, 100)) + 
  ylim(0,100)+
  theme(legend.position = "bottom") +
  labs(y = "Prevelence of inadequacy",
       x = ""
  ) +
  labs(
    title = "Folate"
  )



################################################################################

# stacked bar chart of consumption patterns of rice
level05_30day %>% 
  filter(Item_Code %in% c(102,101,61)) %>% 
  mutate(rice_type = case_when(
    Item_Code == 101 ~ "PDS purchased",
    Item_Code == 61 ~ "Free",
    Item_Code == 102 ~ "Other sources"
  )) %>% 
  left_join(level01 %>% 
              select(common_id, state,sector, multiplier )) %>% 
  group_by(common_id, state, sector, multiplier) %>% 
  pivot_wider(names_from = rice_type, values_from = Total_Consumption_Quantity) %>% 
  select(common_id, state, `PDS purchased`, `Other sources`, Free, sector, multiplier) %>% 
  summarise(
    across(everything(),
           ~sum(.,na.rm = T)
    )) %>% 
  ungroup() %>% 
  mutate(multiplier = as.numeric(multiplier)) %>% 
  as_survey_design(ids = common_id, strata = sector, weights = multiplier) %>% 
  group_by(state) %>% 
  summarise(across(-c(common_id,sector,multiplier),
                   ~survey_mean(.))) %>% 
  pivot_longer(cols = c(2,4,6)) %>% 
  
  mutate(state = factor(case_when(
    state == "02" ~ "Himachal Pradesh",
    state == "03" ~ "Punjab",
    state == "06" ~ "Haryana",
    state == "07" ~ "Delhi",
    state == "08" ~ "Rajasthan",
    state == "09" ~ "Uttar Pradesh",
    state == "10" ~ "Bihar",
    state == "19" ~ "West Bengal",
    state == "20" ~ "Jharkhand",
    state == "21" ~ "Odisha",
    state == "22" ~ "Chhattisgarh",
    state == "23" ~ "Madhya Pradesh",
    state == "28" ~  "Andhra Pradesh",
    state == "34" ~ "Puducherry",
    state == "36" ~ "Telangana"
  ), levels = c("Delhi","Haryana","Himachal Pradesh","Punjab","Rajasthan",# Northern
                "Chhattisgarh", "Madhya Pradesh", "Uttar Pradesh",
                "Bihar","Jharkhand","Odisha","West Bengal",#eastern
                "Andhra Pradesh", "Puducherry","Telangana"))#south
  ) %>% 
  ggplot(aes(fill = factor(name, levels = c("Other sources","PDS purchased","Free")), 
             y = value, 
             
             x = state
             
             
  ))+
  geom_bar(position = "stack", stat = "identity")+
  scale_fill_manual(values =my_colors, )+
  theme_ipsum()+theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  labs(fill = "Source of rice consumed",
       y = "Consumption of rice (g/d/afe)")


my_colors <- c( "#90E0EF","#f0bd7e", "#ec8013")


# tolerable UL ######################################################


wfp_ul <- hh_mn_intake_fort %>% 
  ggplot(aes(x = iron_mg_fort_wfp))+
  geom_histogram()+
  geom_vline(aes(xintercept = 45), color = 'red')+
  geom_text(x = 53, y = 15000, label = "Tolerable upper limit")+
  # xlim(0,75)+
  theme_bw()+
  xlab("Iron intake (mg)") + 
  labs(title = "Iron intake",
       subtitle  = "(fortified to internationally recommended standards)",
       caption = "0.1% of households above UL")



