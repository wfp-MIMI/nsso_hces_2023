#########################################
#      HCES 2022-23 INDIA               #
#########################################


# Author: Gabriel Battcock
# Created: 
# Last updated: 29 April 24
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

#-------------------------------------------------------------------------------

file_list = list.files("data/raw/HCES_2022_23/")
# haven::read_dta("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/level06.dta")
data_list <- lapply(paste0("data/raw/HCES_2022_23/",file_list), haven::read_dta)
names(data_list) <- tools::file_path_sans_ext(file_list)


# ind_state <- sf::st_read("data/raw/shapefiles/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp")
# ind_admin2 <- st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Nutrition analysis/shapefiles/ind_lCss1819_adm2.shp")
nss_region_shapefile <- sf::st_read("data/raw/shapefiles//ind_nss2223_nssregion.shp")

# read in the fct
ind_202223_fct <-  read_xlsx("data/raw/nsso_202223_fct.xlsx")

# AFE CALCUTATION ##############################################################

# filter only 6 states
level01 <- data_list$level01
# 
level02 <- data_list$level02 %>% 
  filter(common_id %in% level01$common_id)

level02 %>% 
  distinct(common_id)

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
        # based on energy from NIN requirements
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

level05 %>% 
  left_join(level01, by = "common_id") %>% 
  group_by(sector) %>% 
  distinct(common_id) %>% 
  summarise(n())
  

# level 5 is 30 day recall  all reported in kg
unique(level05$common_id)

level05_30day <- 
  level05 %>% 
  mutate(Item_Code = as.numeric(Item_Code)) %>% 
  # filter food items that were recorded over 30 days
  filter((Item_Code >100 & Item_Code<160) | 
           (Item_Code>169 &Item_Code<180) | 
           Item_Code %in% c(
             073,074,071,072,061,062,070,001,002,55,56,57,58,59,60,63,64,65,66,67,68
           )) %>% 
  left_join(ind_202223_fct %>% select(item_code, edible_portion), by = c("Item_Code" = "item_code")) %>%
  #convert all values to quantity grammes per day with edible portions
  mutate(Total_Consumption_Quantity_day = (as.numeric(Total_Consumption_Quantity)/30)*1000*edible_portion) %>% 
  select(-edible_portion) %>% 
  left_join(hh_afe, by = "common_id") %>% 
  # adjust for afe
  mutate(Total_Consumption_Quantity_day_afe = Total_Consumption_Quantity_day/afe) 

x <- level05_30day  %>% select(Item_Code, Total_Consumption_Quantity, Total_Consumption_Quantity_day, Total_Consumption_Quantity_day_afe)
x %>% group_by(Item_Code) %>% 
  summarise(quantile(Total_Consumption_Quantity_day_afe, .50))
# 7 day recall 

#read conversion factors
conversion_factor <- read_csv("data/raw/conversion_factors.csv")

level05_7day <- level05 %>% 
  mutate(Item_Code = as.numeric(Item_Code)) %>% 
  # all items that are not in the 30 day list
  filter(!(Item_Code %in% level05_30day$Item_Code)) %>% 
  
  left_join(conversion_factor, by= 'Item_Code') %>% 
  left_join(ind_202223_fct %>% select(item_code, edible_portion), by = c("Item_Code" = "item_code")) %>% 
  mutate(Total_Consumption_Quantity = 
           # convert all to kg with edible potions
           ifelse(is.na(conversion_factor_to_kg), as.numeric(Total_Consumption_Quantity)*edible_portion, as.numeric(Total_Consumption_Quantity)*conversion_factor_to_kg*edible_portion)) %>% 
  select(-c(item_name, conversion_factor_to_kg, edible_portion)) %>% 
  mutate(Total_Consumption_Quantity = (Total_Consumption_Quantity/7)*1000) %>% 
  left_join(hh_afe, by = "common_id") %>% 
  # adjust for afe
  mutate(Total_Consumption_Quantity = Total_Consumption_Quantity/afe) 

# bind the two methods
food_consumption_daily_afe <- 
  bind_rows(level05_7day %>% mutate(Item_Code = as.numeric(Item_Code)),
            level05_30day)

rm(level05_7day, level05_30day, level05)


# filter outliers #############################################################
food_consumption_daily_afe <- food_consumption_daily_afe %>% 
  mutate(log_quantity_g = log(Total_Consumption_Quantity))

#create cut points above which we have outliers
quant_cutpoints <- food_consumption_daily_afe %>% 
  group_by(Item_Code) %>% 
  summarise(
    mean_log = mean(log_quantity_g, na.rm = T),
    sd_log = sd(log_quantity_g, na.rm = T)) %>% 
  mutate(upper_cut = mean_log+2*sd_log) %>% 
  select(Item_Code, upper_cut)

# change 
food_consumption_daily_afe <- food_consumption_daily_afe %>% 
  left_join(quant_cutpoints, by = "Item_Code") %>% 
  mutate(Total_Consumption_Quantity = case_when(
    log_quantity_g>=upper_cut ~ NA_real_,
    TRUE ~ Total_Consumption_Quantity
  )) %>% 
  select(-log_quantity_g,-upper_cut)

food_consumption_daily_afe <- food_consumption_daily_afe %>% 
  group_by(Item_Code) %>% 
  mutate(Total_Consumption_Quantity = ifelse(is.na(Total_Consumption_Quantity),
                                             median(Total_Consumption_Quantity, na.rm =T),
                                             Total_Consumption_Quantity))

rm(quant_cutpoints)



################################################################################

# check unmerged items
unmerged <- anti_join(food_consumption_daily_afe, ind_202223_fct, by=c("Item_Code" ="item_code" )) %>% 
  distinct(Item_Code)

# all unmerged are on purpose as they are sub-totals of other food items
foods_away = c(76,280,281,282,283,284)
food_consumption_daily_afe23 <- read_csv("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/food_consumption23.csv")
food_consumption_daily_afe23 <- food_consumption_daily_afe23 %>% filter(common_id %in% food_consumption_daily_afe$common_id) %>% 
  filter(!Item_Code %in%foods_away)
food_consumption_daily_afe23 %>% filter(Item_Code == 13)


hh_mn_intake23 <- food_consumption_daily_afe23 %>% 
  inner_join(ind_202223_fct , by=c("Item_Code" ="item_code" )) %>% 
  mutate(quantity_100g = Total_Consumption_Quantity/100,
         # calculate mn contributions from each food item 
         across(c(energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg ),
                ~.x*quantity_100g)
  ) %>% 
  select(c(common_id,energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg)) %>% 
  group_by(common_id) %>% 
  summarise(
    across(
      # sum for each hh the mn intake
      everything(),
      ~sum(., na.rm = T)
    )
  )


hh_mn_intake <- food_consumption_daily_afe %>% 
  inner_join(ind_202223_fct , by=c("Item_Code" ="item_code" )) %>% 
  mutate(quantity_100g = Total_Consumption_Quantity/100,
         # calculate mn contributions from each food item 
         across(c(energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg ),
                ~.x*quantity_100g)
  ) %>% 
  select(c(common_id,energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg)) %>% 
  group_by(common_id) %>% 
  summarise(
    across(
      # sum for each hh the mn intake
      everything(),
      ~sum(., na.rm = T)
    )
  )


test <- bind_rows(hh_mn_intake %>% mutate(data = "orginal"), hh_mn_intake23 %>% mutate(data = "new"))

test %>% 
  ggplot(aes(x = energy_kcal, fill = data))+
  geom_histogram(alpha = 0.5, position = 'dodge')

# for lucia# for vita_mcglucia
# ~2% of the total size
# hh_5000 <- hh_mn_intake[sample(nrow(hh_mn_intake),5000),]
# 
# hh_mn_intake_lucia <- food_consumption_daily_afe %>% 
#   filter(common_id %in% hh_5000$common_id) %>% 
#   inner_join(ind_202223_fct , by=c("Item_Code" ="item_code" )) %>% 
#   mutate(quantity_100g = Total_Consumption_Quantity/100,
#          across(c(energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg ),
#                 ~.x*quantity_100g)
#   ) %>% 
#   select(c(common_id, Item_Code, item_name,quantity_100g, energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, zinc_mg, vita_mcg))
# 
# write.csv(hh_mn_intake_lucia, "nsso_subset_lucia.csv")

# look at the energy distribution
hh_mn_intake %>% 
  ggplot(aes(x = energy_kcal))+
  geom_histogram()

summary(hh_mn_intake$energy_kcal)




# sep quintile ################################################################
######## 
# 
# hh_expenditure <- 
#   data_list$level15 %>% 
#   filter(common_id %in% level01$common_id) %>% 
#   mutate(hh_size = as.numeric(hh_size)) %>% 
#   group_by(common_id,hh_size ) %>% 
#   summarise(total = sum(as.numeric(hh_usual_monthly_consumption),na.rm = T)
#   ) %>% 
#   slice(1) %>% 
#   ungroup() %>% 
#   mutate(per_capita_expenditure = total/hh_size) %>% 
#   left_join(level01, by= 'common_id') %>%
#   group_by(sector) %>% 
#   mutate(res_quintile =
#            case_when(
#              per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[2]]~
#                "1",
#              per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[3]]~
#                "2",
#              per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[4]]~
#                "3",
#              per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[5]]~
#                "4",
#              per_capita_expenditure<=quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[6]]~
#                "5",
#            )) %>% 
#   ungroup() %>% 
#   mutate(sep_quintile =
#            case_when(
#              per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[2]]~
#                "1",
#              per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[3]]~
#                "2",
#              per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[4]]~
#                "3",
#              per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[5]]~
#                "4",
#              per_capita_expenditure<=quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[6]]~
#                "5",
#            )) %>% 
#   select(common_id,hh_size, total,per_capita_expenditure, sector,res_quintile,sep_quintile)
# 
# 
# # histogram of total consumption/expenditure
# hh_expenditure %>% 
#   ggplot(aes(x = total))+
#   geom_histogram()+
#   xlim(0,100000)
# 
# hh_expenditure %>% 
#   group_by(sep_quintile) %>% 
#   summarise(
#     n = n()
#   )
# 
# hh_expenditure %>% 
#   summarise(n())
# 
# hh_expenditure %>% 
#   group_by(sector) %>% 
#   summarise(n())
#   
### Save data


# saveRDS(hh_expenditure, file = "ind_nss2223_hh_expenditure.rds")
saveRDS(food_consumption_daily_afe23, file = "data/processed/ind_nss2223_food_consumption.rds")
saveRDS(hh_mn_intake23, file = "ind_nss2223_base_case.rds")
# saveRDS(hh_afe, file = "data/processed/ind_nss2223_afe.rds")

rm(list = ls())

