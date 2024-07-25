#########################################
#                            #
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
plot(ind_admin2$geometry)

# AFE CALCUTATION ##############################################################

# filter only 6 states
level01 <- data_list$level01 %>% 
  filter(state %in% c("03","06", "08","09", "10", "22"))
# c("02", "03","06","07", "08","09", "10","19","20","21", "22","23","28","34","36")
level02 <- data_list$level02 %>% 
  filter(common_id %in% level01$common_id)


summary(factor(level02$gender))


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
  mutate(Total_Consumption_Quantity = Total_Consumption_Quantity/afe)



# 7 day recall 
conversion_factor <- read_csv("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/conversion_factors.csv")

level05_7day <- level05 %>% 
  filter(!(Item_Code %in% level05_30day$Item_Code)) %>% 
  mutate(Item_Code = as.numeric(Item_Code)) %>% 
  left_join(conversion_factor, by= 'Item_Code') %>% 
  left_join(ind_202223_fct %>% select(item_code, edible_portion), by = c("Item_Code" = "item_code")) %>% 
  mutate(Total_Consumption_Quantity = 
           ifelse(is.na(conversion_factor_to_kg), as.numeric(Total_Consumption_Quantity)*edible_portion, as.numeric(Total_Consumption_Quantity)*conversion_factor_to_kg*edible_portion)) %>% 
  select(-c(item_name, conversion_factor_to_kg, edible_portion)) %>% 
  mutate(Total_Consumption_Quantity = (Total_Consumption_Quantity/7)*1000) %>% 
  left_join(hh_afe, by = "common_id") %>% 
  mutate(Total_Consumption_Quantity = Total_Consumption_Quantity/afe) 
  # set all 

level05_7day %>%  filter(Item_Code == 002)



  

# read in the fct
ind_202223_fct <-  read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/nsso_202223_fct.xlsx")

# check whether drop down items are counted twice (they are)
x <- level05_30day %>% 
  filter(Item_Code == 116)

y = level05_30day %>% 
  filter(common_id == "HCES2022653801030311623025 203121  315")

################################################################################

food_consumption_daily_afe <- level05_7day %>% 
              mutate(Item_Code = as.numeric(Item_Code))



food_consumption_daily_afe %>% 
  inner_join(ind_202223_fct, by=c("Item_Code" ="item_code" )) %>% 
  mutate(quantity_100g = Total_Consumption_Quantity/100)


unmerged <- anti_join(food_consumption_daily_afe, ind_202223_fct, by=c("Item_Code" ="item_code" )) %>% 
  distinct(Item_Code)

# all unmerged are on purpose as they are sub-totals of other food items


hh_mn_intake <- food_consumption_daily_afe %>% 
  inner_join(ind_202223_fct , by=c("Item_Code" ="item_code" )) %>% 
  mutate(quantity_100g = Total_Consumption_Quantity/100,
         energy_kcal = energy_kcal*quantity_100g,
         folate_ug = folate_ug*quantity_100g,
         iron_mg = iron_mg*quantity_100g,
         vitaminb12_in_mcg = vitaminb12_in_mcg*quantity_100g) %>% 
  select(common_id, energy_kcal,folate_ug,iron_mg,vitaminb12_in_mcg) %>% 
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
  geom_histogram()+
  xlim(0,10000)

summary(hh_mn_intake$energy_kcal)


# join to hh state information


nss_region_shp <- level01 %>% 
  distinct(state,nss_region,district) %>% 
  mutate(district = as.numeric(paste0(state, district))) %>% 
  left_join(ind_admin2,by = c('district' = "adm2_code")) %>% 
  group_by(nss_region) %>% 
  mutate(geometry = sf::st_union(geometry)) %>% 
  slice(1)

plot(nss_region_shp$geometry)

hh_mn_intake %>% 
  left_join(level01, by = "common_id") %>% 
  mutate(district = as.numeric(paste0(state, district)))
  # group_by(state,nss_region, district)
  
new_shapefile <- st_read("C:/Users/gabriel.battcock/Downloads/india_adm2_shp/DISTRICT_BOUNDARY.shp")

ind_sahpefile_names <- new_shapefile %>% 
  select(District, STATE,State_LGD, DISTRICT_L) %>% 
  st_drop_geometry() %>% 
  filter(State_LGD == 07)

delhi <- new_shapefile%>% 
  select(District, STATE,State_LGD, DISTRICT_L) %>% 
  filter(State_LGD == 07)
plot(delhi$geometry)

nss2223_shp_dictionary <- read.csv("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/ind_new_shapefile.csv")

nss_region_shapefile <- x <-  level01 %>% 
  distinct(state,nss_region,district) %>% 
  mutate(district = as.numeric(paste0(state, district)),
         state = as.numeric(state),
         nss_region = as.numeric(nss_region)) %>% 
  left_join(nss2223_shp_dictionary, by=c("district" = "adm2_code")) %>% 
  mutate(DISTRICT_L = as.character(DISTRICT_L)) %>% 
  select(state,nss_region,district,State_LGD, DISTRICT_L) %>% 
  left_join(new_shapefile , by= c("State_LGD", "DISTRICT_L")) %>% 
  group_by(nss_region) %>%
  summarise(geometry = sf::st_union(geometry))

plot(nss_region_shapefile$geometry,col = "red")
  
# sf::st_write(nss_region_shapefile, "C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/ind_nss2223_nssregion.shp")
  


  

# write.csv(x, "C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/ind_new_nss_shapefile.csv")


################################################################################
# BASE CASE


nss_region_inad <- hh_mn_intake %>% 
  mutate(
    folate_inad = ifelse(folate_ug < 250, 1,0),
    vitb12_inad = ifelse(vitaminb12_in_mcg< 2, 1,0)
  ) %>% 
  left_join(level01 %>% 
              mutate(multiplier = as.numeric(multiplier)), by = "common_id") %>% 
  as_survey_design(ids = common_id, strata = sector, weights = multiplier) %>% 
  srvyr::group_by(nss_region) %>% 
  summarise(
    folate_inad = srvyr::survey_mean(folate_inad == 1, proportion=TRUE),
    vitb12_inad = srvyr::survey_mean(vitb12_inad == 1, proportion=TRUE)
    
  )

nss_region_inad_sp <- nss_region_inad %>% 
  mutate(nss_region = as.numeric(nss_region)) %>% 
  left_join(nss_region_shapefile, by= 'nss_region') %>% 
  st_as_sf()


# create the map
tm_shape(nss_region_inad_sp) +
  tm_fill(col = "vitb12_inad", style = "cont", breaks = seq(0,1,by=.10),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of inadequacy" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = , frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
  ) +
  tm_borders(col = "black", lwd = 0) +
  # tm_shape(india_adm1) +
  # tm_fill(col = "state") +
  # tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)



        