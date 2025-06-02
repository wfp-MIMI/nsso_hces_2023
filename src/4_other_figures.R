#########################################
#      HCES 2022-23 INDIA               #
#          more figures              #
#########################################


# Author: Gabriel Battcock
# Created: 
# Last updated: 28 May 2025

## Load packages ###############################################################

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

## Load data ###################################################################

# rm(list = ls())

# set paths
figure_path <- "figures/"
raw_path <- "data/raw/"
processed_path <- "data/processed/"

###

# shape files
ind_state <- sf::st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Workstream 2/Nutrition analysis/shapefiles/IND/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp")
# ind_admin2 <- st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Nutrition analysis/shapefiles/ind_lss1819_adm2.shp")
nss_region_shapefile <- sf::st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/ind_nss2223_nssregion.shp")

# food consumption and adequacy data
food_consumption_daily_afe <- readRDS(paste0(processed_path,"ind_nss2223_food_consumption.rds"))
hh_mn_intake <- readRDS(paste0(processed_path,"ind_nss2223_base_case.rds"))
nss_region_inad_rice <- readRDS(paste0(processed_path,"nss_region_inad_rice.rds" ))
nss_region_inad_wf <- readRDS(paste0(processed_path,"nss_region_inad_rice.rds" ))
food_consumption_daily_afe <- readRDS(paste0(processed_path,"ind_nss2223_food_consumption.rds"))

# food group data
ind_nss_hdds <- read_xlsx(paste0(raw_path, "ind_nss2223_hdds.xlsx"),sheet = 1)

ind_202223_fct <-  read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/nsso_202223_fct.xlsx")

## Descriptive data ############################################################

# sep quintile


hh_expenditure <- 
  data_list$level15 %>% 
  # filter(common_id %in% level01$common_id) %>% 
  mutate(hh_size = as.numeric(hh_size)) %>% 
  group_by(common_id,hh_size ) %>% 
  summarise(total = sum(as.numeric(hh_usual_monthly_consumption),na.rm = T)
  ) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(per_capita_expenditure = total/hh_size) %>% 
  left_join(data_list$level01, by= 'common_id') %>%
  # group_by(sector) %>% 
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
  select(common_id,hh_size, total,per_capita_expenditure, sector,res_quintile, fsu_serial_no ) %>% 
  left_join(data_list$level01 %>% select(common_id, state,multiplier ) %>% mutate(multiplier = as.numeric(multiplier)), by= 'common_id')

#calculate proportion in lowest quintile 
state_qutintile <- hh_expenditure%>% 
  as_survey_design(ids = common_id, strata = sector, weights = multiplier) %>% 
  group_by(state) %>% 
  summarise(
    prop_lowest_quintile = round(survey_mean(res_quintile == "1", proportion = TRUE, na.rm = T),2)*100
  )


# people receiving PDS as proportion
pds_totals <- data_list$level04 %>% 
  left_join(hh_expenditure %>% select(common_id, sector,state,fsu_serial_no ), by= 'common_id') %>% 
  mutate(multiplier = as.numeric(multiplier),
         fsu_serial_no = as.numeric(fsu_serial_no)) %>% 
  as_survey_design(ids = fsu_serial_no , weights = multiplier) %>%
                     # multiplier) %>% 
  group_by(sector) %>%
  summarise(
    pds_total = round(survey_mean(hh_used_ration_card_30days == "1", proportion = T, na.rm = TRUE),2)*100,
    pds_rice = round(survey_mean(ration_card_item_rice  == "1", proportion = T, na.rm = TRUE),2)*100,
    pds_wf = round(survey_mean(ration_card_item_wheat   == "1", proportion = T, na.rm = TRUE),2)*100,
    total = sum(ifelse(hh_used_ration_card_30days=="1",1,0))
  )

# proportion using a ration card
data_list$level04 %>% 
  left_join(hh_expenditure %>% select(common_id, sector,state,fsu_serial_no ), by= 'common_id') %>% 
  group_by(sector) %>% 
  summarise(total = sum(ifelse(hh_used_ration_card_30days=="1",1,0)))



# create df of summary state sep and pds use

state_summary <- state_qutintile %>% 
  select(state, prop_lowest_quintile) %>% 
  left_join(pds_totals, by= "state") %>% 
  select(state,prop_lowest_quintile,pds_total, pds_rice, pds_wf)

# national level
 data_list$level04 %>% 
    left_join(hh_expenditure %>% select(common_id, sector, state), by= 'common_id') %>% 
  mutate(multiplier = as.numeric(multiplier)) %>% 
  as_survey_design(ids = common_id, strata = sector, weights = multiplier) %>% 
  group_by(state) %>%
  summarise(
    pds_total = round(survey_mean(hh_used_ration_card_30days == "1", proportion = T, na.rm = TRUE),2)*100,
    pds_rice = round(survey_mean(ration_card_item_rice  == "1", proportion = T, na.rm = TRUE),2)*100,
    pds_wf = round(survey_mean(ration_card_item_wheat   == "1", proportion = T, na.rm = TRUE),2)*100
  )


write.csv(state_summary, "state_summary.csv")

 ### proportion plots ###########################################################

### proportion plots

food_group_cols <- colnames(ind_nss_hdds %>% select(-c(item_code,item_name)))

# make into a data 
ind_nss_hdds<- ind_nss_hdds %>% 
  pivot_longer(cols = -c(item_code,item_name)) %>% 
  filter(value == 1) %>% 
  select(-value) %>% 
  rename(food_group = name, 
         Item_Code = item_code)

# data clean
food_group_full <- food_consumption_daily_afe %>% 
  inner_join(ind_202223_fct , by=c("Item_Code" ="item_code" )) %>% 
  mutate(quantity_100g = Total_Consumption_Quantity/100,
         across(c(energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg ),
                ~.x*quantity_100g)
  ) %>% 
  select(c(common_id,Item_Code,item_name, energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg))  %>% 
  left_join(hh_expenditure) %>% 
  left_join(ind_nss_hdds) 


## 
## summarise micronutrient contributions from food groups nationally
national_foodgroup_average <- food_group_full %>% 
  group_by(common_id,state, food_group) %>% 
  summarise(
    across(
      c(energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg),
      ~sum(., na.rm = T)
      
    )
  ) %>% 
  ungroup() %>% 
  group_by(food_group) %>% 
  summarise(
    across(
      c(energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg),
      ~mean(.)
    
    )
  )


## summarise micronutrient contributions from food groups at state level to see regional differences
state_foodggroup_average <-  food_group_full %>% 
  group_by(common_id,state, food_group) %>% 
  summarise(
    across(
      c(energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg),
      ~sum(., na.rm = T)
      
    )
  ) %>% 
  ungroup() %>% 
  group_by(food_group,state) %>% 
  summarise(
    across(
      c(energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg),
      ~mean(.)
      
    )
  )

################################################################################


# create list of micronutrient names
micronutrient <- c(colnames(state_foodggroup_average[4:7]),colnames(state_foodggroup_average[9:11]))
micronutrient <- data.frame(micronutrient = micronutrient,
                            name = c("Folate", "Iron", " Vitamin B12",
                                     "Thiamin", "Niacin", "Vitamin B6",
                                     "Zinc"))

state_prop_boxes <- function(state_num){
  # function reads in a state number and produces proportional box-plots
  # 
  mn_fg_plots <- list()
for(item in micronutrient){
  print(item)
  print({{state_num}})
  p1 <-  state_foodggroup_average %>%
    
    filter(state == state_num & !is.na(food_group)) %>%
    ggplot(aes(area = !!sym(item$micronutrient),
               fill = stringr::str_to_title(
                 str_replace_all(food_group, "_", " and ")   ),
               label =
                 stringr::str_to_title(
                   str_replace_all(food_group, "_", " and ")         )
   )) +
    geom_treemap() +
    geom_treemap_text( colour = "darkblue", place = "topleft", alpha = 0.6,
                       grow = FALSE,min.size = 6)+
    labs(title = item$name)+
    scale_fill_brewer(palette = "Set3")+
    # guides(fill=guide_legend())+
    theme(legend.position="bottom",
          legend.spacing.x = unit(0, 'cm')
          )+
    guides(fill = guide_legend(title="Food group",label.position = "bottom"))
  # theme(legend.direction = "horizontal", legend.position = "bottom")+
  # guides(fill = "none")+
 
  mn_fg_plots[[item]] <- p1
}
  return(mn_fg_plots)
}


state_prop_boxes("32")
national_foodgroup_average <- national_foodgroup_average %>%
  mutate(food_group_clean = str_to_title(str_replace_all(food_group, "_", " and ")))


for (i in seq_len(nrow(micronutrient))) {
  item <- micronutrient[i, ]
  print(item$micronutrient)
  
  p1 <- national_foodgroup_average %>%
    filter(!is.na(food_group)) %>%
    ggplot(aes(
      area = !!sym(item$micronutrient),
      fill = str_to_title(str_replace_all(food_group, "_", " and ")),
      label = str_to_title(str_replace_all(food_group, "_", " and "))
    )) +
    geom_treemap() +
    geom_treemap_text(
      colour = "darkblue",
      place = "topleft",
      alpha = 0.6,
      grow = FALSE,
      min.size = 6
    ) +
    labs(title = item$name) +
    scale_fill_brewer(palette = "Set3") +
    theme(
      legend.position = "bottom",
      legend.spacing.x = unit(0, 'cm')
    ) +
    guides(
      fill = guide_legend(
        title = "Food group",
        label.position = "bottom"
      )
    )
  
  mn_fg_plots[[item$micronutrient]] <- p1
  ggsave(
    filename = paste0(figure_path,"food_group/", item$micronutrient, ".jpg"),
    plot = p1,
    height = 6.5,
    width = 6,
    dpi = 900
  )
}

mn_fg_plots[3]
nat_fg <- ggpubr::ggarrange(plotlist = mn_fg_plots, common.legend = TRUE)

raj_fg <- ggpubr::ggarrange(plotlist = state_prop_boxes("08"), common.legend = T)
up_fg <- ggpubr::ggarrange(plotlist = state_prop_boxes("09"), common.legend = T)
tn_fg <- ggpubr::ggarrange(plotlist = state_prop_boxes("23"), common.legend = T)
ch_fg <- ggpubr::ggarrange(plotlist = state_prop_boxes("22"), common.legend = T)
mz_fg <- ggpubr::ggarrange(plotlist = state_prop_boxes("17"), common.legend = T)
kl_fg <- ggpubr::ggarrange(plotlist = state_prop_boxes("32"), common.legend = T)


raj_fg <- ggpubr::annotate_figure(raj_fg, top = ggpubr::text_grob("Rajasthan", face = "bold", size = 15))
up_fg <- ggpubr::annotate_figure(up_fg, top = ggpubr::text_grob("Uttar Pradesh", face = "bold", size = 15))
tn_fg <- ggpubr::annotate_figure(tn_fg, top = ggpubr::text_grob("Tamil Nadu", face = "bold", size = 15))
ch_fg <- ggpubr::annotate_figure(ch_fg, top = ggpubr::text_grob("Chhattisgarh", face = "bold", size = 15))
mz_fg <- ggpubr::annotate_figure(mz_fg, top = ggpubr::text_grob("Mizoram", face = "bold", size = 15))
nat_fg <- ggpubr::annotate_figure(nat_fg, top = ggpubr::text_grob("National", face = "bold", size = 15))
kl_fg <- ggpubr::annotate_figure(kl_fg, top = ggpubr::text_grob("Kerela", face = "bold", size = 15))


ggsave(paste0(figure_path,"rj_fg.png"), raj_fg, height = 8, width = 6)
ggsave(paste0(figure_path,"up_fg.png"), up_fg, height = 8, width = 6)
ggsave(paste0(figure_path,"tn_fg.png"), tn_fg, height = 8, width = 6)
ggsave(paste0(figure_path,"ch_fg.png"), ch_fg, height = 8, width = 6)
ggsave(paste0(figure_path,"mz_fg.png"), mz_fg, height = 8, width = 6)
ggsave(paste0(figure_path,"food_group/nat_fg.jpg"), nat_fg, height = 8, width = 6, dpi = 900)



food_group_full %>% 
  group_by(common_id, food_group) %>% 
  summarise(
    across(energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg)
    )




x <- 
  hh_expenditure %>% 
  as_survey_design(ids = common_id, weights = multiplier, strata = sector) %>% 
  group_by(state) %>% 
  summarise(
    prop_poorest = survey_mean(res_quintile  == 1, proportion = T, na.rm = T)
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





