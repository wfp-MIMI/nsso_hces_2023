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
ind_202223_fct <-  read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/nsso_202223_fct.xlsx",sheet =1)
ind_202223_fct_amino_acids <- read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/nsso_202223_fct.xlsx",sheet =2)


################################################################################

# rename some data columns 
ind_202223_fct_amino_acids <- ind_202223_fct_amino_acids %>% 
  rename(s_num = `S.No in Molly's database`)

mol_db <- mol_db %>% 
  rename(
    s_num= S.No,
    ratio_lys = `Ratio of digestable Lysine - Digestability factor`,
    ratio_tryp = `Ratio of digestable Tryptophan - Digestability factor`,
    ratio_cyst = `Ratio of digestable Cysteine - Digestability factor`,
    ratio_meth = `Ratio of digestable Methionine - Digestability factor`,
    ratio_thre = `Ratio of digestable Threonine - Digestability factor`
  ) %>% 
  select(s_num, ratio_cyst,ratio_lys,ratio_tryp,ratio_meth,ratio_thre)

################################################################################

# match food items to protein
item_level_amino_acids <- food_consumption_daily_afe %>% 
  # head(10000) %>% 
  left_join(ind_202223_fct %>% 
              select(item_code, energy_kcal, protein_g),
            by = c("Item_Code" = "item_code")) %>% 
  left_join(ind_202223_fct_amino_acids, by= c("Item_Code" = "item_code")) %>% 
  left_join(mol_db, by = "s_num") %>% 
  # select(common_id, Item_Code, s_num,lysine_g, protein_g )
  mutate(
    
    # #aa are in g/100g of protein, so need to divide by protein g/100
    protein_g_new = (Total_Consumption_Quantity/100)*protein_g,
    lysine_g = (protein_g_new/100)*lysine_g*ratio_lys,
    tryptophan_g = (protein_g_new/100)*as.numeric(tryptophan_g)*ratio_tryp,
    methionine_g = (protein_g_new/100)*methionine_g*ratio_meth ,
    cystine_g = (protein_g_new/100)*cystine_g*ratio_cyst ,
    threonine_g = (protein_g_new/100)*threonine_g*ratio_thre,
    energy_kcal = (Total_Consumption_Quantity/100)*energy_kcal
  ) 

  

# calcalate household totals of amino acids

household_amino_acids <- item_level_amino_acids %>% 
  group_by(common_id) %>% 
  summarise(
    across(
      c(lysine_g,tryptophan_g,methionine_g,cystine_g,threonine_g,energy_kcal, protein_g),
      ~sum(., na.rm = TRUE)
    )
  ) %>% 
  left_join(hh_expenditure %>% 
              select(common_id, sector, res_quintile, sep_quintile),
            by = 'common_id') %>% 
  left_join(level01 %>% 
              select(common_id, fsu_serial_no,state,nss_region,multiplier),
            by = 'common_id') %>% 
  mutate(multiplier = as.numeric(multiplier))
  






# create intake at an admin level
mean_aa_intake <- function(...){
  ind_aa_intake <- household_amino_acids
  
  x <- ind_aa_intake %>% 
   
    as_survey_design(
      ids = fsu_serial_no ,
      
      weights = multiplier
    ) %>% 
    srvyr::group_by(
      # aggregate by who 
      ...
    ) %>% 
    srvyr::summarise(
      across(
        c(protein_g,lysine_g, tryptophan_g,cystine_g,methionine_g,threonine_g),
        ~survey_mean(.x, na.rm = T)
      )
    )
  return(x)
}

sep_quintile_intake_aa <- median_aa_intake(sep_quintile) %>% 
  filter(!is.na(sep_quintile))

res_quintile_intake_aa <- median_aa_intake(res_quintile)  %>% 
  filter(!is.na(res_quintile)) %>% 
  bind_rows(
    median_aa_intake(sector) %>% 
      mutate(res_quintile = 0)
  ) %>% mutate(res_quintile = case_when(
    res_quintile == 0 ~ "National",
    res_quintile == 1 ~ "Poorest",
    res_quintile == 2 ~ "Poor",
    res_quintile == 3 ~ "Middle",
    res_quintile == 4 ~ "Rich",
    res_quintile == 5 ~ "Richest"
  ))

pop_intake_aa <- mean_aa_intake()

nss_region_intake_aa <-  mean_aa_intake(nss_region)

# prevalence of inadequacy -----------------------------------------------------

# RDA 
inad_aa_intake <- function(...){
  
  ind_aa_intake <- household_amino_acids
  
  aa_rda <- data.frame(
    amino_acid = c("lysine", "tryptophan","cystine","methionine", "threonine", "protein"),
    value_g_per_kg = c(0.030,0.004,0.015,0.015,0.015, 0.66)#WHO recommendations - https://iris.who.int/bitstream/handle/10665/43411/WHO_TRS_935_eng.pdf
  )
  
  weight_kg = 55 # ASSUMPTION - Based on average body weight of women (EAR india)
  
  aa_rda <- aa_rda %>% 
    mutate(rda = value_g_per_kg*55)
  
  aa_intake_inad <- ind_aa_intake %>% 
    mutate(
      lys_rda = 1.65, 
      tryp_rda = 0.22,
      cyst_rda = 0.825,
      meth_rda = 0.825,
      thre_rda = 0.825,
      prot_rda = 36.30,
      lys_inad = ifelse(lysine_g<lys_rda, 1,0),
      tryp_inad = ifelse(tryptophan_g<tryp_rda, 1,0),
      cyst_inad = ifelse(cystine_g<cyst_rda,1,0),
      meth_inad = ifelse(methionine_g<meth_rda,1,0),
      thre_inad = ifelse(threonine_g<thre_rda,1,0),
      protein_inad = ifelse(protein_g<prot_rda, 1,0),
      prot_aas = protein_g/prot_rda,
      lys_aas = lysine_g/lys_rda,
      tryp_aas = tryptophan_g/tryp_rda,
      cyst_aas = cystine_g/cyst_rda,
      meth_aas = methionine_g/meth_rda,
      thre_aas = threonine_g/thre_rda
    ) %>%
  
    as_survey_design(
      ids = fsu_serial_no ,
      
      weights = multiplier
    ) %>% 
    srvyr::group_by(...) %>% 
    srvyr::summarise(
      lys_inad = srvyr::survey_mean(lys_inad == 1, proportion = TRUE, na.rm = TRUE)*100,
      tryp_inad = srvyr::survey_mean(tryp_inad == 1, proportion = TRUE, na.rm = TRUE)*100,
      cyst_inad = srvyr::survey_mean(cyst_inad == 1, proportion = TRUE,na.rm = TRUE )*100,
      meth_inad = srvyr::survey_mean(meth_inad == 1, proportion = TRUE, na.rm = TRUE)*100,
      thre_inad = srvyr::survey_mean(thre_inad == 1,proportion = TRUE, na.rm = TRUE)*100,
      protein_inad = srvyr::survey_mean(protein_inad == 1, proportion = TRUE, na.rm = TRUE)*100,
      lys_aas = srvyr::survey_mean(lys_aas,na.rm = TRUE),
      tryp_aas = srvyr::survey_mean(tryp_aas,na.rm = TRUE),
      cyst_aas = srvyr::survey_mean(cyst_aas,na.rm = TRUE),
      meth_aas = srvyr::survey_mean(meth_aas,na.rm = TRUE),
      thre_aas = srvyr::survey_mean(thre_aas,na.rm = TRUE),
  
    )
  return(aa_intake_inad)
}

# calculations of sub-population

pop_inad <- inad_aa_intake()

res_inad <- inad_aa_intake(sector)
res_quin_inad <- inad_aa_intake(res_quintile)
state_inad <- inad_aa_intake(state)

nss_region_aa_inad <- inad_aa_intake(nss_region)

aa_all <- state_inad %>% 
  rename(sub_pop = state) %>% 
  bind_rows(pop_inad %>% 
              mutate(sub_pop = "National")) %>% 
  bind_rows(res_inad %>% rename(sub_pop = sector)) %>% 
  bind_rows(res_quin_inad %>% 
              rename(sub_pop = res_quintile))

write.csv(res_inad, "protein/aa_score.csv")

## create maps 

nss_region_aa_inad_sp <- nss_region_aa_inad %>% 
  mutate(nss_region = as.numeric(nss_region)) %>% 
  left_join(nss_region_shapefile, by = "nss_region") %>% 
  st_as_sf()


inadequacy_map <- function(amino_acid,
                           title = ""
){
  
  # creates a map of risk of inadequate intake for chosen mn and scenario
  # without a legend
  # tm_shape(ind_state) +
  #   tm_borders(col= 'white')+
  tm_shape(ind_state) +
    tm_fill(col = "grey77") +
    tm_shape(nss_region_aa_inad_sp) +
    tm_fill(col = {{amino_acid}}, style = "cont", breaks = seq(0,100,by=10),
            # palette = (wesanderson::wes_palette("Zissou1Continuous")),
            title = "Risk of inadequacy" ,
            legend.is.portrait = FALSE
    ) +
    tm_layout(main.title = {{title}} , frame = F,
              main.title.size = 0.8,
              legend.outside.position = "bottom",
              legend.outside.size = 0.35
    ) +
    # tm_borders(col = "black", lwd = 0.2) +
    tm_shape(ind_state) +
    # tm_text("State_Name", size = 0.6, remove.overlap = TRUE)+
    # tm_fill(col = "state") +
    tm_borders(col = "black", lwd = 1.5)+
    tm_legend(show = F)

}

inadequacy_map('lys_inad', "Lysine")
inadequacy_map('tryp_inad', "Tryptophan")
inadequacy_map('cyst_inad', "Cysteine")
inadequacy_map('meth_inad', "Methionine")
inadequacy_map('thre_inad', "Threonine")

# amino acid score

aas_map <- function(amino_acid,
                    title = ""
){
  
  # creates a map of risk of inadequate intake for chosen mn and scenario
  # without a legend
  # tm_shape(ind_state) +
  #   tm_borders(col= 'white')+
  tm_shape(ind_state) +
    tm_fill(col = "grey77") +
    tm_shape(nss_region_aa_inad_sp) +
    tm_fill(col = {{amino_acid}}, style = "cont", breaks = seq(0,2,by=0.2),
            # palette = (wesanderson::wes_palette("Zissou1Continuous")),
            palette = 'RdYlBu',
            title = "Risk of inadequacy" ,
            legend.is.portrait = FALSE
    ) +
    tm_layout(main.title = {{title}} , frame = F,
              main.title.size = 0.8,
              legend.outside.position = "bottom",
              legend.outside.size = 0.35
    ) +
    # tm_borders(col = "black", lwd = 0.2) +
    tm_shape(ind_state) +
    # tm_text("State_Name", size = 0.6, remove.overlap = TRUE)+
    # tm_fill(col = "state") +
    tm_borders(col = "black", lwd = 1.5)+
    tm_legend(show = F)
  
}

aas_map('lys_aas', "Lysine")
aas_map('tryp_aas', "Tryptophan")
aas_map('cyst_aas', "Cysteine")
aas_map('meth_aas', "Methionine")
aas_map('thre_aas', "Threonine")


tm_shape(ind_state) +
  tm_fill(col = "grey77") +
  tm_shape(nss_region_aa_inad_sp) +
  tm_fill(col = 'lys_aas', style = "cont", breaks = seq(0,2,by=0.2),
          # palette = (wesanderson::wes_palette("Zissou1Continuous")),
          palette = 'RdYlBu',
          title = "Amino acid score" ,
          legend.is.portrait = TRUE
  ) +
  tm_layout(legend.only = TRUE,
            legend.text.size = 1.5,         # Increase the size of legend text
            legend.title.size = 2,
  ) 
 

################################################################################
# limiting amino acid
limiting_aa <- nss_region_aa_inad %>% 
  select(nss_region, lys_aas,tryp_aas,cyst_aas,meth_aas,thre_aas) %>% 
  group_by(nss_region) %>% 
  summarise(
    limiting_aa = colnames(pick(lys_aas:thre_aas))[which.min(c_across(lys_aas:thre_aas))], # Get column name
    # Find the AA with the lowest score
    min_aas =min(c_across(lys_aas:thre_aas))                      # Get the minimum AAS value
  )

# create shapefile with limiting aa
limiting_aa_sp <- limiting_aa %>% 
  mutate(nss_region = as.numeric(nss_region)) %>% 
  left_join(nss_region_shapefile, by = "nss_region") %>% 
  st_as_sf()

# create a map with limiting 
tm_shape(ind_state) +
  tm_fill(col = "grey77") +
  tm_shape(limiting_aa_sp) +
  tm_fill(col ="limiting_aa", 
          # style = "cont", breaks = seq(0,100,by=10),
          # palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Limiting Amino Acid" ,
          palette = "Set3",
          # legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "" , frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
  ) +
  # tm_borders(col = "black", lwd = 0.2) +
  tm_shape(ind_state) +
  # tm_text("State_Name", size = 0.6, remove.overlap = TRUE)+
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)

