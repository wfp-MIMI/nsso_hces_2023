#########################################
#      HCES 2022-23 INDIA               #
#         Protein/amino acid            #
#########################################


# Author: Gabriel Battcock
# Created: 
# Last updated: 29 May 2025

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
# set paths
figure_path <- "figures/"
raw_path <- "data/raw/"
processed_path <- "data/processed/"



file_list = list.files("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/")
# haven::read_dta("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/level06.dta")
data_list <- lapply(paste0("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/HCES_2022_23/",file_list), haven::read_dta)
names(data_list) <- tools::file_path_sans_ext(file_list)
level01 <- data_list$level01
rm(data_list, file_list)


ind_state <- sf::st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Workstream 2/Nutrition analysis/shapefiles/IND/India-State-and-Country-Shapefile-Updated-Jan-2020-master/India_State_Boundary.shp")
nss_region_shapefile <- sf::st_read("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/ind_nss2223_nssregion.shp")



food_consumption_daily_afe <- readRDS(paste0(processed_path,"ind_nss2223_food_consumption.rds"))
hh_mn_intake <- readRDS(paste0(processed_path,"ind_nss2223_base_case.rds"))
hh_expenditure <- readRDS(paste0(processed_path,"ind_nss2223_hh_expenditure.rds"))
# read in the fct
ind_202223_fct <-  read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/nsso_202223_fct.xlsx",sheet =1)
ind_202223_fct_amino_acids <- read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/nsso_202223_fct.xlsx",sheet =2)
ind_nss_hdds <- read_xlsx(paste0(raw_path, "ind_nss2223_hdds.xlsx"),sheet = 1)
# molly's database

mol_db <- read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/Ileal IAA digestibility and DIAAS of world foods_Molly Muleya_November 2021_RS_14102024.xlsx",
                    sheet ='Total and digestible IAA',
                    skip =1)

protein_digestibility <- read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/Ileal IAA digestibility and DIAAS of world foods_Molly Muleya_November 2021_RS_14102024.xlsx",
                           sheet ='IAA digestibility',
                           skip = 0)

################################################################################
hh_expenditure <- hh_expenditure %>% 
  mutate(
  sector = ifelse(sector == '1', "Rural","Urban"),
  res_quintile = paste(sector, res_quintile))


# rename some data columns 
ind_202223_fct_amino_acids <- ind_202223_fct_amino_acids %>% 
  rename(s_num = `S.No in Molly's database`)

# rename adjustable ratios 
mol_db <- mol_db %>% 
  rename(
    s_num= S.No,
    ratio_lys = `Ratio of digestable Lysine - Digestability factor`,
    ratio_tryp = `Ratio of digestable Tryptophan - Digestability factor`,
    ratio_cyst = `Ratio of digestable Cysteine - Digestability factor`,
    ratio_meth = `Ratio of digestable Methionine - Digestability factor`,
    ratio_thre = `Ratio of digestable Threonine - Digestability factor`,
    ratio_hist = `Ratio of digestable Histidine - Digestability factor`,
    ratio_iso = `Ratio of digestable Isoleucine - Digestability factor`,
    ratio_leuc = `Ratio of digestable Leucine - Digestability factor`,
    ratio_phe = `Ratio of digestable Phenylalanine - Digestability factor`,
    ratio_val = `Ratio of digestable Valine - Digestability factor`
  ) %>% 
  select(s_num, ratio_cyst,ratio_lys,ratio_tryp,ratio_meth,ratio_thre, ratio_hist,ratio_iso,ratio_leuc,ratio_phe,ratio_val)

#rename protein distibility
protein_digestibility <- 
  
  protein_digestibility %>%
  select(S.No, `Protein`) %>% 
  slice(0:-1) %>% 
  rename(s_num = S.No,
         prot_diget = `Protein`) %>% 
  mutate(s_num = as.numeric(s_num),
         prot_diget = as.numeric(prot_diget))
  

################################################################################

# match food items to protein
 # match food items to proein
  
adjust_lysine <- function(data, set_lysine_multiplier ) {
    
    figure_path <<- case_when(set_lysine_multiplier == 1 ~ "figures/protein/base/",
                              set_lysine_multiplier ==1.25 ~ "figures/protein/lysine_25/",
                              set_lysine_multiplier ==1.5 ~ "figures/protein/lysine_50/")
    
    data %>%
      left_join(ind_202223_fct %>%
                  select(item_code, energy_kcal, protein_g),
                by = c("Item_Code" = "item_code")) %>%
      left_join(ind_202223_fct_amino_acids %>% select(-protein_g), by = c("Item_Code" = "item_code")) %>%
      left_join(mol_db, by = "s_num") %>%
      left_join(protein_digestibility, by = "s_num") %>% 
      mutate(
        protein_g = (Total_Consumption_Quantity / 100) * protein_g,
        lysine_g_50 = case_when(
          #item codes for wheat 62,107, 108
          Item_Code %in% c(62, 107, 108) ~ (protein_g / 100) * as.numeric(lysine_g) * ratio_lys *1.5,
          TRUE ~ (as.numeric(protein_g) / 100) * as.numeric(lysine_g) * ratio_lys,
        ),
        lysine_g_25 = case_when(
          #item codes for wheat 62,107, 108
          Item_Code %in% c(62, 107, 108) ~ (protein_g / 100) * as.numeric(lysine_g) * ratio_lys *1.25,
          TRUE ~ (as.numeric(protein_g) / 100) * as.numeric(lysine_g) * ratio_lys,
        ),
        
        lysine_g = case_when(
          #item codes for wheat 62,107, 108
          Item_Code %in% c(62, 107, 108) ~ (protein_g / 100) * as.numeric(lysine_g) * ratio_lys *1,
          TRUE ~ (as.numeric(protein_g) / 100) * as.numeric(lysine_g) * ratio_lys,
        ),
       
       
        tryptophan_g = (protein_g / 100) * as.numeric(tryptophan_g) * ratio_tryp,
        methionine_g = (protein_g / 100) * methionine_g * ratio_meth,
        cystine_g = (protein_g / 100) * cystine_g * ratio_cyst,
        threonine_g = (protein_g / 100) * threonine_g * ratio_thre,
        histidine_g = (protein_g / 100) * histidine_g * ratio_hist,
        isoleucine_g = (protein_g / 100) * isoleucine_g * ratio_iso,
        leucine_g = (protein_g / 100) * leucine_g * ratio_leuc,
        phenylalanine_g = (protein_g / 100) * phenylalanine_g * ratio_phe,
        valine_g = (protein_g / 100) * valine_g * ratio_val,
        
        energy_kcal = (Total_Consumption_Quantity / 100) * energy_kcal,
        protein_adjust_g = prot_diget*protein_g
      )
  }
  
#set the value
set_lysine_multiplier <- 1

item_level_amino_acids <-adjust_lysine(food_consumption_daily_afe, set_lysine_multiplier)
# rm(mol_db)

# calcalate household totals of amino acids

household_amino_acids <- item_level_amino_acids %>% 
  group_by(common_id) %>% 
  summarise(
    across(
      c(lysine_g,lysine_g_25, lysine_g_50,tryptophan_g,methionine_g,cystine_g,threonine_g,histidine_g,isoleucine_g,leucine_g,phenylalanine_g,valine_g, energy_kcal, protein_adjust_g),
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
  

# sanity check
household_amino_acids %>% 
  select(lysine_g, lysine_g_25,lysine_g_50) %>% 
  ggplot(aes(x = lysine_g, y = lysine_g_50))+
  geom_point(alpha = 0.5)




# create intake at an admin level
median_aa_intake <- function(...){
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
        c(protein_adjust_g,lysine_g,lysine_g_25, lysine_g_50,tryptophan_g,cystine_g,methionine_g,threonine_g,histidine_g,isoleucine_g,leucine_g,phenylalanine_g,valine_g),
        list(
          median = ~survey_quantile(., 0.5,na.rm = TRUE),
          Q1 = ~survey_quantile(., 0.25, na.rm = TRUE),
          Q3 = ~survey_quantile(., 0.75, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}" 
      )
    )
  return(x)
}







national <- median_aa_intake()

sep_quintile_intake_aa <- median_aa_intake(sep_quintile) %>% 
  filter(!is.na(sep_quintile))

res_quintile_intake_aa <- median_aa_intake(res_quintile)  %>% 
  
  filter(!is.na(res_quintile)) %>% 
  bind_rows(
    median_aa_intake(sector) %>% 
      mutate(res_quintile = "0")
  ) 
# %>% mutate(res_quintile = case_when(
#     res_quintile == "0" ~ "National",
#     res_quintile == "1" ~ "Poorest",
#     res_quintile == "2" ~ "Poor",
#     res_quintile == "3" ~ "Middle",
#     res_quintile == "4" ~ "Rich",
#     res_quintile == "5" ~ "Richest"
#   ))

pop_intake_aa <- median_aa_intake()

nss_region_intake_aa <-  median_aa_intake(nss_region)
state_intake <- median_aa_intake(state)


intake_all <- 
  state_intake %>% 
  rename(sub_pop = state) %>% 
  bind_rows(national %>% 
              mutate(sub_pop = "National")) %>% 
  bind_rows(res_quintile_intake_aa %>% 
              rename(sub_pop = res_quintile)) 


write.csv(intake_all, paste0(figure_path, "all_intake.csv"))

nss_region_aa_intake_sp <- nss_region_intake_aa %>% 
  mutate(nss_region = as.numeric(nss_region)) %>% 
  left_join(nss_region_shapefile, by = "nss_region") %>% 
  st_as_sf()


# intake maps 

intake_map <- function(amino_acid,
                       title = ""){
  
  tm_shape(ind_state) +
    tm_fill(col = "grey77") +
    tm_shape(nss_region_aa_intake_sp) +
    tm_fill(col = {{amino_acid}}, style = "cont",
            # breaks =  seq(1.4,2.8,0.4),
            palette = "Reds",
            title = paste({{title}},"intake"),
            legend.is.portrait = TRUE
    ) +
    tm_layout(main.title = {{title}}, frame = F,
              main.title.size = 0.8,
              legend.outside = FALSE
              # legend.outside.position = "bottom"
              # legend.outside.size = 1   # Adjust the width of the legend
              # legend.height = 0.2   # Adjust height if needed
    ) +
    tm_shape(ind_state) +
    tm_borders(col = "black", lwd = 1.5) +
    tm_legend(show = T)
}

# Function to save intake data
save_intake_map <- function(var_name, amino_acid) {
  # Call intake_map function (assuming it generates a plot or some output)
  m <- intake_map(var_name, amino_acid)
  
  # Define file name and path
  file_name <- paste0(amino_acid, "_intake.png")
  file_path <- file.path(figure_path, file_name)
  
  # Save the plot (assuming intake_map generates a plot)
  tmap_save(m,file_path, width = 6, height = 4)
}



save_intake_map("protein_adjust_g_median_q50", "Protein")

# Only calculate for the base case
if (set_lysine_multiplier == 1) {
  amino_acids <- c(
    "tryptophan_g_median_q50" = "Tryptophan",
    "cystine_g_median_q50" = "Cystine",
    "methionine_g_median_q50" = "Methionine",
    "threonine_g_median_q50" = "Threonine",
    "histidine_g_median_q50" = "Histidine",
    "isoleucine_g_median_q50" = "Isoleucine",
    "leucine_g_median_q50" = "Leucine",
    "phenylalanine_g_median_q50" = "Phenylalanine",
    "valine_g_median_q50" = "Valine",
    "protein_adjust_g_median_q50" = "Protein"
  )
  
  # Apply function to save each amino acid intake map
  lapply(names(amino_acids), function(x) save_intake_map(x, amino_acids[x]))
}

print("Figures saved successfully!")


# 
  amino_acids <- c(
    "lysine_g_median_q50" = "Lysine",
    "lysine_g_25_median_q50" = "Lysine25",
    "lysine_g_50_median_q50" = "Lysine50"
  )
  
  # Apply function to save each amino acid intake map
  lapply(names(amino_acids), function(x) save_intake_map(x, amino_acids[x]))


  
  
# prevalence of inadequacy -----------------------------------------------------

# RDA 
inad_aa_intake <- function(...){
  
  ind_aa_intake <- household_amino_acids
  
  aa_rda <- data.frame(
    amino_acid = c("lysine", "tryptophan","cystine","methionine", "threonine","histidine","isoleucine","leucine",
                  "phenylalanine","valine", "protein"),
    value_g_per_kg = c(0.030,0.004,0.004,0.010,0.015,0.01,0.02,0.039,0.012,0.026, 0.66)#WHO recommendations - https://iris.who.int/bitstream/handle/10665/43411/WHO_TRS_935_eng.pdf
  )
  
  weight_kg = 55 # ASSUMPTION - Based on average body weight of women (EAR india)
  
  aa_rda <- aa_rda %>% 
    mutate(rda = value_g_per_kg*55)
  
  aa_intake_inad <- ind_aa_intake %>% 
    mutate(
      lys_rda = 1.65, 
      tryp_rda = 0.22,
      cyst_rda = 0.220,
      meth_rda = 0.550,
      thre_rda = 0.825,
      hist_rda = 0.55,
      iso_rda = 1.1,
      leuc_rda = 2.14,
      phe_rda = 0.66,
      val_rda = 1.43,
      prot_rda = 36.30,
      lys_inad = ifelse(lysine_g<lys_rda, 1,0),
      tryp_inad = ifelse(tryptophan_g<tryp_rda, 1,0),
      cyst_inad = ifelse(cystine_g<cyst_rda,1,0),
      meth_inad = ifelse(methionine_g<meth_rda,1,0),
      thre_inad = ifelse(threonine_g<thre_rda,1,0),
      hist_inad = ifelse(histidine_g<hist_rda,1,0),
      iso_inad = ifelse(isoleucine_g<iso_rda,1,0),
      leuc_inad = ifelse(leucine_g<leuc_rda,1,0),
      phe_inad = ifelse(phenylalanine_g<phe_rda,1,0),
      val_inad = ifelse(valine_g<val_rda,1,0),
      
      
      
      
      
      protein_inad = ifelse(protein_adjust_g<prot_rda, 1,0),
      prot_aas = protein_adjust_g/prot_rda,
      lys_aas = lysine_g/lys_rda,
      tryp_aas = tryptophan_g/tryp_rda,
      cyst_aas = cystine_g/cyst_rda,
      meth_aas = methionine_g/meth_rda,
      thre_aas = threonine_g/thre_rda,
      hist_aas = histidine_g/hist_rda,
      iso_aas = isoleucine_g/iso_rda,
      leuc_aas = leucine_g/leuc_rda,
      phe_aas = phenylalanine_g/phe_rda,
      val_aas = valine_g/val_rda
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
      hist_inad = srvyr::survey_mean(hist_inad == 1,proportion = TRUE, na.rm = TRUE)*100,
      iso_inad = srvyr::survey_mean(iso_inad == 1,proportion = TRUE, na.rm = TRUE)*100,
      leuc_inad = srvyr::survey_mean(leuc_inad == 1,proportion = TRUE, na.rm = TRUE)*100,
      phe_inad = srvyr::survey_mean(phe_inad == 1,proportion = TRUE, na.rm = TRUE)*100,
      val_inad = srvyr::survey_mean(val_inad == 1,proportion = TRUE, na.rm = TRUE)*100,
      protein_inad = srvyr::survey_mean(protein_inad == 1, proportion = TRUE, na.rm = TRUE)*100,
      lys_aas = srvyr::survey_mean(lys_aas,na.rm = TRUE),
      tryp_aas = srvyr::survey_mean(tryp_aas,na.rm = TRUE),
      cyst_aas = srvyr::survey_mean(cyst_aas,na.rm = TRUE),
      meth_aas = srvyr::survey_mean(meth_aas,na.rm = TRUE),
      thre_aas = srvyr::survey_mean(thre_aas,na.rm = TRUE),
      hist_aas = srvyr::survey_mean(hist_aas,na.rm = TRUE),
      iso_aas = srvyr::survey_mean(iso_aas,na.rm = TRUE),
      leuc_aas = srvyr::survey_mean(leuc_aas,na.rm = TRUE),
      phe_aas = srvyr::survey_mean(phe_aas,na.rm = TRUE),
      val_aas = srvyr::survey_mean(val_aas,na.rm = TRUE),
  
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



write.csv(aa_all, paste0(figure_path,"aa_score.csv"))

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


# Function to save intake data
save_inad_map <- function(var_name, amino_acid) {
  # Call intake_map function (assuming it generates a plot or some output)
  m <- inadequacy_map(var_name, amino_acid)
  
  # Define file name and path
  file_name <- paste0(amino_acid, "_inad.png")
  file_path <- file.path(figure_path, file_name)
  
  # Save the plot (assuming intake_map generates a plot)
  tmap_save(m,file_path, width = 6, height = 4)
}



save_inad_map("lys_inad", "Lysine")

# Only calculate for the base case
if (set_lysine_multiplier == 1) {
  amino_acids <- c(
    "tryp_inad" = "Tryptophan",
    "cyst_inad" = "Cystine",
    "meth_inad" = "Methionine",
    "thre_inad" = "Threonine",
    "hist_inad" = "Histidine",
    "iso_inad" = "Isoleucine",
    "leuc_inad" = "Leucine",
    "phe_inad" = "Phenylalanine",
    "val_inad" = "Valine"
  )
  
  # Apply function to save each amino acid intake map
  lapply(names(amino_acids), function(x) save_inad_map(x, amino_acids[x]))
}

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

# Function to save intake data
save_aas_map <- function(var_name, amino_acid) {
  # Call intake_map function (assuming it generates a plot or some output)
  m <- aas_map(var_name, amino_acid)
  
  # Define file name and path
  file_name <- paste0(amino_acid, "_aas.png")
  file_path <- file.path(figure_path, file_name)
  
  # Save the plot (assuming intake_map generates a plot)
  tmap_save(m,file_path, width = 6, height = 4)
}



save_aas_map("lys_aas", "Lysine")

# Only calculate for the base case
if (set_lysine_multiplier == 1) {
  amino_acids <- c(
    "tryp_aas" = "Tryptophan",
    "cyst_aas" = "Cystine",
    "meth_aas" = "Methionine",
    "thre_aas" = "Threonine",
    "hist_aas" = "Histidine",
    "iso_aas" = "Isoleucine",
    "leuc_aas" = "Leucine",
    "phe_aas" = "Phenylalanine",
    "val_aas" = "Valine"
  )
  
  # Apply function to save each amino acid intake map
  lapply(names(amino_acids), function(x) save_aas_map(x, amino_acids[x]))
}

tm_shape(ind_state) +
  tm_fill(col = "grey77") +
  tm_shape(nss_region_aa_inad_sp) +
  tm_fill(col = 'lys_inad', style = "cont", breaks = seq(0,100,by=10),
          # palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Risk of inadequacy" ,
          legend.is.portrait = FALSE
  )+
  tm_layout(legend.only = TRUE,
            legend.text.size = 1.5,         # Increase the size of legend text
            legend.title.size = 2,
  ) 
 

################################################################################
# limiting amino acid
limiting_aa <- nss_region_aa_inad %>% 
  select(nss_region, lys_aas,tryp_aas,cyst_aas,meth_aas,thre_aas,hist_aas,iso_aas, leuc_aas, phe_aas,val_aas) %>% 
  group_by(nss_region) %>% 
  summarise(
    limiting_aa = colnames(pick(lys_aas:val_aas))[which.min(c_across(lys_aas:val_aas))], # Get column name
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


################################################################################


food_group_cols <- colnames(ind_nss_hdds %>% select(-c(item_code,item_name)))

ind_nss_hdds<- ind_nss_hdds %>% 
  pivot_longer(cols = -c(item_code,item_name)) %>% 
  filter(value == 1) %>% 
  select(-value) %>% 
  rename(food_group = name, 
         Item_Code = item_code)

# data clean
food_group_full <- item_level_amino_acids %>% 
  select(c(common_id,Item_Code, protein_g,lysine_g,tryptophan_g , cystine_g , threonine_g , methionine_g, histidine_g, isoleucine_g , leucine_g, phenylalanine_g , valine_g ))  %>% 
  left_join(hh_expenditure) %>% 
  left_join(ind_nss_hdds,by = 'Item_Code' ) 

## 
## summarise micronutrient contributions from food groups nationally
national_foodgroup_average <- food_group_full %>% 
  group_by(common_id, food_group) %>% 
  summarise(
    across(
      c(protein_g,lysine_g,tryptophan_g , cystine_g , threonine_g , methionine_g, histidine_g, isoleucine_g , leucine_g, phenylalanine_g , valine_g ),
      ~sum(., na.rm = T)
      
    )
  ) %>% 
  ungroup() %>% 
  group_by(food_group) %>% 
  summarise(
    across(
      c(protein_g,lysine_g,tryptophan_g , cystine_g , threonine_g , methionine_g, histidine_g, isoleucine_g , leucine_g, phenylalanine_g , valine_g ),
      ~mean(.)
      
    )
  )




# create list of micronutrient names
prot_lys <- c(colnames(national_foodgroup_average[2:3]))


state_prop_boxes <- function(){
  # function reads in a state number and produces proportional box-plots
  # 
  mn_fg_plots <- list()
  for(item in prot_lys){
    print(item)
    # print({{state_num}})
    p1 <-  national_foodgroup_average %>%
      
      filter( !is.na(food_group)) %>%
      ggplot(aes(area = !!sym(item),
                 fill = stringr::str_to_title(
                   str_replace_all(food_group, "_", " and ")   ),
                 label =
                   stringr::str_to_title(
                     str_replace_all(food_group, "_", " and ")         )
      )) +
      geom_treemap() +
      geom_treemap_text( colour = "darkblue", place = "topleft", alpha = 0.6,
                         grow = FALSE,min.size = 6)+
      labs(title =
             stringr::str_to_title(stringr::str_split_i(item,
                                                        "\\_",
                                                        1)),
           
      )+
      scale_fill_brewer(palette = "Set3")+
      # guides(fill=guide_legend())+
      theme(legend.position="bottom",
            legend.spacing.x = unit(0, 'cm'))+
      guides(fill = guide_legend(title="Food group",label.position = "bottom"))
    # theme(legend.direction = "horizontal", legend.position = "bottom")+
    # guides(fill = "none")+
    theme_ipsum()
    mn_fg_plots[[item]] <- p1
  }
  return(mn_fg_plots)
}

nat_fg<-state_prop_boxes()

nat_fg <- ggpubr::ggarrange(plotlist = nat_fg, common.legend = TRUE)
nat_fg <- ggpubr::annotate_figure(nat_fg, top = ggpubr::text_grob("National", face = "bold", size = 15))
ggsave(paste0(figure_path,"nat_fg.png"), nat_fg, height = 5, width = 6)



other_aa<-c(colnames(national_foodgroup_average[4:12]))

other_aa_prop <- function(){
  # function reads in a state number and produces proportional box-plots
  # 
  mn_fg_plots <- list()
  for(item in other_aa){
    print(item)
    # print({{state_num}})
    p1 <-  national_foodgroup_average %>%
      
      filter( !is.na(food_group)) %>%
      ggplot(aes(area = !!sym(item),
                 fill = stringr::str_to_title(
                   str_replace_all(food_group, "_", " and ")   ),
                 label =
                   stringr::str_to_title(
                     str_replace_all(food_group, "_", " and ")         )
      )) +
      geom_treemap() +
      geom_treemap_text( colour = "darkblue", place = "topleft", alpha = 0.6,
                         grow = FALSE,min.size = 6)+
      labs(title =
             stringr::str_to_title(stringr::str_split_i(item,
                                                        "\\_",
                                                        1)),
           
      )+
      scale_fill_brewer(palette = "Set3")+
      # guides(fill=guide_legend())+
      theme(legend.position="bottom",
            legend.spacing.x = unit(0, 'cm'))+
      guides(fill = guide_legend(title="Food group",label.position = "bottom"))
    # theme(legend.direction = "horizontal", legend.position = "bottom")+
    # guides(fill = "none")+
    theme_ipsum()
    mn_fg_plots[[item]] <- p1
  }
  return(mn_fg_plots)
}


other_aa_fg<-other_aa_prop()

other_aa_fg <- ggpubr::ggarrange(plotlist = other_aa_fg, common.legend = TRUE)
other_aa_fg <- ggpubr::annotate_figure(other_aa_fg, top = ggpubr::text_grob("National", face = "bold", size = 15))
ggsave(paste0(figure_path,"other_aa_fg.png"), other_aa_fg, height = 8, width = 6)

