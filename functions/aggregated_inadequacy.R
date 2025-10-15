
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