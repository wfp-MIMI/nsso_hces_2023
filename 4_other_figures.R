

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
res_quintile_db <- caculate


hh_mn_intake_fort %>% 
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



