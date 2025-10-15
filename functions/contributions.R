
contributions <- function(vehicle = "rice"){
  ## takes quantity of vehicle consumed and combines the standards for that 
  ## vehicle to result in the contribution of each mn to the hhs intake
  
  
  if(vehicle == "rice"){
    selected_item =c(061,101)
  }else if (vehicle == "wheat"){
    selected_item = c(062,107)
  }
  
  # print(selected_item)
  food_consumption_daily_afe %>% 
    select(
      common_id,Item_Code, Total_Consumption_Quantity) %>% 
  filter(Item_Code %in% selected_item) %>% 
  # mutate(Home_Produce_Quantity = as.numeric(Home_Produce_Quantity),
  #        # remove any of the home produced quantity
  #        Purchased_Quantity = Total_Consumption_Quantity - Home_Produce_Quantity) %>%
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
    across(-c(Item_Code),
           ~sum(., na.rm = TRUE))
  )

  
}