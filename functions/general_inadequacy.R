devtools::source_url("https://raw.githubusercontent.com/MIMI-wfp/MIMI-R-functions/refs/heads/main/iron_full_probability/iron_inad_prev_GB.R")


general_inadequacy <- function(data, group, ear_table, scenarios = c("")) {
  group_sym <- rlang::ensym(group)
  
  # Helper to generate inadequacy flags
  generate_inad_flags <- function(df, scenario_suffix) {
    for (nutrient in ear_table$nutrient) {
      var_name <- paste0(nutrient, scenario_suffix)
      inad_name <- paste0(nutrient, "_inad", scenario_suffix)
      ear_value <- ear_table$ear_value[ear_table$nutrient == nutrient]
      
      if (var_name %in% names(df)) {
        df[[inad_name]] <- ifelse(df[[var_name]] < ear_value, 1, 0)
      }
    }
    return(df)
  }
  
  # Apply inadequacy flag generation across scenarios
  for (scenario in scenarios) {
    data <- generate_inad_flags(data, scenario)
  }
  
  # Join additional data
  data <- data %>%
    mutate(national = '1')
  
  # Survey design
  survey_data <- data %>%
    as_survey_design(ids = c(ea, hhid), strata = res, weights = survey_wgt) %>%
    srvyr::group_by({{group}}) %>%
    summarise(across(contains("inad"), ~survey_mean(. == 1, proportion = TRUE, na.rm = TRUE) * 100))
  
  # # Add iron inadequacy estimates
  add_fe_inad <- function(fe_var, new_var) {
    fe_full_prob(
      data %>%
        # rename(ai_afe = fe_mg) |> 
        mutate(national = '1'),
      group1 = group_sym,
      bio_avail = 10,
      hh_weight = "survey_wgt" ,
      strata = "res",
      psu = "ea"
    ) 
    # %>%
    #   rename(!!group_sym := subpopulation)
  }
  
  survey_data <- survey_data %>%
    left_join(add_fe_inad("fe_mg", "fe_inad"))
  

  return(survey_data)
}

