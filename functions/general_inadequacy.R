devtools::source_url(
  "https://raw.githubusercontent.com/MIMI-wfp/MIMI-R-functions/refs/heads/main/iron_full_probability/iron_inad_prev_GB.R"
)

general_inadequacy <- function(
  data,
  group = NULL,
  ear_table,
  scenarios = c("")
) {
  has_group <- !is.null(substitute(group))

  if (has_group) {
    group_sym <- rlang::ensym(group)
    group_quo <- rlang::enquo(group)
  } else {
    group_sym <- rlang::sym("national")
    group_quo <- rlang::quo(national)
  }

  group_name <- rlang::as_name(group_sym)

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

  # Add national column for ungrouped analysis
  data <- data %>%
    mutate(national = '1')

  # Survey design and summarise
  survey_data <- data %>%
    as_survey_design(ids = c(ea, hhid), strata = res, weights = survey_wgt) %>%
    srvyr::group_by(!!group_quo) %>%
    summarise(across(
      contains("inad"),
      ~ survey_mean(. == 1, proportion = TRUE, na.rm = TRUE) * 100
    ))

  # Add iron inadequacy estimates per scenario
  for (scenario in scenarios) {
    fe_var <- paste0("fe_mg", scenario)
    new_var <- paste0("fe_inad", scenario)

    if (fe_var %in% names(data)) {
      message("Calculating iron inadequacy for scenario: ", scenario)

      temp_data <- data

      # Rename scenario iron variable to fe_mg for fe_full_prob
      if (fe_var != "fe_mg") {
        temp_data <- temp_data |>
          select(-any_of("fe_mg")) |>
          rename(fe_mg = !!rlang::sym(fe_var))
      }

      fe_results <- fe_full_prob(
        temp_data,
        group1 = group_name,
        bio_avail = 10,
        hh_weight = "survey_wgt",
        strata = "res",
        psu = "ea"
      ) %>%
        # fe_full_prob returns the group column directly (no 'subpopulation')
        # srvyr names the SE column as prev_inad_se automatically
        rename(
          !!new_var := prev_inad,
          !!paste0(new_var, "_se") := prev_inad_se
        )

      survey_data <- survey_data %>%
        left_join(fe_results, by = group_name)
    }
  }

  return(survey_data)
}
