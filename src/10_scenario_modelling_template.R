# Authors: Uchenna Agu & Gabriel Battcock
# Date: August 2026

# ==============================================================================
# 1. INSTALL AND LOAD REQUIRED PACKAGES
# ==============================================================================

# List of required packages
rq_packages <- c("tidyverse", "srvyr", "survey")

# Install missing packages
installed_packages <- rq_packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

# Load packages into session
lapply(rq_packages, require, character.only = TRUE)

# Clean workspace
rm(list = c("rq_packages", "installed_packages"))

# ------------------------------------------------------------------------------
# 2. DEFINE FILE PATHS
# ------------------------------------------------------------------------------

data_folder <- paste0("Folder that contains the three files")

hh_info_path <- file.path(
  data_folder,
  "hh_info.csv"
)

cons_path <- file.path(
  data_folder,
  "food_consumption_quantities.csv"
)

fct_path <- file.path(
  data_folder,
  "fct.csv"
)

output_folder <- paste0("Folder for saving results")

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 3. CHECK THAT THE INPUT FILES EXIST
# ------------------------------------------------------------------------------

input_files <- c(
  hh_info_path,
  cons_path,
  fct_path
)

missing_files <- input_files[!file.exists(input_files)]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "The following input files were not found:\n",
      paste(missing_files, collapse = "\n")
    )
  )
}


# ------------------------------------------------------------------------------
# 4. DEFINE FORTIFICATION PARAMETERS
# ------------------------------------------------------------------------------

# item item codes
# Replace X values with code(s) for food item
# Note: an adjustment map must be created for wheat flour products, this script assumes 100% of food item is fortifiable
item_codes <- c(X, X, X)

# Compliance scenarios: base line, 50%, 85%, 90% and 100%
compliance_scenarios <- c(0.00, 0.50, 0.85, 0.90, 1.00)

# Additional micronutrient concentration in fortified item per 100 g
# Replace X values with fortification standards per 100g of food item. 
fortification_values <- list(
  vita_rae_mcg = X,
  vitb12_mcg = X,
  fe_mg      = x,
  zn_mg      = x,
  folate_mcg = x
)


# ------------------------------------------------------------------------------
# 5. PROCESS FOOD CONSUMPTION DATA
# ------------------------------------------------------------------------------

process_food_consumption <- function(
    hh_info_path,
    cons_path,
    fct_path
) {
  
  # --------------------------------------------------------------------------
  # Read household information and retain AFE
  # --------------------------------------------------------------------------
  
  hh_info <- read_csv(
    hh_info_path,
    show_col_types = FALSE
  ) %>%
    transmute(
      hhid = as.character(hhid),
      afe = as.numeric(afe)
    )
  
  
  # --------------------------------------------------------------------------
  # Check for invalid AFE values
  # --------------------------------------------------------------------------
  
  invalid_afe <- hh_info %>%
    filter(
      is.na(afe) |
        afe <= 0
    )
  
  if (nrow(invalid_afe) > 0) {
    
    warning(
      nrow(invalid_afe),
      " household(s) have missing or non-positive AFE values. ",
      "Their consumption records will have missing quantities per AFE."
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Read food-consumption data
  # --------------------------------------------------------------------------
  
  cons_data <- read_csv(
    cons_path,
    show_col_types = FALSE
  ) %>%
    mutate(
      hhid = as.character(hhid)
    ) %>%
    left_join(
      hh_info,
      by = "hhid"
    ) %>%
    mutate(
      # quantity_100g = number of 100 g portions consumed
      # Dividing by AFE gives quantity consumed per AFE, assuming quantities are recorded at the hh level
      quantity_100g_afe = quantity_100g / afe
    )
  
  
  # --------------------------------------------------------------------------
  # Read food-composition table
  # --------------------------------------------------------------------------
  
  fct_data <- read_csv(
    fct_path,
    show_col_types = FALSE
  )
  
  
  # --------------------------------------------------------------------------
  # Identify consumption records without an FCT match
  # --------------------------------------------------------------------------
  
  unmatched_items <- cons_data %>%
    anti_join(
      fct_data %>%
        distinct(item_code),
      by = "item_code"
    )
  
  if (nrow(unmatched_items) > 0) {
    
    warning(
      nrow(unmatched_items),
      " consumption record(s) have item codes that are not found ",
      "in the food-composition table."
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Nutrient columns
  # --------------------------------------------------------------------------
  
  nutrient_columns <- c(
    "vita_rae_mcg",
    "folate_mcg",
    "vitb12_mcg",
    "fe_mg",
    "zn_mg"
  )
  
  
  # --------------------------------------------------------------------------
  # Check that all required nutrient columns are in FCT
  # --------------------------------------------------------------------------
  
  missing_nutrient_columns <- setdiff(
    nutrient_columns,
    names(fct_data)
  )
  
  if (length(missing_nutrient_columns) > 0) {
    
    stop(
      paste0(
        "The following nutrient columns are missing from the FCT:\n",
        paste(missing_nutrient_columns, collapse = ", ")
      )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Join food quantities to FCT
  # --------------------------------------------------------------------------
  
  cons_fct_data <- cons_data %>%
    inner_join(
      fct_data,
      by = "item_code"
    ) %>%
    mutate(
      across(
        all_of(nutrient_columns),
        as.numeric
      )
    )
  
  
  # --------------------------------------------------------------------------
  # Apply item fortification scenarios
  #
  # IMPORTANT:
  # The fortification values are ADDITIONAL amounts.
  #
  # For item:
  #
  # fortified nutrient =
  #       existing FCT nutrient
  #       + (compliance × fortification level)
  #
  # For non-item foods:
  #       existing FCT nutrient remains unchanged.
  # --------------------------------------------------------------------------
  
  # Create a function to apply one compliance scenario
  apply_fortification <- function(data, compliance) {
    
    data %>%
      mutate(
        
        # --------------------------------------------------------------
        # Vitamin A
        # --------------------------------------------------------------
        
        vita_rae_mcg = case_when(
          
          item_code %in% item_codes ~
            vita_rae_mcg +
            compliance * fortification_values$vita_rae_mcg,
          
          TRUE ~ vita_rae_mcg
        ),
        
        
        # --------------------------------------------------------------
        # Folate
        # --------------------------------------------------------------
        
        folate_mcg = case_when(
          
          item_code %in% item_codes ~
            folate_mcg +
            compliance * fortification_values$folate_mcg,
          
          TRUE ~ folate_mcg
        ),
        
        
        # --------------------------------------------------------------
        # Vitamin B12
        # --------------------------------------------------------------
        
        vitb12_mcg = case_when(
          
          item_code %in% item_codes ~
            vitb12_mcg +
            compliance * fortification_values$vitb12_mcg,
          
          TRUE ~ vitb12_mcg
        ),
        
        
        # --------------------------------------------------------------
        # Iron
        # --------------------------------------------------------------
        
        fe_mg = case_when(
          
          item_code %in% item_codes ~
            fe_mg +
            compliance * fortification_values$fe_mg,
          
          TRUE ~ fe_mg
        ),
        
        
        # --------------------------------------------------------------
        # Zinc
        # --------------------------------------------------------------
        
        zn_mg = case_when(
          
          item_code %in% item_codes ~
            zn_mg +
            compliance * fortification_values$zn_mg,
          
          TRUE ~ zn_mg
        )
      )
  }
  
  
  # --------------------------------------------------------------------------
  # Create all compliance scenarios
  # --------------------------------------------------------------------------
  
  scenario_data <- map_dfr(
    compliance_scenarios,
    
    function(compliance) {
      
      cons_fct_scenario <- cons_fct_data %>%
        apply_fortification(
          compliance = compliance
        ) %>%
        mutate(
          compliance = compliance
        )
      
      cons_fct_scenario
    }
  )
  
  
  # --------------------------------------------------------------------------
  # Convert nutrient concentrations into household nutrient intake
  # --------------------------------------------------------------------------
  
  scenario_data <- scenario_data %>%
    mutate(
      across(
        all_of(nutrient_columns),
        ~ .x * quantity_100g_afe
      )
    )
  
  
  # --------------------------------------------------------------------------
  # Aggregate nutrient contributions to household level
  # --------------------------------------------------------------------------
  
  cons_fct_totals <- scenario_data %>%
    group_by(
      compliance,
      hhid
    ) %>%
    summarise(
      across(
        all_of(nutrient_columns),
        
        ~ {
          
          if (all(is.na(.x))) {
            
            NA_real_
            
          } else {
            
            sum(.x, na.rm = TRUE)
          }
        }
      ),
      
      .groups = "drop"
    )
  
  
  return(cons_fct_totals)
}


# ------------------------------------------------------------------------------
# 6. DEFINE EAR REFERENCE VALUES
# ------------------------------------------------------------------------------

# Define the EAR reference data for targets creation
# Replace X values with EAR for each
allen_ear <- data.frame(
  nutrient = c(
    "vita_rae_mcg",
    "folate_mcg",
    "vitb12_mcg",
    "fe_mg",
    "zn_mg"
  ),
  ear_value = c(
    X,
    X,
    X,
    X, 
    X  
  )
)

reference_values <- setNames(
  allen_ear$ear_value,
  allen_ear$nutrient
)


# ------------------------------------------------------------------------------
# 7. FUNCTION FOR IRON FULL-PROBABILITY APPROACH
# ------------------------------------------------------------------------------

iron_probability <- function(
    fe_mg,
    bio_avail = 10
) {
  
  if (!bio_avail %in% c(5, 10, 15)) {
    
    stop(
      "bio_avail must be one of the following values: 5, 10, or 15."
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Iron thresholds
  # --------------------------------------------------------------------------
  
  thresholds <- switch(
    
    as.character(bio_avail),
    
    "5" = c(
      15.0,
      16.7,
      18.7,
      21.4,
      23.6,
      25.7,
      27.8,
      30.2,
      33.2,
      37.3,
      45.0,
      53.5,
      63.0
    ),
    
    "10" = c(
      7.5,
      8.4,
      9.4,
      10.7,
      11.8,
      12.9,
      13.9,
      15.1,
      16.6,
      18.7,
      22.5,
      26.7,
      31.5
    ),
    
    "15" = c(
      5.0,
      5.6,
      6.2,
      7.1,
      7.9,
      8.6,
      9.3,
      10.1,
      11.1,
      12.4,
      15.0,
      17.8,
      21.0
    )
  )
  
  
  # --------------------------------------------------------------------------
  # Corresponding probabilities
  # --------------------------------------------------------------------------
  
  probabilities <- c(
    1.00,
    0.96,
    0.93,
    0.85,
    0.75,
    0.65,
    0.55,
    0.45,
    0.35,
    0.25,
    0.15,
    0.08,
    0.04,
    0.00
  )
  
  
  # --------------------------------------------------------------------------
  # Assign probability according to iron intake
  # --------------------------------------------------------------------------
  
  result <- cut(
    
    fe_mg,
    
    breaks = c(
      -Inf,
      thresholds,
      Inf
    ),
    
    labels = probabilities,
    
    right = TRUE,
    
    include.lowest = TRUE
  )
  
  
  as.numeric(
    as.character(result)
  )
}


# ------------------------------------------------------------------------------
# 8. PROCESS FOOD-CONSUMPTION DATA
# ------------------------------------------------------------------------------

base_ai <- process_food_consumption(
  
  hh_info_path = hh_info_path,
  
  cons_path = cons_path,
  
  fct_path = fct_path
)


# ------------------------------------------------------------------------------
# 9. READ HOUSEHOLD INFORMATION
# ------------------------------------------------------------------------------

hh_information <- read_csv(
  hh_info_path,
  show_col_types = FALSE
) %>%
  mutate(
    hhid = as.character(hhid)
  )


# ------------------------------------------------------------------------------
# 10. CHECK REQUIRED HOUSEHOLD VARIABLES
# ------------------------------------------------------------------------------

required_hh_variables <- c(
  "hhid",
  "survey_wgt",
  "adm1",
  "res",
  "sep_quintile"
)

missing_hh_variables <- setdiff(
  required_hh_variables,
  names(hh_information)
)

if (length(missing_hh_variables) > 0) {
  
  stop(
    paste0(
      "The following required variables are missing from hh_info:\n",
      paste(missing_hh_variables, collapse = ", ")
    )
  )
}


# ------------------------------------------------------------------------------
# 11. CHECK FOR DUPLICATE HOUSEHOLD IDs
# ------------------------------------------------------------------------------

duplicate_hhid <- hh_information %>%
  count(
    hhid,
    name = "number_of_records"
  ) %>%
  filter(
    number_of_records > 1
  )

if (nrow(duplicate_hhid) > 0) {
  
  stop(
    nrow(duplicate_hhid),
    " household ID(s) occur more than once in the household-information file."
  )
}


# ------------------------------------------------------------------------------
# 12. CREATE HOUSEHOLD-LEVEL INADEQUACY VARIABLES
# ------------------------------------------------------------------------------

analysis_data <- base_ai %>%
  
  mutate(
    
    # ------------------------------------------------------------------------
    # Compliance label
    # ------------------------------------------------------------------------
    
    compliance_label = case_when(
      
      compliance == 0.00 ~ "0%",
      
      compliance == 0.50 ~ "50%",
      
      compliance == 0.85 ~ "85%",
      
      compliance == 0.90 ~ "90%",
      
      compliance == 1.00 ~ "100%",
      
      TRUE ~ as.character(compliance)
    ),
    
    
    # ------------------------------------------------------------------------
    # Vitamin A
    # ------------------------------------------------------------------------
    
    vita_inadequate = case_when(
      
      is.na(vita_rae_mcg) ~ NA_real_,
      
      vita_rae_mcg < reference_values["vita_rae_mcg"] ~ 1,
      
      TRUE ~ 0
    ),
    
    
    # ------------------------------------------------------------------------
    # Folate
    # ------------------------------------------------------------------------
    
    folate_inadequate = case_when(
      
      is.na(folate_mcg) ~ NA_real_,
      
      folate_mcg < reference_values["folate_mcg"] ~ 1,
      
      TRUE ~ 0
    ),
    
    
    # ------------------------------------------------------------------------
    # Vitamin B12
    # ------------------------------------------------------------------------
    
    vitb12_inadequate = case_when(
      
      is.na(vitb12_mcg) ~ NA_real_,
      
      vitb12_mcg < reference_values["vitb12_mcg"] ~ 1,
      
      TRUE ~ 0
    ),
    
    
    # ------------------------------------------------------------------------
    # Zinc
    # ------------------------------------------------------------------------
    
    zn_inadequate = case_when(
      
      is.na(zn_mg) ~ NA_real_,
      
      zn_mg < reference_values["zn_mg"] ~ 1,
      
      TRUE ~ 0
    ),
    
    
    # ------------------------------------------------------------------------
    # Iron
    #
    # Full-probability approach at 10% bioavailability
    # ------------------------------------------------------------------------
    
    fe_prob_inadequate = iron_probability(
      
      fe_mg = fe_mg,
      
      bio_avail = 10
    )
  ) %>%
  
  # --------------------------------------------------------------------------
# Add household survey information
# --------------------------------------------------------------------------

left_join(
  
  hh_information %>%
    
    select(
      hhid,
      survey_wgt,
      adm1,
      res,
      sep_quintile
    ),
  
  by = "hhid"
) %>%
  
  mutate(
    
    survey_wgt = as.numeric(survey_wgt),
    
    adm1 = as.character(adm1),
    
    res = as.character(res),
    
    sep_quintile = as.character(sep_quintile)
  )


# ------------------------------------------------------------------------------
# 13. REMOVE MISSING VALUES
# ------------------------------------------------------------------------------

analysis_data <- na.omit(
  analysis_data
)


# ------------------------------------------------------------------------------
# 14. DATA-QUALITY CHECKS
# ------------------------------------------------------------------------------

data_quality_summary <- analysis_data %>%
  
  summarise(
    
    number_households = n_distinct(hhid),
    
    missing_weight = sum(
      is.na(survey_wgt)
    ),
    
    nonpositive_weight = sum(
      !is.na(survey_wgt) &
        survey_wgt <= 0
    ),
    
    missing_adm1 = sum(
      is.na(adm1)
    ),
    
    missing_res = sum(
      is.na(res)
    ),
    
    missing_sep_quintile = sum(
      is.na(sep_quintile)
    ),
    
    missing_vitamin_a = sum(
      is.na(vita_inadequate)
    ),
    
    missing_folate = sum(
      is.na(folate_inadequate)
    ),
    
    missing_vitamin_b12 = sum(
      is.na(vitb12_inadequate)
    ),
    
    missing_zinc = sum(
      is.na(zn_inadequate)
    ),
    
    missing_iron = sum(
      is.na(fe_prob_inadequate)
    )
  )

print(data_quality_summary)


# ------------------------------------------------------------------------------
# 15. DISPLAY GROUPING CATEGORIES
# ------------------------------------------------------------------------------

cat("\nADM1 categories:\n")

print(
  sort(
    unique(
      analysis_data$adm1
    )
  )
)


cat("\nResidence categories:\n")

print(
  sort(
    unique(
      analysis_data$res
    )
  )
)


cat("\nSEP quintile categories:\n")

print(
  sort(
    unique(
      analysis_data$sep_quintile
    )
  )
)

analysis_data <- analysis_data %>%
  mutate(
    compliance_label = factor(
      compliance_label,
      levels = c(
        "0%",
        "50%",
        "85%",
        "90%",
        "100%"
      ),
      ordered = TRUE
    )
  )


# ------------------------------------------------------------------------------
# 16. CREATE THE SURVEY DESIGN, use EAs ,Cluster etc if avaialable
# ------------------------------------------------------------------------------

analysis_svy <- analysis_data %>%
  
  filter(
    
    !is.na(survey_wgt),
    
    survey_wgt > 0
  ) %>%
  
  as_survey_design(
    
    ids = 1,
    
    weights = survey_wgt
  )


# ------------------------------------------------------------------------------
# 17. REUSABLE FUNCTION FOR MICRONUTRIENT INADEQUACY ESTIMATES
# ------------------------------------------------------------------------------

estimate_mn_inadequacy <- function(
    
  survey_object,
  
  group_vars = NULL
  
) {
  
  
  # --------------------------------------------------------------------------
  # Apply grouping if requested
  # --------------------------------------------------------------------------
  
  if (!is.null(group_vars)) {
    
    survey_object <- survey_object %>%
      
      group_by(
        
        across(
          all_of(group_vars)
        )
      )
  }
  
  
  # --------------------------------------------------------------------------
  # Calculate weighted prevalence
  # --------------------------------------------------------------------------
  
  result <- survey_object %>%
    
    summarise(
      
      vita_inadequacy = survey_mean(
        
        vita_inadequate,
        
        na.rm = TRUE,
        
        vartype = NULL
      ),
      
      folate_inadequacy = survey_mean(
        
        folate_inadequate,
        
        na.rm = TRUE,
        
        vartype = NULL
      ),
      
      vitb12_inadequacy = survey_mean(
        
        vitb12_inadequate,
        
        na.rm = TRUE,
        
        vartype = NULL
      ),
      
      zn_inadequacy = survey_mean(
        
        zn_inadequate,
        
        na.rm = TRUE,
        
        vartype = NULL
      ),
      
      # Mean probability equals estimated iron inadequacy prevalence
      
      fe_inadequacy = survey_mean(
        
        fe_prob_inadequate,
        
        na.rm = TRUE,
        
        vartype = NULL
      )
      
    ) %>%
    
    mutate(
      
      across(
        
        c(
          vita_inadequacy,
          folate_inadequacy,
          vitb12_inadequacy,
          zn_inadequacy,
          fe_inadequacy
        ),
        
        ~ round(
          .x * 100,
          digits = 0
        )
      )
    )
  
  
  result
}


# ==============================================================================
# 18. NATIONAL RESULTS BY COMPLIANCE
# ==============================================================================

national_mn_inadequacy <- estimate_mn_inadequacy(
  survey_object = analysis_svy,
  group_vars = c(
    "compliance",
    "compliance_label"
  )
) %>%
  mutate(
    disaggregation = "National",
    category = "National",
    .before = 1
  ) %>%
  arrange(
    compliance
  )

print(
  national_mn_inadequacy,
  n = Inf
)


# ==============================================================================
# 19. RESULTS BY ADM1 AND COMPLIANCE
# ==============================================================================

mn_inadequacy_adm1 <- estimate_mn_inadequacy(
  survey_object = analysis_svy,
  group_vars = c(
    "compliance_label",
    "adm1"
  )
) %>%
  arrange(
    compliance_label,
    adm1
  )

print(
  mn_inadequacy_adm1,
  n = Inf
)

# ==============================================================================
# 20. RESULTS BY RESIDENCE AND COMPLIANCE
# ==============================================================================

mn_inadequacy_res <- estimate_mn_inadequacy(
  
  survey_object = analysis_svy,
  
  group_vars = c(
    "compliance_label",
    "res"
  )
  
) %>%
  
  arrange(
    compliance_label,
    res
  )

print(
  mn_inadequacy_res,
  n = Inf
)


# ==============================================================================
# 21. RESULTS BY SOCIOECONOMIC QUINTILE AND COMPLIANCE
# ==============================================================================

mn_inadequacy_sep_quintile <- estimate_mn_inadequacy(
  
  survey_object = analysis_svy,
  
  group_vars = c(
    "compliance_label",
    "sep_quintile"
  )
  
) %>%
  
  arrange(
    compliance_label,
    sep_quintile
  )

print(
  mn_inadequacy_sep_quintile,
  n = Inf
)

# # ==============================================================================
# # 22. OPTIONAL: SAVE RESULTS
# # ==============================================================================

write_csv(
  national_mn_inadequacy,
  file.path(
    output_folder,
    "ind_item_fortification_national.csv"
  )
)


write_csv(
  mn_inadequacy_adm1,
  file.path(
    output_folder,
    "item_fortification_adm1.csv"
  )
)


write_csv(
  mn_inadequacy_res,
  file.path(
    output_folder,
    "item_fortification_residence.csv"
  )
)


write_csv(
  mn_inadequacy_sep_quintile,
  file.path(
    output_folder,
    "item_fortification_sep_quintile.csv"
  )
)


