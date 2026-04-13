rm(list = ls())

rq_packages <- c(
  "tidyverse",
  "dplyr",
  "readr",
  "srvyr",
  "ggplot2",
  "tidyr",
  "ggridges",
  "gt",
  "haven",
  "foreign",
  "tmap",
  "sf",
  "rmapshaper",
  "readxl",
  "hrbrthemes",
  "wesanderson",
  "treemap",
  "treemapify"
)

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list = c("rq_packages", "installed_packages"))

source("functions/aggregated_inadequacy.R")
source("functions/general_inadequacy.R")
source("functions/get_har.R")
source("src/7_clean_for_db.R")


#---------------------------------------------------------------------------

# set paths
figure_path <- "figures/"
raw_path <- "data/raw/"
processed_path <- "data/processed/"

file_list = list.files("data/raw/HCES_2022_23/")
data_list <- lapply(
  paste0("data/raw/HCES_2022_23/", file_list),
  haven::read_dta
)
names(data_list) <- tools::file_path_sans_ext(file_list)

level04 <- data_list$level04
level03 <- data_list$level03

# household intake
hh_mn_intake <- readRDS(paste0(processed_path, "ind_nss2223_base_case.rds"))

# rearrange dfs
use_of_rc <- level04 |>
  rename(hhid = common_id)
rm(level04)

type_rc <- level03 |>
  rename(hhid = common_id) |>
  select(hhid, ration_card_type) |>
  mutate(
    ration_card_type = case_match(
      ration_card_type,
      '0' ~ "None",
      '1' ~ "AAY",
      '2' ~ "BPL",
      '3' ~ "APL",
      '4' ~ "PPH",
      '5' ~ "SFSS",
      '9' ~ "Others"
    )
  )

rm(level03)

##############################

type_rc |>
  # group_by(ration_card_type) |>
  # summarise(count = n()) |>
  ggplot(aes(x = ration_card_type)) +
  geom_bar()

use_of_rc |>
  select(hhid, ration_card_item_rice) |>
  ggplot(aes(ration_card_item_rice)) +
  geom_bar()

ration_card_total <- type_rc |>
  left_join(
    use_of_rc |>
      select(
        hhid,
        hh_used_ration_card_30days,
        ration_card_item_rice,
        ration_card_item_wheat
      ),
    by = 'hhid'
  ) |>
  mutate(
    hh_used_ration_card_30days = case_match(
      hh_used_ration_card_30days,
      '1' ~ "Yes",
      '2' ~ "No"
    ),
    ration_card_item_rice = case_match(
      ration_card_item_rice,
      '1' ~ "Yes",
      '' ~ "No"
    ),
    ration_card_item_wheat = case_match(
      ration_card_item_wheat,
      '1' ~ "Yes",
      '' ~ "No"
    )
  )

# look at
summary_data <- ration_card_total |>
  select(-hh_used_ration_card_30days) |>
  pivot_longer(
    cols = c(ration_card_item_rice, ration_card_item_wheat),
    names_to = "food_item_purchased",
    values_to = "used_ration_card"
  ) |>
  left_join(
    ind_nss2223_hh_info |> select(hhid, ea, res, survey_wgt),
    by = 'hhid'
  ) |>
  filter(!is.na(ea)) |>
  as_survey_design(ids = "ea", strata = "res", weights = "survey_wgt") |>
  group_by(ration_card_type, food_item_purchased) %>%
  summarize(
    access_rate = survey_mean(
      used_ration_card == "Yes",
      proportion = T,
      vartype = "ci"
    ) *
      100,
    .groups = "drop"
  )

summary_data |>
  mutate(
    food_item_purchased = ifelse(
      food_item_purchased == "ration_card_item_rice",
      "PDS Rice",
      "PDS Wheat"
    )
  ) |>
  ggplot(aes(
    x = food_item_purchased,
    y = access_rate,
    fill = ration_card_type
  )) +

  geom_bar(stat = "identity", position = "dodge") +
  geom_text(
    aes(label = round(access_rate, 0)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Access to Potentially Fortifiable Foods via Social Assistance",
    x = "Food Item",
    y = "Proportion (%)",
    fill = "Ration Card Type"
  ) +
  theme_minimal()

ggsave(
  paste0(figure_path, "ration_cards/ration_card_vehicle.png"),
  width = 8, # inches
  height = 5, # inches
  dpi = 300
)


# ------------------------------------------------------------------------

all_intake <- ind_nss2223_base_ai |>
  left_join(ind_nss2223_hh_info) |>
  left_join(ration_card_total) |>
  mutate(res_quintile = paste(res, res_quintile))

ear_table <- data.frame(
  nutrient = c("fe_mg", "folate_mcg", "vitb12_mcg"),
  ear_value = c(15, 180, 2)
)
# ear_table <- get_har() |>
#   filter(iso3 == "IND") |>
#   select(-iso3) |>
#   pivot_longer(cols = energy_kcal:vitb12_mcg, names_to = "nutrient", values_to = "ear_value")

ration_card_inad <- general_inadequacy(all_intake, ration_card_type, ear_table)

ration_card_inad_paper <- ration_card_inad |>
  rename(group = ration_card_type) |>
  select(
    group,
    prev_inad,
    prev_inad_se,
    folate_mcg_inad,
    folate_mcg_inad_se,
    vitb12_mcg_inad,
    vitb12_mcg_inad_se
  ) |>
  mutate(broad = "Ration card type")

# economic quintile

res_quin_inad <- general_inadequacy(all_intake, res_quintile, ear_table)

res_quin_inad <- res_quin_inad |>
  rename(group = res_quintile) |>
  select(
    group,
    prev_inad,
    prev_inad_se,
    folate_mcg_inad,
    folate_mcg_inad_se,
    vitb12_mcg_inad,
    vitb12_mcg_inad_se
  ) |>
  mutate(broad = "Economic Quintile")

# national
national_inad <- general_inadequacy(all_intake, national, ear_table)
national_inad <- national_inad |>
  mutate(group = "National") |>
  select(
    group,
    prev_inad,
    prev_inad_se,
    folate_mcg_inad,
    folate_mcg_inad_se,
    vitb12_mcg_inad,
    vitb12_mcg_inad_se
  ) |>
  mutate(broad = "")

# create a table with the data
table1 <- bind_rows(national_inad, res_quin_inad, ration_card_inad_paper)

table1 |>

  mutate(
    fe_display = paste0(round(prev_inad, 1), " (", round(prev_inad_se, 1), ")"),
    folate_inad_display = paste0(
      round(folate_mcg_inad, 1),
      " (",
      round(folate_mcg_inad_se, 1),
      ")"
    ),
    vitb12_display = paste0(
      round(vitb12_mcg_inad, 1),
      " (",
      round(vitb12_mcg_inad_se, 1),
      ")"
    )
  ) |>
  select(group, fe_display, folate_inad_display, vitb12_display, broad) |>
  gt(groupname_col = "broad") |>
  tab_header(
    title = "Risk of inadequate micronutrient intake ",
    subtitle = "Risk (%) by Economic group and ration card type"
  ) |>

  opt_table_font(
    font = list(
      google_font("Roboto"),
      default_fonts()
    )
  ) |>

  cols_label(
    group = "Population",
    fe_display = "Iron",
    folate_inad_display = "Folate",
    vitb12_display = "Vitamin B12"
  ) |>

  # fmt_percent() |>
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "gray",
      weight = px(2)
    ),
    locations = cells_body(
      rows = c(1, 11) # Apply thicker border after row 4
    )
  )


# fortification ------------------------------------------------------------

# FUNCTIONS ###
plot_inadequacy_slope <- function(
  data,
  group,
  nutrient,
  nutrient_name,
  title_prefix = "Change in",
  group_label = NULL,
  save_path = NULL
) {
  clean_data <- data %>%
    select({{ group }}, source, starts_with(nutrient)) %>%
    rename(
      value_Base = !!sym(nutrient),
      se_Base = !!sym(paste0(nutrient, "_se")),
      value_Fortified = !!sym(paste0(nutrient, "_fort")),
      se_Fortified = !!sym(paste0(nutrient, "_fort_se")),
      value_FortifiedWFP = !!sym(paste0(nutrient, "_fort_wfp")),
      se_FortifiedWFP = !!sym(paste0(nutrient, "_fort_wfp_se"))
    )

  long_data <- clean_data %>%
    pivot_longer(
      cols = -c({{ group }}, source),
      names_to = c(".value", "scenario"),
      names_sep = "_"
    )

  p <- ggplot(
    long_data,
    aes(
      x = scenario,
      y = value,
      group = interaction({{ group }}, source),
      color = {{ group }},
      shape = source
    )
  ) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_shape_manual(values = c("Rice" = 16, "Wheat Flour" = 17)) +
    scale_color_brewer(palette = "Set2") +
    geom_errorbar(aes(ymin = value - se, ymax = value + se), width = 0.1) +
    geom_text(aes(label = round(value, 1)), vjust = -0.8, size = 3) +
    theme_minimal() +
    labs(
      title = paste(title_prefix, nutrient_name, "Inadequacy Across Scenarios"),
      x = "Scenario",
      y = "% Inadequacy",
      color = group_label %||% rlang::as_name(enquo(group)),
      shape = "Fortification Source"
    ) +
    theme(legend.position = "bottom")

  if (!is.null(save_path)) {
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
    ggsave(
      filename = paste0(save_path, "/", nutrient_name, "_slope_chart.png"),
      plot = p,
      width = 8,
      height = 6
    )
  }

  return(p)
}

# DATA

hh_rice_fort <- readRDS("data/processed/ind_fort_rice.rds")
hh_wf_fort <- readRDS("data/processed/ind_fort_wf_v2.rds")


hh_rice_fort <- hh_rice_fort %>%
  mutate(source = "Rice")

hh_wf_fort <- hh_wf_fort %>%
  mutate(source = "Wheat Flour")


hh_rice_fort <- hh_rice_fort |>
  select(
    common_id,
    folate_ug,
    iron_mg,
    vitaminb12_in_mcg,
    fe_mg_fort,
    folate_mcg_fort,
    vitb12_mcg_fort,
    fe_mg_fort_wfp,
    folate_mcg_fort_wfp,
    vitb12_mcg_fort_wfp
  ) |>
  rename(
    hhid = common_id,
    fe_mg = iron_mg,
    folate_mcg = folate_ug,
    vitb12_mcg = vitaminb12_in_mcg
  ) |>
  left_join(ind_nss2223_hh_info) |>
  left_join(ration_card_total) |>
  mutate(res_quintile = paste(res, res_quintile))


hh_wf_fort <- hh_wf_fort %>%
  select(
    common_id,
    folate_ug,
    iron_mg,
    vitaminb12_in_mcg,
    fe_mg_fort,
    folate_mcg_fort,
    vitb12_mcg_fort,
    fe_mg_fort_wfp,
    folate_mcg_fort_wfp,
    vitb12_mcg_fort_wfp
  ) %>%
  rename(
    hhid = common_id,
    fe_mg = iron_mg,
    folate_mcg = folate_ug,
    vitb12_mcg = vitaminb12_in_mcg
  ) |>
  left_join(ind_nss2223_hh_info) |>
  left_join(ration_card_total) |>
  mutate(res_quintile = paste(res, res_quintile))


# Nutrients and names

nutrients <- c("folate_mcg_inad", "vitb12_mcg_inad", "fe_mg_inad")
nutrient_names <- c("Folate", "Vitamin_B12", "Iron")


# For ration_card_type
plots_rc <- Map(
  function(nutrient, name) {
    plot_inadequacy_slope(
      rc_inad_fort,
      group = ration_card_type,
      nutrient,
      name,
      group_label = "Ration Card Type",
      save_path = "figures/ration_cards"
    )
  },
  nutrients,
  nutrient_names
)
# For res_quintile
plots_rq <- Map(
  function(nutrient, name) {
    plot_inadequacy_slope(
      res_quintile_inad_fort,
      group = res_quintile,
      nutrient,
      name,
      save_path = "figures/ration_cards"
    )
  },
  nutrients,
  nutrient_names
)


rc_inad_rice <- general_inadequacy(
  hh_rice_fort,
  ration_card_type,
  ear_table,
  scenarios = c("", "_fort", "_fort_wfp")
) %>%
  mutate(source = "Rice")

rc_inad_wf <- general_inadequacy(
  hh_wf_fort,
  ration_card_type,
  ear_table,
  scenarios = c("", "_fort", "_fort_wfp")
) %>%
  mutate(source = "Wheat Flour")

rc_inad_combined <- bind_rows(rc_inad_rice, rc_inad_wf)


plots_rc <- Map(
  function(nutrient, name) {
    plot_inadequacy_slope(
      rc_inad_combined,
      group = ration_card_type,
      nutrient,
      name,
      save_path = "figures/ration_cards/combined"
    )
  },
  nutrients,
  nutrient_names
)


plots_rc <- Map(
  function(nutrient, name) {
    plot_inadequacy_slope(
      rc_inad_wf,
      group = ration_card_type,
      nutrient,
      name,
      group_label = "Ration Card Type",
      save_path = "figures/ration_cards"
    )
  },
  nutrients,
  nutrient_names
)

# national_inad_fort <- general_inadequacy(
#   hh_rice_fort,
#   national,
#   ear_table,
#   scenarios = c("", "_fort", "_fort_wfp")
# )

# res_quintile_inad_fort <- general_inadequacy(
#   hh_rice_fort,
#   res_quintile,
#   ear_table,
#   scenarios = c("", "_fort", "_fort_wfp")
# )

# rc_inad_fort <- general_inadequacy(
#   hh_rice_fort,
#   ration_card_type,
#   ear_table,
#   scenarios = c("", "_fort", "_fort_wfp")
# )
