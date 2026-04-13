#########################################
#      HCES 2022-23 INDIA               #
#         Figures                       #
#########################################


# Author: Gabriel Battcock
# Created:
# Last updated: 21 May 2025

rq_packages <-
  c(
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
    "treemapify",
    'ggtext',
    'openxlsx'
  )

installed_packages <-
  rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list = c("rq_packages", "installed_packages"))
################################################################################

processed_path <- "data/processed/"
figure_path <- "figures/"

india_rice_inad <-
  read.csv(paste0(processed_path, "india_rice_inad.csv"))
india_wf_inad <-
  read.csv(paste0(processed_path, "india_wf_inad_v2.csv"))
india_rice_comm <-
  read.csv(paste0(processed_path, "india_rice_com_inad.csv"))


rice <- india_rice_inad %>%
  select(
    category,
    folate_inad,
    folate_inad_fort,
    folate_inad_fort_wfp,
    vitb12_inad,
    vitb12_inad_fort,
    vitb12_inad_fort_wfp,
    fe_inad,
    fe_inad_fort,
    fe_inad_fort_wfp
  ) %>%
  rename(
    folate_rice_ind = folate_inad_fort,
    folate_rice_int = folate_inad_fort_wfp,
    vitb12_rice_ind = vitb12_inad_fort,
    vitb12_rice_int = vitb12_inad_fort_wfp,
    fe_rice_ind = fe_inad_fort,
    fe_rice_int = fe_inad_fort_wfp
  ) %>%
  filter(category %in% c("08", "09", "10", "22", "national")) %>%
  mutate(
    category =
      case_when(
        category == "08" ~ "Rajasthan",
        category == "09" ~ "Uttar Pradesh",
        category == "10" ~ "Bihar",
        category == "22" ~ "Chhattisgarh",
        category == "national" ~ "National"
      )
  )

rice_manita <- india_rice_inad %>%
  select(
    category,
    folate_inad,
    folate_inad_fort,
    # folate_inad_fort_wfp,
    vitb12_inad,
    vitb12_inad_fort,
    # vitb12_inad_fort_wfp,
    fe_inad,
    fe_inad_fort
    # fe_inad_fort_wfp
  ) %>%
  rename(
    folate_rice_ind = folate_inad_fort,
    # folate_rice_int = folate_inad_fort_wfp,
    vitb12_rice_ind = vitb12_inad_fort,
    # vitb12_rice_int = vitb12_inad_fort_wfp,
    fe_rice_ind = fe_inad_fort
    # fe_rice_int = fe_inad_fort_wfp
  ) %>%
  filter(category %in% c("08", "09", "10", "22", "national")) %>%
  mutate(
    category =
      case_when(
        category == "08" ~ "Rajasthan",
        category == "09" ~ "Uttar Pradesh",
        category == "10" ~ "Bihar",
        category == "22" ~ "Chhattisgarh",
        category == "national" ~ "National"
      )
  )

only_wheat_comparison <-
india_rice_inad %>%
  select(category, folate_inad_fort,
         vitb12_inad_fort,
         fe_inad_fort) %>%
  rename(
    folate_inad = folate_inad_fort,
    vitb12_inad = vitb12_inad_fort,
    fe_inad = fe_inad_fort
  ) %>%
  left_join(
    india_wf_inad %>%
      select(
        category,

        folate_inad_fort,
        folate_inad_fort_wfp,

        vitb12_inad_fort,
        vitb12_inad_fort_wfp,

        fe_inad_fort,
        fe_inad_fort_wfp
      ),
    by = "category"
  ) %>% 
  rename(
    folate_wheat_ind = folate_inad_fort,
    folate_wheat_int = folate_inad_fort_wfp,
    vitb12_wheat_ind = vitb12_inad_fort,
    vitb12_wheat_int = vitb12_inad_fort_wfp,
    fe_wheat_ind = fe_inad_fort,
    fe_wheat_int = fe_inad_fort_wfp
  ) %>%
  filter(category %in% c("08", "09", "03", "06","23", "national")) %>%
  mutate(
    category =
      case_when(
        category == "08" ~ "Rajasthan",
        category == "09" ~ "Uttar Pradesh",
        category == "03" ~ "Punjab",
        category == "06" ~ "Haryana",
        category == "23" ~ "Madhya Pradesh",
        category == "national" ~ "National"
      )
  )




wheat <- india_wf_inad %>%
  select(
    category,
    folate_inad,
    folate_inad_fort,
    folate_inad_fort_wfp,
    vitb12_inad,
    vitb12_inad_fort,
    vitb12_inad_fort_wfp,
    fe_inad,
    fe_inad_fort,
    fe_inad_fort_wfp
  ) %>%
  rename(
    folate_wheat_ind = folate_inad_fort,
    folate_wheat_int = folate_inad_fort_wfp,
    vitb12_wheat_ind = vitb12_inad_fort,
    vitb12_wheat_int = vitb12_inad_fort_wfp,
    fe_wheat_ind = fe_inad_fort,
    fe_wheat_int = fe_inad_fort_wfp
  ) %>%
  filter(category %in% c("08", "09", "03", "06","23", "national")) %>%
  mutate(
    category =
      case_when(
        category == "08" ~ "Rajasthan",
        category == "09" ~ "Uttar Pradesh",
        category == "03" ~ "Punjab",
        category == "06" ~ "Haryana",
        category == "23" ~ "Madhya Pradesh",
        category == "national" ~ "National"
      )
  )

both_vehicles <-  rice %>%
  left_join(wheat, by = 'category')


# write these as an excel for remaking

wb <- createWorkbook()

addWorksheet(wb, "figure3")
addWorksheet(wb, "figure4")

writeData(wb, "figure3", rice)
writeData(wb, "figure4", wheat)

saveWorkbook(wb,
             paste0(figure_path, "bar_plots/bar_plots.xlsx"),
             overwrite = TRUE)



wb <- createWorkbook()

addWorksheet(wb, "all_rice")
addWorksheet(wb, "all_wheat")

writeData(wb, "all_rice", rice_manita)
writeData(wb, "all_wheat", only_wheat_comparison)

saveWorkbook(wb,
             paste0(figure_path, "bar_plots/briefs/bar_plots.xlsx"),
             overwrite = TRUE)



# Plot function ################################################################
#
plot_fortification_reduction_vertical <- function(df, micronutrient) {
  df_long <- df %>%
    select(category, starts_with(micronutrient)) %>%
    pivot_longer(
      cols = starts_with(paste0(micronutrient, "_")),
      names_to = "scenario",
      values_to = "prevalence"
    ) %>%
    mutate(
      vehicle = case_when(
        grepl("rice", scenario) ~ "Rice",
        grepl("wheat", scenario) ~ "Wheat and rice",
        TRUE ~ "No Fortification"
      ),
      standard = case_when(
        grepl("ind", scenario) ~ "Indian",
        grepl("int", scenario) ~ "International",
        TRUE ~ "Base Case"
      ),
      scenario_label = paste(standard, vehicle, sep = " - ")
    )

  base_case <- df_long %>%
    filter(standard == "Base Case") %>%
    select(category, base_prevalence = prevalence)

  df_long <- df_long %>%
    left_join(base_case, by = "category") %>%
    mutate(
      reduction = base_prevalence - prevalence,
      reduction_pct = round((reduction / base_prevalence) * 100, 0)
    )

  unique_scenarios <- unique(df_long$scenario_label)
  df_long$scenario_label <- factor(df_long$scenario_label, levels = sort(unique_scenarios))
  
  print(unique_scenarios)

  num_scenarios <- length(unique_scenarios)
  dodge_width <- 0.95
  offset_step <- dodge_width / (num_scenarios + 1)

  custom_colors <- c(
    "Base Case - No Fortification" = "grey80",
    "Indian - Rice" = "#3A9AA0",
    "Indian - Wheat and rice" = "#E98905",
    "International - Rice" = "#9BBDAC",
    "International - Wheat and rice" = "#DFC12F"
  )

  scenario_colors <- setNames(
    custom_colors[unique_scenarios],
    unique_scenarios
  )

  df_long$category <- factor(df_long$category, levels = c("National", setdiff(unique(df_long$category), "National")))

  ggplot(df_long, aes(x = category, y = prevalence, fill = scenario_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = dodge_width)) +
    ylim(0, 75) +
    geom_text(
      aes(label = paste0(round(prevalence, 0), "%"), y = 1),
      color = "black",
      size = 3.8,
      position = position_dodge(width = dodge_width),
      vjust = 0
    ) +
    geom_segment(
      data = df_long %>% filter(reduction_pct > 0),  # Exclude zero reductions
      aes(
        x = as.numeric(as.factor(category)) + (as.numeric(as.factor(scenario_label)) - (num_scenarios / 2 + 0.5)) * offset_step,
        xend = as.numeric(as.factor(category)) + (as.numeric(as.factor(scenario_label)) - (num_scenarios / 2 + 0.5)) * offset_step,
        y = base_prevalence,
        yend = prevalence
      ),
      arrow = arrow(length = unit(0.2, "cm")),
      color = "black"
    ) +
    geom_text(
      data = df_long %>% filter(reduction_pct > 0),  # Exclude zero reductions
      aes(
        x = as.numeric(as.factor(category)) + (as.numeric(as.factor(scenario_label)) - (num_scenarios / 2 + 0.5)) * offset_step,
        y = base_prevalence + 2.5,
        label = paste0(reduction_pct, "%")
      ),
      size = 3.9, color = "black"
    ) +
    labs(
      title = paste(
        case_when(
          micronutrient == "vitb12" ~ "Vitamin B12",
          micronutrient == "folate" ~ "Folate",
          micronutrient == 'fe' ~ "Iron"
        )
      ),
      x = "", y = "Risk of inadequate intake (%)",
      fill = "Fortification Scenario"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 14)) +
    scale_fill_manual(values = scenario_colors) +
    theme_ipsum(axis_title_size = 15.5, grid =  FALSE)

}


plot_fortification_reduction_horizontal <- function(df, micronutrient) {

  # Reshape data
  df_long <- df %>%
    select(category, starts_with(micronutrient)) %>%
    pivot_longer(
      cols = starts_with(paste0(micronutrient, "_")),
      names_to = "scenario",
      values_to = "prevalence"
    ) %>%
    mutate(
      vehicle = case_when(
        grepl("rice", scenario) ~ "Rice",
        grepl("wheat", scenario) ~ "Wheat and rice",
        TRUE ~ "No Fortification"
      ),
      standard = case_when(
        grepl("ind", scenario) ~ "Indian",
        grepl("int", scenario) ~ "International",
        TRUE ~ "Base Case"
      ),
      scenario_label = paste(standard, vehicle, sep = " - ")
    )
  
  # Get base case
  base_case <- df_long %>%
    filter(standard == "Base Case") %>%
    select(category, base_prevalence = prevalence)
  
  df_long <- df_long %>%
    left_join(base_case, by = "category") %>%
    mutate(
      reduction = base_prevalence - prevalence,
      reduction_pct = round((reduction / base_prevalence) * 100, 0)
    )
  
  scenario_order <- df_long %>%
    distinct(scenario_label) %>%
    pull(scenario_label) %>%
    sort() %>%  # just to be safe
    { c(
      
      .[grepl("International", .)],
      .[grepl("Indian", .)],
      .[grepl("Base Case", .)]
    ) }
  
  # Order categories by max prevalence
  category_order <- df_long %>%
    distinct(category) %>%
    pull(category) %>%
    setdiff("National") %>%
    c("National")
  
  # Set factors
  df_long <- df_long %>%
    mutate(
      category = factor(category, levels = category_order),
      scenario_label = factor(scenario_label, levels = scenario_order)
    )

  # Dodge config
  dodge_width <- 0.9
  num_scenarios <- n_distinct(df_long$scenario_label)
  offset_step <- dodge_width / (num_scenarios + 1)
  
  # Manual dodge y
  df_long <- df_long %>%
    mutate(
      category_numeric = as.numeric(category),
      scenario_numeric = as.numeric(scenario_label),
      dodge_y = category_numeric + (scenario_numeric - (num_scenarios / 2 + 0.5)) * offset_step
    )
  
  # Custom colors
  
  custom_colors <- c(
    "Base Case - No Fortification" = "grey80",
    "Indian - Rice" = "#3A9AA0",
    "Indian - Wheat and rice" = "#E98905",
    "International - Rice" = "#9BBDAC",
    "International - Wheat and rice" = "#DFC12F"
  )
  
  scenario_colors <- setNames(
    custom_colors[scenario_order],
    scenario_order
  )
  
  # Plot
  ggplot(df_long, aes(x = prevalence, y = category, fill = scenario_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = dodge_width)) +
    geom_text(
      aes(label = paste0(round(prevalence, 0), "%"),
          x = 0.5,
          y = category),
      color = "black",
      size = 3.8,
      position = position_dodge(width = dodge_width),
      hjust = 0
    ) +
    geom_segment(
      aes(
        x = base_prevalence,
        xend = ifelse(reduction_pct > 0, prevalence, base_prevalence),
        y = dodge_y,
        yend = dodge_y
      ),
      arrow = arrow(length = unit(0.2, "cm")),
      color = ifelse(df_long$reduction_pct > 0, "black", NA)
    ) +
    geom_text(
      aes(
        x = base_prevalence + 2.5,
        y = dodge_y,
        label = ifelse(reduction_pct > 0, paste0(reduction_pct, "%"), "")
      ),
      size = 3.9,
      color = "black"
    ) +
    # coord_flip() +
    labs(
      title = case_when(
        micronutrient == "vitb12" ~ "Vitamin B12",
        micronutrient == "folate" ~ "Folate",
        micronutrient == "fe" ~ "Iron",
        TRUE ~ tools::toTitleCase(micronutrient)
      ),
      x = "Risk of inadequate intake (%)", y = NULL,
      fill = "Fortification Scenario"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14)
    ) +
    scale_fill_manual(values = scenario_colors) +
    theme_ipsum(axis_title_size = 15.5, grid = FALSE)
}




# make the plots for rice only #################################################

# Generate plots for 3 different micronutrients
p1 <-
  plot_fortification_reduction(rice, "fe") #+ theme(legend.position = "none")  # Remove individual legends
p2 <-
  plot_fortification_reduction(rice, "vitb12") #+ theme(legend.position = "none")
p3 <-
  plot_fortification_reduction(rice, "folate") #+ theme(legend.position = "none")

ggsave(
  plot = p1,
  filename = paste0(figure_path, "bar_plots/iron.jpg"),
  dpi = 900,
  height = 8,
  width = 15
)
ggsave(
  plot = p2,
  filename = paste0(figure_path, "bar_plots/vitb12.jpg"),
  dpi = 900,
  height = 8,
  width = 15
)
ggsave(
  plot = p3,
  filename = paste0(figure_path, "bar_plots/folate.jpg"),
  dpi = 900,
  height = 8,
  width = 15
)


# legend <- cowplot::get_legend(plot_fortification_reduction(rice, "fe") + theme(legend.position = "bottom"))
# ggplotify::as.ggplot(legend)

# Combine using patchwork and add a common legend
final_plot <-
  ggpubr::ggarrange(p1, p2, p3,  # The three plots  # Arrange in one row
                    # nrow = 2,
                    common.legend = TRUE,
                    legend = 'bottom')

ggpubr::annotate_figure(
  final_plot,
  top = ggpubr::text_grob(
    "PDS-distributed rice",
    color = "black",
    face = "bold",
    size = 18
  )
)
# ggpubr::as_ggplot( rice)
#
# path_to_save <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/India/Report/figures/"
# ggpubr::ggexport(rice, paste0(path_to_save,"rice_barchart.png"),
#                     width = 1900, height = 1100, units = "px")


# make the plots for both vehicles #############################################
p1 <-
  plot_fortification_reduction(both_vehicles, "fe") #+ theme(legend.position = "none")  # Remove individual legends
p2 <-
  plot_fortification_reduction(both_vehicles, "vitb12") #+ theme(legend.position = "none")
p3 <-
  plot_fortification_reduction(both_vehicles, "folate") #+ theme(legend.position = "none")



ggsave(
  plot = p1,
  filename = paste0(figure_path, "bar_plots/iron_both.jpg"),
  dpi = 900,
  height = 8,
  width = 15
)
ggsave(
  plot = p2,
  filename = paste0(figure_path, "bar_plots/vitb12_both.jpg"),
  dpi = 900,
  height = 8,
  width = 15
)
ggsave(
  plot = p3,
  filename = paste0(figure_path, "bar_plots/folate_both.jpg"),
  dpi = 900,
  height = 8,
  width = 15
)
# legend <- cowplot::get_legend(plot_fortification_reduction(rice, "fe") + theme(legend.position = "bottom"))
# ggplotify::as.ggplot(legend)


# make the plots for rice only India standards #################################
p1 <-
  plot_fortification_reduction_vertical(rice_manita, "fe") #+ theme(legend.position = "none")  # Remove individual legends
p2 <-
  plot_fortification_reduction_vertical(rice_manita, "vitb12") #+ theme(legend.position = "none")
p3 <-
  plot_fortification_reduction_vertical(rice_manita, "folate") #+ theme(legend.position = "none")

ggsave(
  plot = p1,
  filename = paste0(figure_path, "bar_plots/briefs/iron_rice_brief.jpg"),
  dpi = 900,
  height = 8,
  width = 15
)
ggsave(
  plot = p2,
  filename = paste0(figure_path, "bar_plots/briefs/vitb12_rice_brief.jpg"),
  dpi = 900,
  height = 8,
  width = 15
)
ggsave(
  plot = p3,
  filename = paste0(figure_path, "bar_plots/briefs/folate_rice_brief.jpg"),
  dpi = 900,
  height = 8,
  width = 15
)

final_plot <-
  ggpubr::ggarrange(p1, p2, p3,  # The three plots  # Arrange in one row
                    nrow = 1,
                    common.legend = TRUE,
                    legend = 'bottom')
all_fig <- ggpubr::annotate_figure(
  final_plot,
  top = ggpubr::text_grob(
    "PDS-distributed rice",
    color = "black",
    face = "bold",
    size = 18
  ),
  bottom = ggpubr::text_grob(
    "Source: NSS HCES 2022-23",
    hjust = -1,
    color = "black",
    size = 12)
)

ggsave(
  paste0(figure_path,"bar_plots/briefs/all_rice_brief.jpg"),
  plot = all_fig,
  device = "jpg",
  height = 11,
  width = 21,
  dpi = 900,
  units = "in"
)


###############################################################################

# Generate plots for 3 different micronutrients
p1 <-
  plot_fortification_reduction_vertical(only_wheat_comparison, "fe") #+ theme(legend.position = "none")  # Remove individual legends
p2 <-
  plot_fortification_reduction_vertical(only_wheat_comparison, "vitb12") #+ theme(legend.position = "none")
p3 <-
  plot_fortification_reduction_vertical(only_wheat_comparison, "folate") #+ theme(legend.position = "none")

ggsave(
  plot = p1,
  filename = paste0(figure_path, "bar_plots/briefs/iron_wheat_brief.jpg"),
  dpi = 900,
  height = 8,
  width = 15
)
ggsave(
  plot = p2,
  filename = paste0(figure_path, "bar_plots/briefs/vitb12_wheat_brief.jpg"),
  dpi = 900,
  height = 8,
  width = 15
)
ggsave(
  plot = p3,
  filename = paste0(figure_path, "bar_plots/briefs/folate_wheat_brief.jpg"),
  dpi = 900,
  height = 8,
  width = 15
)

final_plot <-
  ggpubr::ggarrange(p1, p2, p3,  # The three plots  # Arrange in one row
                    nrow = 1,
                    common.legend = TRUE,
                    legend = 'bottom')
all_fig <- ggpubr::annotate_figure(
  final_plot,
  top = ggpubr::text_grob(
    "PDS-distributed wheat and rice",
    color = "black",
    face = "bold",
    size = 18
  ),
  bottom = ggpubr::text_grob(
    "Source: NSS HCES 2022-23",
    hjust = -1,
    color = "black",
    size = 12)
)

ggsave(
  paste0(figure_path,"bar_plots/briefs/all_wheat_brief.jpg"),
  plot = all_fig,
  device = "jpg",
  height = 11,
  width = 27,
  dpi = 900,
  units = "in"
)










# rm(list = ls())

## END OF SCRIPT ##
