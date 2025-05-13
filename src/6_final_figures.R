#########################################
#      HCES 2022-23 INDIA               #
#         Figures                       #
#########################################


# Author: Gabriel Battcock
# Created: 
# Last updated: 29 April 24

rq_packages <- c("tidyverse","dplyr","readr","srvyr","ggplot2", "tidyr",
                 "ggridges", "gt", "haven","foreign",
                 "tmap","sf","rmapshaper","readxl","hrbrthemes",
                 "wesanderson","treemap","treemapify", 'ggtext')

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))
################################################################################

india_rice_inad <- read.csv("india_rice_inad.csv")
india_wf_inad <- read.csv("india_wf_inad_v2.csv")
india_rice_comm <-  read.csv("india_rice_com_inad.csv")


rice <- india_rice_inad %>%
  select(category, folate_inad, folate_inad_fort, folate_inad_fort_wfp,
         vitb12_inad, vitb12_inad_fort, vitb12_inad_fort_wfp,
         fe_inad, fe_inad_fort, fe_inad_fort_wfp) %>% 
  rename(folate_rice_ind = folate_inad_fort,
         folate_rice_int = folate_inad_fort_wfp,
         vitb12_rice_ind = vitb12_inad_fort,
         vitb12_rice_int = vitb12_inad_fort_wfp,
         fe_rice_ind = fe_inad_fort,
         fe_rice_int = fe_inad_fort_wfp) %>% 
  filter(category %in% c("08", "09", "10", "22", "national")) %>% 
  mutate(category = 
           case_when(category == "08" ~ "Rajasthan", 
                     category == "09" ~ "Uttar Pradesh",
                     category == "10" ~ "Bihar",
                     category == "22" ~ "Chhattisgarh",
                     category == "national" ~ "National"))

wheat <- india_wf_inad %>%
  select(category, folate_inad, folate_inad_fort, folate_inad_fort_wfp,
         vitb12_inad, vitb12_inad_fort, vitb12_inad_fort_wfp,
         fe_inad, fe_inad_fort, fe_inad_fort_wfp) %>% 
  rename(folate_wheat_ind = folate_inad_fort,
         folate_wheat_int = folate_inad_fort_wfp,
         vitb12_wheat_ind = vitb12_inad_fort,
         vitb12_wheat_int = vitb12_inad_fort_wfp,
         fe_wheat_ind = fe_inad_fort,
         fe_wheat_int = fe_inad_fort_wfp)%>% 
  filter(category %in% c("08", "09", "10", "22", "national")) %>% 
  mutate(category = 
           case_when(category == "08" ~ "Rajasthan", 
                     category == "09" ~ "Uttar Pradesh",
                     category == "10" ~ "Bihar",
                     category == "22" ~ "Chhattisgarh",
                     category == "national" ~ "National"))

both_vehicles <-  rice %>% 
  left_join(wheat, by= 'category') 

plot_fortification_reduction <- function(df, micronutrient) {
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
  
  num_scenarios <- length(unique_scenarios)
  dodge_width <- 0.95
  offset_step <- dodge_width / (num_scenarios + 1)
  
  custom_colors <- c(
    "Indian - Rice" = "#27AE60",
    "Indian - Wheat and rice" = "#3498DB",
    "International - Rice" = "#145A32",
    "International - Wheat and rice" = "#2C3E50"
  )
  
  scenario_colors <- setNames(
    custom_colors[unique_scenarios],
    unique_scenarios
  )
  
  df_long$category <- factor(df_long$category, levels = c("National", setdiff(unique(df_long$category), "National")))
  
  ggplot(df_long, aes(x = category, y = prevalence, fill = scenario_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = dodge_width)) +
    ylim(0, 100) +
    geom_text(
      aes(label = paste0(round(prevalence, 0), "%"), y = 1),
      color = "white",
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
    scale_fill_manual(values = scenario_colors) + theme_ipsum(axis_title_size = 15.5)
}


# Example usage
plot_fortification_reduction(rice, "fe")
plot_fortification_reduction(rice, "vitb12")
plot_fortification_reduction(rice, "folate")

plot_fortification_reduction(both_vehicles, "fe")
plot_fortification_reduction(both_vehicles, "vitb12")
plot_fortification_reduction(both_vehicles, "folate")

plot_fortification_reduction(wheat, "fe")
plot_fortification_reduction(wheat, "vitb12")
plot_fortification_reduction(wheat, "folate")



# Generate plots for 3 different micronutrients
p1 <- plot_fortification_reduction(rice, "fe") #+ theme(legend.position = "none")  # Remove individual legends
p2 <-plot_fortification_reduction(rice, "vitb12") #+ theme(legend.position = "none")
p3 <- plot_fortification_reduction(rice, "folate") #+ theme(legend.position = "none")


# legend <- cowplot::get_legend(plot_fortification_reduction(rice, "fe") + theme(legend.position = "bottom"))
# ggplotify::as.ggplot(legend)

# Combine using patchwork and add a common legend
final_plot <- ggpubr::ggarrange(
  p1, p2, p3,  # The three plots  # Arrange in one row
  # nrow = 2,
 common.legend = TRUE, 
 legend = 'bottom'

)

ggpubr::annotate_figure(final_plot, top = ggpubr::text_grob("PDS-distributed rice", 
                                      color = "black", face = "bold", size = 18))
# ggpubr::as_ggplot( rice)
# 
# path_to_save <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/India/Report/figures/"
# ggpubr::ggexport(rice, paste0(path_to_save,"rice_barchart.png"),
#                     width = 1900, height = 1100, units = "px")


# Generate plots for 3 different micronutrients
p1 <- plot_fortification_reduction(both_vehicles, "fe") #+ theme(legend.position = "none")  # Remove individual legends
p2 <-plot_fortification_reduction(both_vehicles, "vitb12") #+ theme(legend.position = "none")
p3 <- plot_fortification_reduction(both_vehicles, "folate") #+ theme(legend.position = "none")


# legend <- cowplot::get_legend(plot_fortification_reduction(rice, "fe") + theme(legend.position = "bottom"))
# ggplotify::as.ggplot(legend)

# Combine using patchwork and add a common legend
final_plot <- ggpubr::ggarrange(
  p1, p2, p3,  # The three plots  # Arrange in one row
  # nrow = 2,
  common.legend = TRUE, 
  legend = 'bottom'
  
)

 ggpubr::annotate_figure(final_plot, top = ggpubr::text_grob("PDS-distributed wheat and rice", 
                                                            color = "black", face = "bold", size = 18))

ggsave("plot.svg", plot = p2, device = "svg",width = 2000, height = 900, units = "px")
