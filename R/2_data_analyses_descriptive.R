library(tidyverse)

# Data analyses process for epiData ############################################
df_epi <- read.csv("inputs/epiData_with_final_pneumo_decision.csv") %>% 
  dplyr::select(-X, -specimen_id) %>% 
  # conduct corrections for supposedly NUMERICAL and FACTOR (not ordered) columns!
  dplyr::mutate(
    age_month = as.numeric(age_month),
    age_year = as.numeric(age_year),
    nTotal_people = as.numeric(nTotal_people),
    nTotal_child_5yo_andBelow = as.numeric(nTotal_child_5yo_andBelow),
    n_child_1yo_andBelow = as.numeric(n_child_1yo_andBelow),
    n_child_1to2yo = as.numeric(n_child_1to2yo),
    n_child_2to4yo = as.numeric(n_child_2to4yo),
    nTotal_child_5yo_andBelow_sleep = as.numeric(nTotal_child_5yo_andBelow_sleep),
    hospitalised_last_3mo_n = as.numeric(hospitalised_last_3mo_n),
    healthcareVisit_last_3mo_n = as.numeric(healthcareVisit_last_3mo_n), # 1 "unknown" is replaced as NA
    # healthcareVisit_last_3mo_n = dplyr::na_if(healthcareVisit_last_3mo_n, "unknown")
    
    vaccination_hibpentavalent_dc_n = as.numeric(vaccination_hibpentavalent_dc_n),
    vaccination_pcv13_dc_n = as.numeric(vaccination_pcv13_dc_n),
    area = as.factor(area),
    sex = as.factor(sex),
    tribe = as.factor(tribe),
    breastMilk_given = as.factor(breastMilk_given),
    breastMilk_still_being_given = as.factor(breastMilk_still_being_given),
    contact_kindergarten = as.factor(contact_kindergarten),
    contact_otherChildren = as.factor(contact_otherChildren),
    contact_cigarettes = as.factor(contact_cigarettes),
    contact_cooking_fuel = as.factor(contact_cooking_fuel),
    contact_cooking_place = as.factor(contact_cooking_place),
    house_building_regroup = as.factor(house_building_regroup),
    house_roof_regroup = as.factor(house_roof_regroup),
    house_window_regroup = as.factor(house_window_regroup),
    hospitalised_last_3mo = as.factor(hospitalised_last_3mo),
    healthcareVisit_last_3mo = as.factor(healthcareVisit_last_3mo),
    sickness_past3days_fever = as.factor(sickness_past3days_fever),
    antibiotic_past3days = as.factor(antibiotic_past3days),
    antibiotic_past1mo = as.factor(antibiotic_past1mo),
    age_year_2groups = factor(age_year_2groups,
                              levels = c("1 and below", "more than 1")),
    final_pneumo_decision = factor(final_pneumo_decision,
                                   levels = c("negative", "positive"))
  )

col_map <- c(
  # final_pneumo_decision
  "positive" = "steelblue",
  "negative" = "indianred2"
)

column_names <- setdiff(names(df_epi), "final_pneumo_decision")

for (column in column_names){
  df_summary <- df_epi %>% 
    dplyr::group_by(!!sym(column), final_pneumo_decision) %>%  # Use !!sym(column) to reference column
    dplyr::summarise(count = n(), .groups = "drop") %>% 
    glimpse()
  
  f_name <- paste0("barplot_", column, ".png")
  f_path <- file.path("pictures/2_clean_data_descriptive", paste0(f_name))
  
  # Position = stack
  plot_stack <- ggplot(df_summary,
                       aes(x = !!sym(column), y = count,
                           fill = final_pneumo_decision)) + 
    geom_bar(position = "stack", stat = "identity") + 
    geom_text(aes(label = count), 
              position = position_stack(vjust = 1.05)) +
    scale_fill_manual(values = c(col_map)) +
    labs(y = "Count",
         x = "") + 
    theme_bw() +
    theme(legend.position="none")
  
  # Position = fill
  plot_fill <- ggplot(df_summary,
                      aes(x = !!sym(column), y = count,
                          fill = final_pneumo_decision)) + 
    geom_bar(position = "fill", stat = "identity") + 
    scale_fill_manual(values = c(col_map)) +
    labs(y = "Proportion",
         x = "") + 
    theme_bw() +
    theme(legend.position="none")
  
  # legend <- cowplot::get_legend(plot_stack)
  
  # Cowplot
  p1 <- cowplot::plot_grid(plot_stack, plot_fill,
                           ncol = 2,
                           labels = c("A", "B"))
  
  # Inspect weird distributions
  if (is.numeric(df_epi[[column]])) {
    predict <- ggplot(df_epi,
                      aes(x = !!sym(column), # convert 1-2 to proportion
                          y = as.numeric(final_pneumo_decision)-1)) +
      geom_point() +
      geom_smooth(method = "loess") +  # Check for nonlinearity
      theme_bw()
    
    p2 <- cowplot::plot_grid(p1, predict,
                             nrow = 2,
                             labels = c("", "C"))
    
    png(file = f_path, width = 23, height = 23, unit = "cm", res = 600)
    print(p2)
    dev.off()
    
  } else {
    png(file = f_path, width = 23, height = 12, unit = "cm", res = 600)
    print(p1)
    dev.off()
    
  }
  
}



# Data analyses process for genData ############################################

