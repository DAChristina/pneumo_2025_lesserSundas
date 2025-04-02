library(tidyverse)

# Data analyses process for epiData ############################################
df_epi <- read.csv("inputs/epiData_with_final_pneumo_decision.csv") %>% 
  dplyr::select(-specimen_id) %>% 
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
    illness_past3days_fever_nDays_regroup = as.numeric(illness_past3days_fever_nDays_regroup),
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
    breastFeed_compiled = as.factor(breastFeed_compiled),
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
    illness_past3days_fever_regroup = as.factor(illness_past3days_fever_regroup),
    illness_past24h_cough = as.factor(illness_past24h_cough),
    illness_past24h_runny_nose = as.factor(illness_past24h_runny_nose),
    illness_past24h_difficulty_breathing = as.factor(illness_past24h_difficulty_breathing),
    illness_past24h_difficulty_compiled = as.factor(illness_past24h_difficulty_compiled),
    antibiotic_past3days = as.factor(antibiotic_past3days),
    antibiotic_past1mo = as.factor(antibiotic_past1mo),
    age_year_2groups = factor(age_year_2groups,
                              levels = c("1 and below", "more than 1")),
    age_year_3groups = factor(age_year_3groups,
                           levels = c("1 and below", "1-2", "3-5")),
    nTotal_people_regroup = factor(nTotal_people_regroup,
                                   levels = c("1-3 (low)", "4-6 (moderate)", ">6 (high)")),
    nTotal_child_5yo_andBelow_regroup = factor(nTotal_child_5yo_andBelow_regroup,
                                               levels = c("0", "1-4")),
    nTotal_child_5yo_andBelow_sleep_regroup = factor(nTotal_child_5yo_andBelow_sleep_regroup,
                                                     levels = c("0", "1-3")),
    vaccination_hibpentavalent_dc_n_regroup = factor(vaccination_hibpentavalent_dc_n_regroup,
                                            levels = c("1-3 mandatory", "4 booster")),
    vaccination_pcv13_dc_n_regroup = factor(vaccination_pcv13_dc_n_regroup,
                                            levels = c("1-2 mandatory", "3-4 booster")),
    final_pneumo_decision = factor(final_pneumo_decision,
                                   levels = c("negative", "positive"))
  ) %>% 
  glimpse()

col_map <- c(
  # final_pneumo_decision
  "positive" = "steelblue",
  "negative" = "indianred2",
  # area
  "lombok" = "orange",
  "sumbawa" = "seagreen4",
  # serotype groups
  "VT" = "indianred3",
  "NVT" = "skyblue2",
  "acapsular" = "deepskyblue3",
  # PCV13 groups
  "1-2 mandatory" = "rosybrown",
  "3-4 booster" = "paleturquoise3"
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
    predict1 <- ggplot(df_epi,
                      aes(x = !!sym(column), # convert 1-2 to proportion
                          y = as.numeric(final_pneumo_decision)-1)) +
      geom_point() +
      geom_smooth(method = "loess") +  # Check for nonlinearity
      theme_bw()
    
    p2 <- cowplot::plot_grid(p1, predict1,
                             nrow = 2,
                             labels = c("", "C"))
    
    png(file = f_path, width = 23, height = 23, unit = "cm", res = 600)
    print(p2)
    dev.off()
    
  } else {
    predict2 <- ggplot(df_epi, 
                      aes(x = !!sym(column), 
                          y = as.numeric(final_pneumo_decision) - 1)) +
      # geom_boxplot()+
      geom_violin(trim = T, colour = NA, fill = "skyblue", alpha = 0.7) + 
      geom_jitter(width = 0.1, alpha = 0.2) +
      theme_bw()
    
    p3 <- cowplot::plot_grid(p1, predict2,
                             nrow = 2,
                             labels = c("", "C"))
    
    png(file = f_path, width = 23, height = 23, unit = "cm", res = 600)
    print(p3)
    dev.off()
    
  }
}

column_names_area <- setdiff(names(df_epi), c("area", "final_pneumo_decision"))
for (column in column_names_area){
  df_summary <- df_epi %>% 
    # dplyr::filter(final_pneumo_decision == "positive") %>%
    dplyr::group_by(area, final_pneumo_decision, !!sym(column)) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>% 
    glimpse()
  
  f_name <- paste0("barplot_", column, "_areaGrouped.png")
  f_path <- file.path("pictures/2_clean_data_descriptive", paste0(f_name))
  
  count <- ggplot(df_summary,
                  aes(x = !!sym(column), y = count,
                      fill = area)) + 
    geom_bar(position = "dodge", stat = "identity") + 
    geom_text(aes(label = count), 
              position = position_dodge(width = 0.9),
              vjust = 1.05) +
    scale_fill_manual(values = c(col_map)) +
    labs(y = "Count",
         x = "") + 
    theme_bw() +
    theme(legend.position="none") +
    facet_wrap(~final_pneumo_decision,nrow=1)
  
  proportion <- ggplot(df_summary,
                       aes(x = !!sym(column), y = count,
                           fill = area)) + 
    geom_bar(position = "fill", stat = "identity") + 
    # geom_text(aes(label = proportion), 
    #           position = position_dodge(width = 0.9),
    #           vjust = 1.05) +
    scale_fill_manual(values = c(col_map)) +
    labs(y = "Proportion",
         x = "") + 
    theme_bw() +
    theme(legend.position="none") +
    facet_wrap(~final_pneumo_decision,nrow=1)
  
  # Cowplot
  p1 <- cowplot::plot_grid(count, proportion,
                           ncol = 2,
                           labels = c("A", "B"))
  
  png(file = f_path, width = 23, height = 6, unit = "cm", res = 600)
  print(p1)
  dev.off()
}

# based on visual inspection,
# I preferred the re-grouped columns over the original one
# generate descriptive report based on area
df_epi_sorted <- df_epi %>% 
  dplyr::select(sort(names(df_epi))) %>% 
  dplyr::select(final_pneumo_decision,
                age_year_3groups,
                contains("antibiotic"),
                area,
                breastFeed_compiled,
                contains("contact"),
                healthcareVisit_last_3mo,
                hospitalised_last_3mo,
                contains("house"),
                contains("illness"),
                -illness_past3days_fever,
                -illness_past3days_fever_nDays,
                contains("n_child"),
                nTotal_child_5yo_andBelow_regroup,
                nTotal_child_5yo_andBelow_sleep_regroup,
                nTotal_people_regroup,
                sex,
                tribe,
                vaccination_hibpentavalent_dc_n_regroup,
                vaccination_pcv13_dc_n_regroup
                ) %>% 
  glimpse()


# compiled pneumo positive
df_compiled_positivePneumo <- dplyr::left_join(
  df_epi_sorted %>% 
    dplyr::mutate(across(everything(), as.character)) %>% 
    tidyr::pivot_longer(cols = everything(),
                        names_to = "variable", values_to = "value") %>% 
    dplyr::group_by(variable, value) %>% 
    dplyr::summarise(count_all = n(), .groups = "drop")
  ,
  df_epi_sorted %>% 
    dplyr::mutate(across(-final_pneumo_decision, as.character)) %>%
    tidyr::pivot_longer(cols = -final_pneumo_decision, names_to = "variable", values_to = "value") %>% 
    dplyr::group_by(variable, value, final_pneumo_decision) %>% 
    dplyr::summarise(count_positivePneumo = n(), .groups = "drop") %>% 
    dplyr::filter(final_pneumo_decision == "positive") %>% 
    dplyr::select(-final_pneumo_decision)
    ,
  by = c("variable", "value")
) %>% 
  dplyr::mutate(
    count_positivePneumo = case_when(
      is.na(count_positivePneumo) ~ 0,
      TRUE ~ count_positivePneumo
      ),
    percent = round(count_positivePneumo/count_all*100, 2),
    report_positive_all = paste0(percent, "% (", count_positivePneumo, "/", count_all, ")")
  ) %>% 
  dplyr::select(-count_all,
                -count_positivePneumo,
                -percent) %>% 
  # view() %>% 
  glimpse()

# compiled pneumo positive per-area
df_area_positivePneumo <- dplyr::left_join(
  df_epi_sorted %>% 
    dplyr::mutate(across(-area, as.character)) %>%
    tidyr::pivot_longer(cols = -area, names_to = "variable", values_to = "value") %>% 
    dplyr::group_by(variable, value, area) %>% 
    dplyr::summarise(count_perArea = n(), .groups = "drop")
  ,
  df_epi_sorted %>% 
    dplyr::mutate(across(-c(area, final_pneumo_decision), as.character)) %>%  
    tidyr::pivot_longer(cols = -c(area, final_pneumo_decision),
                        names_to = "variable", values_to = "value") %>% 
    dplyr::group_by(variable, value, area, final_pneumo_decision) %>% 
    dplyr::summarise(count_positivePneumo = n(), .groups = "drop") %>% 
    dplyr::filter(final_pneumo_decision == "positive") %>% 
    dplyr::select(-final_pneumo_decision)
  ,
  by = c("variable", "value", "area")
) %>% 
  dplyr::mutate(
    count_positivePneumo = case_when(
      is.na(count_positivePneumo) ~ 0,
      TRUE ~ count_positivePneumo
    ),
    percent = round(count_positivePneumo/count_perArea*100, 2),
    report_positive_perArea = paste0(percent, "% (", count_positivePneumo, "/", count_perArea, ")")
  ) %>% 
  dplyr::select(-count_perArea,
                -count_positivePneumo,
                -percent) %>% 
  tidyr::pivot_wider(names_from = area, 
                     values_from = report_positive_perArea) %>% 
  dplyr::rename(
    report_positive_lombok = lombok,
    report_positive_sumbawa = sumbawa
  ) %>% 
  # view() %>% 
  glimpse()

compile_positive_all <- dplyr::left_join(
  df_compiled_positivePneumo, df_area_positivePneumo,
  by = c("variable", "value")
) %>% 
  view() %>% 
  glimpse()

# compile all assessment
df_assessed <- df_epi_sorted %>% 
  dplyr::mutate(across(everything(), as.character)) %>% 
  tidyr::pivot_longer(cols = everything(),
                      names_to = "variable", values_to = "value") %>% 
  dplyr::group_by(variable, value) %>% 
  dplyr::summarise(count_all = n(), .groups = "drop") %>% 
  dplyr::mutate(
    percent = round(count_all/900*100, 2), # total n = 900
    report_assessed_all = paste0(percent, "% (", count_all, "/900)")
  ) %>% 
  dplyr::select(-count_all,
                -percent) %>% 
  # view() %>%
  glimpse()

# compile all assessment per-area
df_assessed_area <- df_epi_sorted %>% 
  dplyr::mutate(across(-area, as.character)) %>%  
  tidyr::pivot_longer(cols = -area,
                      names_to = "variable", values_to = "value") %>% 
  dplyr::group_by(variable, value, area) %>% 
  dplyr::summarise(count_assessed_perArea = n(), .groups = "drop") %>% 
  dplyr::mutate(
    percent = round(count_assessed_perArea/450*100, 2), # total n = 450 per-area
    report_assessed_perArea = paste0(percent, "% (", count_assessed_perArea, "/450)")
  ) %>% 
  dplyr::select(-count_assessed_perArea,
                -percent) %>% 
  tidyr::pivot_wider(names_from = area, 
                     values_from = report_assessed_perArea) %>% 
  dplyr::rename(
    report_assessed_lombok = lombok,
    report_assessed_sumbawa = sumbawa
  ) %>% 
  # view() %>%
  glimpse()

compile_assessed_all <- dplyr::left_join(
  df_assessed, df_assessed_area,
  by = c("variable", "value")
) %>% 
  # view() %>% 
  glimpse()

compile_all_report <- dplyr::left_join(
  compile_assessed_all, compile_positive_all,
  by = c("variable", "value")
) %>% 
  # re-arranged
  dplyr::select(variable, value,
                report_assessed_lombok, report_positive_lombok,
                report_assessed_sumbawa, report_positive_sumbawa,
                report_assessed_all, report_positive_all) %>% 
  # view() %>% 
  glimpse()

write.csv(compile_all_report, "outputs/epi_all_descriptive_percentages_report.csv",
          row.names = F)


# Data analyses process for genData ############################################
# I separate genData to 2 groups: pneumo & interSp
df_epi_gen_pneumo <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
  dplyr::mutate(workWGS_kity_predicted_serotype_regroup = case_when(
    workWGS_kity_predicted_serotype_regroup == "mixed serotypes/serogroups" ~ "mixed serogroups",
    TRUE ~ workWGS_kity_predicted_serotype_regroup
  ),
                workWGS_kity_predicted_serotype_regroup = factor(workWGS_kity_predicted_serotype_regroup,
                                                                 levels = c(# VT
                                                                   "3", "6A/6B", "6A/6B/6C/6D", "serogroup 6 (6B/6E)",
                                                                   "14", "17F", "18C/18B", "19A", "19F", "23F",
                                                                   # NVT
                                                                   "7C", "10A", "10B", "11A/11D", "13", "15A", "15B/15C",
                                                                   "16F", "19B", "20", "23A", "23B", "24F/24B", "25F/25A/38",
                                                                   "28F/28A", "31", "34", "35A/35C/42", "35B/35D", "35F",
                                                                   "37", "39", "mixed serogroups",
                                                                   "acapsular")),
                serotype_classification_PCV13 = factor(serotype_classification_PCV13,
                                                       levels = c("VT", "NVT", "acapsular"))
                ) %>%
  glimpse()

# Quick viz serotypes
# Compute percentage
df_serotype_summary <- df_epi_gen_pneumo %>% 
  dplyr::count(workWGS_kity_predicted_serotype_regroup) %>%
  dplyr::mutate(percentage = n / sum(n) * 100) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::select(workWGS_kity_predicted_serotype_regroup,
                    serotype_classification_PCV13)
    ,
    by = "workWGS_kity_predicted_serotype_regroup"
  ) %>% 
  dplyr::distinct()

ser1 <- ggplot(df_serotype_summary, aes(x = workWGS_kity_predicted_serotype_regroup, y = percentage,
                                fill = serotype_classification_PCV13)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = " ", y = "Percentage", 
       # title = "All Serotypes"
       ) +
  scale_fill_manual(values = c(col_map)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.justification = c(0.5, 0),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))

# Compute percentage by area
df_serotype_area_summary <- df_epi_gen_pneumo %>% 
  dplyr::group_by(area, serotype_classification_PCV13, workWGS_kity_predicted_serotype_regroup) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::mutate(percentage = count / sum(count) * 100,
                # slightly change classifications
                serotype_classification_PCV13 = case_when(
                  serotype_classification_PCV13 == "acapsular" ~ " ",
                  TRUE ~ serotype_classification_PCV13
                ),
                serotype_classification_PCV13 = factor(serotype_classification_PCV13,
                                                       levels = c("VT", "NVT", " ")))

ser2 <- ggplot(df_serotype_area_summary, aes(x = workWGS_kity_predicted_serotype_regroup,
                                     y = percentage,
                                     fill = area)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(~ serotype_classification_PCV13,
             scales = "free_x",
             space = "free_x"
             ) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = "Category", y = "Percentage", 
       # title = "All Serotypes"
       ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.justification = c(0.5, 0),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"),
        # strip.placement = "outside",
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.background = element_blank(),
        # panel.spacing = unit(1, "lines"),
        strip.position = "bottom"
        ) # +
  # facet_wrap(~ serotype_classification_PCV13, nrow = 1, scales = "free_x")

png(file = "pictures/genData_serotypes_classification.png", width = 23, height = 25, unit = "cm", res = 600)
cowplot::plot_grid(ser1, ser2,
                   nrow = 2,
                   labels = c("A", "B"))
dev.off()

# serotype classification based on vaccination period
df_serotype_PCV13_summary <- df_epi_gen_pneumo %>% 
  dplyr::group_by(vaccination_pcv13_dc_n, serotype_classification_PCV13) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::mutate(percentage = count / sum(count) * 100,
                serotype_classification_PCV13 = factor(serotype_classification_PCV13,
                                                       levels = c("VT", "NVT", "acapsular")))

ser3 <- ggplot(df_serotype_PCV13_summary, aes(x = vaccination_pcv13_dc_n,
                                             y = percentage,
                                             fill = serotype_classification_PCV13)) +
  # geom_line(size = 1.5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            position = position_dodge(width = 0.9),
            vjust = -0.3) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = "PCV13", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.justification = c(0.5, 0),
        # legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"),
        # strip.placement = "outside",
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.background = element_blank(),
        # panel.spacing = unit(1, "lines"),
        strip.position = "bottom"
  )

df_serotype_PCV13_grouped <- df_epi_gen_pneumo %>% 
  dplyr::group_by(vaccination_pcv13_dc_n_regroup, serotype_classification_PCV13, workWGS_kity_predicted_serotype_regroup) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::mutate(percentage = count / sum(count) * 100,
                # slightly change classifications
                serotype_classification_PCV13 = case_when(
                  serotype_classification_PCV13 == "acapsular" ~ " ",
                  TRUE ~ serotype_classification_PCV13
                ),
                serotype_classification_PCV13 = factor(serotype_classification_PCV13,
                                                       levels = c("VT", "NVT", " ")))

ser4 <- ggplot(df_serotype_PCV13_grouped, aes(x = workWGS_kity_predicted_serotype_regroup,
                                              y = percentage,
                                              fill = vaccination_pcv13_dc_n_regroup)) +
  # geom_line(size = 1.5) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(~ serotype_classification_PCV13,
             scales = "free_x",
             space = "free_x"
  ) +
  # geom_text(aes(label = paste0(round(percentage, 2), "%")),
  #           position = position_dodge(width = 0.9),
  #           vjust = -0.3) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = "Grouped PCV13", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.justification = c(0.5, 0),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"),
        # strip.placement = "outside",
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.background = element_blank(),
        # panel.spacing = unit(1, "lines"),
        strip.position = "bottom"
  )


png(file = "pictures/genData_serotypes_vaccination_classification.png", width = 23, height = 25, unit = "cm", res = 600)
cowplot::plot_grid(ser3, ser4,
                   nrow = 2,
                   labels = c("A", "B"))
dev.off()

# trial multicowplot
cowplot::plot_grid(ser1, ser3,
                   ser2, ser4,
                   nrow = 2,
                   labels = c("A", "B", "C", "D"))





























# focused on inter-species
df_gen_interSp <- read.csv("inputs/genData_all.csv") %>% 
  dplyr::filter(workWGS_stats_pneumo_cutoff == "> 2.3 Mb") %>% # S. christatus automatically deleted 
  # view() %>% 
  glimpse()

















