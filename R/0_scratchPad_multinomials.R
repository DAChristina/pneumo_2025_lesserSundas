library(nnet)
library(dplyr)
# multivariable multinom

# multinom in Py
# https://ujangriswanto08.medium.com/the-complete-beginners-guide-to-multinomial-logistic-regression-49811c38737b
# https://www.princeton.edu/~otorres/LogitR101.pdf
# http://www.christoph-scherber.de/content/PDF%20Files/VisualizingpredictionsfrommultinomialmodelsinR.pdf

# nice source:
# https://bookdown.org/sarahwerth2024/CategoricalBook/multinomial-logit-regression-r.html


choosen_epi <- df_epi_gen_pneumo %>% 
  select(serotype_classification_PCV13_final_decision,
         serotype_final_decision,
         workWGS_gpsc_strain,
         
         age_year_3groups,
         contains("antibiotic"),
         area,
         breastFeed_compiled,
         contains("contact"),
         -healthcareVisit_last_3mo,
         -hospitalised_last_3mo,
         contains("house"),
         contains("illness"),
         -illness_past3days_fever,
         -illness_past3days_fever_nDays,
         contains("n_child"),
         -nTotal_child_5yo_andBelow_regroup, # collinearity with individual n_childs
         nTotal_child_5yo_andBelow_sleep_regroup,
         nTotal_people_regroup,
         sex,
         tribe,
         vaccination_hibpentavalent_dc_n_regroup,
         vaccination_pcv13_dc_n_regroup
         )

column_names <- setdiff(names(choosen_epi), "serotype_classification_PCV13_final_decision")
for (column in column_names){
  df_summary <- choosen_epi %>% 
    dplyr::group_by(!!sym(column), serotype_classification_PCV13_final_decision) %>%  # Use !!sym(column) to reference column
    dplyr::summarise(count = n(), .groups = "drop") %>% 
    glimpse()
  
  f_name <- paste0("barplot_", column, ".png")
  f_path <- file.path("pictures/3_serotype_vs_epi", paste0(f_name))
  
  # Position = stack
  plot_stack <- ggplot(df_summary,
                       aes(x = !!sym(column), y = count,
                           fill = serotype_classification_PCV13_final_decision)) + 
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
                          fill = serotype_classification_PCV13_final_decision)) + 
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
  if (is.numeric(choosen_epi[[column]])) {
    predict1 <- ggplot(choosen_epi,
                       aes(x = !!sym(column), # convert 1-2 to proportion
                           y = as.numeric(serotype_classification_PCV13_final_decision)-1)) +
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
    predict2 <- ggplot(choosen_epi, 
                       aes(x = !!sym(column), 
                           y = as.numeric(serotype_classification_PCV13_final_decision) - 1)) +
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

column_names_area <- setdiff(names(choosen_epi), c("area", "serotype_classification_PCV13_final_decision"))
for (column in column_names_area){
  df_summary <- choosen_epi %>% 
    # dplyr::filter(serotype_classification_PCV13_final_decision == "positive") %>%
    dplyr::group_by(area, serotype_classification_PCV13_final_decision, !!sym(column)) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>% 
    glimpse()
  
  f_name <- paste0("barplot_", column, "_areaGrouped.png")
  f_path <- file.path("pictures/3_serotype_vs_epi", paste0(f_name))
  
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
    facet_wrap(~serotype_classification_PCV13_final_decision,nrow=1)
  
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
    facet_wrap(~serotype_classification_PCV13_final_decision,nrow=1)
  
  # Cowplot
  p1 <- cowplot::plot_grid(count, proportion,
                           ncol = 2,
                           labels = c("A", "B"))
  
  png(file = f_path, width = 23, height = 6, unit = "cm", res = 600)
  print(p1)
  dev.off()
}










# Run multivariate multinomial regression
# or_multivar_all <- multinomial_analysis(df_input = choosen_epi %>% 
#                                           dplyr::select(where(~ all(!is.na(.)))),
#                                         outcome_col = "serotype_classification_PCV13_final_decision")





