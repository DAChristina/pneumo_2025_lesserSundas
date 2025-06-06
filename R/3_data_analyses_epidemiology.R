library(tidyverse)
source("global/fun.R")

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
    illness_past24h_difficulty_compiled = factor(illness_past24h_difficulty_compiled,
                                                 levels = c("no", "≥ 1 respiratory illness")),
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
  dplyr::select(sort(names(.))) %>% 
  dplyr::select(final_pneumo_decision,
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
  ) %>% 
  glimpse()

# Test epiFunction ehehe
# try OR report works only for categorical data
# df_epi_chars <- df_epi %>% 
#   dplyr::select(where(~ all(!is.na(.))), # NAs in workLab, workFasta & not interesting columns
#                 -contains("hospitalised"), # NAs in workLab, workFasta & not interesting columns
#                 -contains("healthcareVisit"), # NAs in workLab, workFasta & not interesting columns
#                 -sickness_past24h, # NAs in workLab, workFasta & not interesting columns
#                 -sickness_past3days_fever_howManyDays, # conflicted values with fever column
#                 # imbalanced values
#                 -age_month, # use age_year instead
#                 -breastMilk_given,
#                 -n_child_1yo_andBelow,
#                 -n_child_1to2yo,
#                 -n_child_2to4yo,
#                 -sickness_past3days_fever,
#                 -antibiotic_past3days,
#                 -antibiotic_past1mo,
#                 # too diverse, use other grouping columns instead
#                 -nTotal_people,
#                 -nTotal_child_5yo_andBelow,
#                 -nTotal_child_5yo_andBelow_sleep,
#                 -vaccination_pcv13_dc_n,
#                 # reduced because multicollinearity occur (GVIF^(1/(2*Df)) should be < 2)
#                 # I omit them from car::vif output
#                 -tribe, # multiCol with area (extremely high: > 10)
#                 # -house_building_regroup, # multicol with window (high: > 5)
#                 # -house_window_regroup,
#                 # -house_roof_regroup,
#                 # -nTotal_child_5yo_andBelow_regroup,
#                 # -nTotal_child_5yo_andBelow_sleep_regroup,
#                 -where(is.character)
#                 ) %>% 
#   glimpse()


# test carriage vs. age
test <- chisq.test(table(df_epi$final_pneumo_decision,
                         df_epi$age_year_3groups))

# all matrix calculation
df_epi_chars <- df_epi
column_names <- setdiff(names(df_epi_chars), "final_pneumo_decision")

cols_with_na_sums <- df_epi_chars %>%
  dplyr::summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  # transpose
  tidyr::pivot_longer(everything(),
                      names_to = "columns", values_to = "NAs") %>% 
  dplyr::filter(!str_detect(columns, "work"),
                NAs != 0) %>% 
  glimpse()

# odds ratio using epitools
or_matrix_all <- generate_or_matrix_report(df_input = df_epi_chars,
                                           binary_disease = "area")

or_matrix_table_report <- dplyr::full_join(
  purrr::imap_dfr(or_matrix_all, ~{
    df <- .x$measure
    if (!is.null(df)){
      df %>%
        as.data.frame() %>%
        tibble::rownames_to_column("value") %>%
        dplyr::mutate(variable = .y, value = rownames(df)) %>%
        dplyr::relocate(variable, value) # Moves to first columns
    }
  }),
  purrr::imap_dfr(or_matrix_all, ~{
    df <- .x$p.value
    if (!is.null(df)){
      df %>%
        as.data.frame() %>%
        tibble::rownames_to_column("value") %>%
        dplyr::mutate(variable = .y) %>%
        dplyr::relocate(variable, value)
    }
  }),
  by = c("variable", "value")
) %>% 
  dplyr::mutate(
    significance = case_when(
      midp.exact < 0.05 | fisher.exact < 0.05 | chi.square < 0.05 ~ "occur",
      !is.na(midp.exact) & !is.na(fisher.exact) & !is.na(chi.square) ~ "no",
      TRUE ~ NA_character_
    )) %>% 
  dplyr::select(variable, value, estimate, lower, upper, 
                midp.exact, fisher.exact, chi.square, significance) %>% 
  # view() %>% 
  glimpse()

# p-values chi-squares or fisher exact tools
ptest_matrix_all <- generate_or_chisq_report(df_input = df_epi_chars,
                                           binary_disease = "area")

ptest_matrix_table_report <- purrr::imap_dfr(ptest_matrix_all, ~{
  chisq <- .x$chisq_result
  fisher <- .x$fisher_result

  tidyr::tibble(
    variable = .y,
    chisq_stat = chisq$statistic %||% NA,
    chisq_df = chisq$parameter %||% NA,
    chisq_p = chisq$p.value %||% NA,
    chisq_method = chisq$method %||% NA_character_,
    
    fisher_or = fisher$estimate %||% NA,
    fisher_or_lo = fisher$conf.int[[1]] %||% NA,
    fisher_or_hi = fisher$conf.int[[2]] %||% NA,
    fisher_p = fisher$p.value %||% NA,
    fisher_method = fisher$method %||% NA_character_
  )
  }) %>% 
  dplyr::mutate(
    significance = case_when(
      fisher_p < 0.05 | chisq_p < 0.05 ~ "occur",
      !is.na(fisher_p) & !is.na(chisq_p) ~ "no",
      TRUE ~ NA_character_
      )
  ) %>% 
  glimpse()


# combine report
compile_all_report_with_pValues <- dplyr::left_join(
  read.csv("outputs/epi_all_descriptive_percentages_report_corrected.csv"),
  or_matrix_table_report,
  by = c("variable"),
  relationship = "many-to-many"
) %>% 
  # dplyr::filter(estimate != 1) %>% # rese amat
  dplyr::select(-value.y) %>% 
  dplyr::rename(value = value.x) %>% 
  dplyr::left_join(
    ptest_matrix_table_report,
    by = c("variable")
  ) %>% 
  dplyr::distinct(variable, value, .keep_all = T) %>% 
  # view() %>%
  glimpse()

write.csv(compile_all_report_with_pValues, "outputs/epi_all_descriptive_percentages_report_corrected_with_pValues.csv",
          row.names = F)


# trial chisq lombok vs. sumbawa
test <- chisq.test(df_epi_chars$area, df_epi_chars$final_pneumo_decision)
test

test <- chisq.test(df_epi_chars$area, df_epi_chars$tribe)
test$observed

# try glm crude OR report
or_univar_all <- generate_univar_report(df_input = df_epi_chars %>% 
                                          dplyr::select(where(~ all(!is.na(.)))),
                                                   binary_disease = "final_pneumo_decision")

or_univar_model_report <- purrr::imap_dfr(or_univar_all, ~{
  model <- .x$model
  coefficients_df <- broom::tidy(model) %>%
    mutate(variable = .y) %>%
    rename(value = term)
  
  model_stats <- dplyr::tibble(
    variable = .y,
    null_deviance = model$null.deviance,
    residual_deviance = model$deviance,
    df_null = model$df.null,
    df_residual = model$df.residual,
    AIC = model$aic
  )
  # Combine everything into one table
  dplyr::left_join(coefficients_df, model_stats, by = "variable")
}) %>% 
  dplyr::mutate(OR = exp(estimate), # estimate is log(OR)
                OR_lower_CI = exp(estimate - 1.96 * std.error),
                OR_upper_CI = exp(estimate + 1.96 * std.error),
                significance = case_when(
                  p.value < 0.05 ~ "occur",
                  !is.na(p.value) ~ "no",
                  TRUE ~ NA_character_)
  ) %>% 
  dplyr::arrange(variable) %>% 
  dplyr::rename_all(~ paste0("crude_", .)) %>% 
  dplyr::rename(variable = crude_variable,
                value = crude_value) %>% 
  dplyr::mutate(crude_OR_report = paste0(round(crude_OR, 2),
                                         " (", round(crude_OR_lower_CI, 2),
                                         "-", round(crude_OR_upper_CI, 2), ")")) %>% 
  dplyr::select(variable, value, crude_estimate, crude_std.error,
                crude_OR, crude_OR_lower_CI, crude_OR_upper_CI,
                crude_OR_report,
                crude_statistic, crude_p.value, crude_significance,
                crude_null_deviance, crude_residual_deviance,
                crude_df_null, crude_df_residual, crude_AIC) %>% 
  glimpse()

# try univariable glm crude OR plus area report
or_univar_all_plusArea <- generate_univar_plusArea_report(df_input = df_epi_chars %>% 
                                          dplyr::select(where(~ all(!is.na(.)))),
                                        binary_disease = "final_pneumo_decision")


or_univar_model_plusArea_report <- purrr::imap_dfr(or_univar_all_plusArea, ~{
  model <- .x$model
  coefficients_df <- broom::tidy(model) %>%
    mutate(variable = .y) %>%
    rename(value = term)
  
  model_stats <- dplyr::tibble(
    variable = .y,
    null_deviance = model$null.deviance,
    residual_deviance = model$deviance,
    df_null = model$df.null,
    df_residual = model$df.residual,
    AIC = model$aic
  )
  # Combine everything into one table
  dplyr::left_join(coefficients_df, model_stats, by = "variable")
}) %>% 
  dplyr::mutate(OR = exp(estimate), # estimate is log(OR)
                OR_lower_CI = exp(estimate - 1.96 * std.error),
                OR_upper_CI = exp(estimate + 1.96 * std.error),
                significance = case_when(
                  p.value < 0.05 ~ "occur",
                  !is.na(p.value) ~ "no",
                  TRUE ~ NA_character_)
  ) %>% 
  dplyr::arrange(variable) %>% 
  dplyr::rename_all(~ paste0("plusArea_", .)) %>% 
  dplyr::rename(variable = plusArea_variable,
                value = plusArea_value) %>% 
  dplyr::mutate(plusArea_report = paste0(round(plusArea_OR, 2),
                                         " (", round(plusArea_OR_lower_CI, 2),
                                         "-", round(plusArea_OR_upper_CI, 2), ")")) %>% 
  dplyr::select(variable, value, plusArea_estimate, plusArea_std.error,
                plusArea_OR, plusArea_OR_lower_CI, plusArea_OR_upper_CI,
                plusArea_report,
                plusArea_statistic, plusArea_p.value, plusArea_significance,
                plusArea_null_deviance, plusArea_residual_deviance,
                plusArea_df_null, plusArea_df_residual, plusArea_AIC) %>% 
  glimpse()

compile_crude_plusArea <- dplyr::left_join(
  or_univar_model_plusArea_report, or_univar_model_report,
  by = c("variable", "value")
) %>% 
  dplyr::select(variable, value,
                crude_OR_report, crude_p.value, crude_significance,
                plusArea_report, plusArea_p.value, plusArea_significance,) %>% 
  view() %>% 
  glimpse()

# try glm multivariable OR report
or_multivariable_all <- generate_multivar_report(df_input = df_epi_chars %>% 
                                                   dplyr::select(where(~ all(!is.na(.)))),
                                                 binary_disease = "final_pneumo_decision") 

saveRDS(or_multivariable_all,
        "outputs/epi_all_multivariable_logistic_regression_model.rds")
# test <- readRDS("outputs/epi_all_multivariable_logistic_regression_model.rds")

# generate pictures
png(file = "pictures/epiAnalyses_all_multivariable_model1.png",
    width = 23, height = 23, unit = "cm", res = 600)
performance::check_model(or_multivariable_all$model1)
dev.off()

# ignore multicollinearity test
# png(file = "pictures/epiAnalyses_all_multivariable_model2.png",
#     width = 23, height = 23, unit = "cm", res = 600)
# performance::check_model(or_multivariable_all$model2)
# dev.off()

final_model_reconvert <- glm(formula(or_multivariable_all$final_model),
                             family = binomial, data = df_epi_chars)

png(file = "pictures/epiAnalyses_all_multivariable_final_model.png",
    width = 23, height = 23, unit = "cm", res = 600)
performance::check_model(final_model_reconvert)
dev.off()

png(file = "pictures/epiAnalyses_all_multivariable_final_model_collinearity.png",
    width = 23, height = 12, unit = "cm", res = 600)
performance::check_model(final_model_reconvert, check = "vif")
dev.off()

# additional analysis for plusArea
final_model_reconvert_variables <- attr(terms(final_model_reconvert),
                                        "term.labels")

formula_plusArea <- as.formula(paste("final_pneumo_decision ~ area +",
                                     paste(final_model_reconvert_variables,
                                           collapse = "+")))
final_model_plusArea <- glm(formula_plusArea,
                            family = binomial, data = df_epi_chars)

png(file = "pictures/epiAnalyses_all_multivariable_final_model_wider_plusArea.png",
    width = 23, height = 23, unit = "cm", res = 600)
performance::check_model(final_model_plusArea)
dev.off()

final_model_area_comparison <- anova(final_model_reconvert,
                                     final_model_plusArea,
                                     test = "Chisq")
saveRDS(final_model_area_comparison,
        "outputs/epi_all_multivariable_logistic_regression_model_area_comparison.rds")


# generate justification report
df_models <- data.frame(
  Variable = names(coef(or_multivariable_all$model1)),
  Odds_Ratio = or_multivariable_all$odds_ratio1,
  Lower_CI = or_multivariable_all$lower_CI1,
  Upper_CI = or_multivariable_all$upper_CI1,
  P_Value = or_multivariable_all$p_value1,
  row.names = NULL
) %>% 
  dplyr::rename_all(~ paste0("model1_", .)) %>% 
  # ignore multicollinearity check
  # dplyr::left_join(
  #   data.frame(
  #     Variable = names(coef(or_multivariable_all$model2)),
  #     Odds_Ratio = or_multivariable_all$odds_ratio2,
  #     Lower_CI = or_multivariable_all$lower_CI2,
  #     Upper_CI = or_multivariable_all$upper_CI2,
  #     P_Value = or_multivariable_all$p_value2,
  #     row.names = NULL
  #   ) %>% 
  #     dplyr::rename_all(~ paste0("model2_", .))
  #   ,
  #   by = c("model1_Variable" = "model2_Variable")
  # ) %>% 
  dplyr::left_join(
    as.data.frame(or_multivariable_all$final_model[["coefficients"]]) %>%
      tibble::rownames_to_column(var = "Variable") %>% 
      dplyr::left_join(
        tibble::enframe(or_multivariable_all$adj_odds_ratio,
                        name = "Variable",
                        value = "Adjusted_Odds_Ratio")
        ,
        by = "Variable"
      ) %>% 
      dplyr::left_join(
        tibble::enframe(or_multivariable_all$adj_OR_lower_CI,
                        name = "Variable",
                        value = "Lower_CI")
        ,
        by = "Variable"
      ) %>% 
      dplyr::left_join(
        tibble::enframe(or_multivariable_all$adj_OR_upper_CI,
                        name = "Variable",
                        value = "Upper_CI")
        ,
        by = "Variable"
      ) %>%
      dplyr::rename_all(~ paste0("final_", .))
    ,
    by = c("model1_Variable" = "final_Variable")
  ) %>% 
  dplyr::rename_with(~ tolower(gsub("[^[:alnum:]_]", "", .x))) %>% 
  dplyr::rename(variable_value = model1_variable) %>% 
  dplyr::mutate(
    model1_report_OR = paste0(round(model1_odds_ratio, 2),
                              " (", round(model1_lower_ci, 2),
                              "-", round(model1_upper_ci, 2), ")"),
    # model2_report_OR = paste0(round(model2_odds_ratio, 2),
    #                           " (", round(model2_lower_ci, 2),
    #                           "-", round(model2_upper_ci, 2), ")"),
    final_report_OR = paste0(round(final_adjusted_odds_ratio, 2),
                             " (", round(final_lower_ci, 2),
                             "-", round(final_upper_ci, 2), ")")
  ) %>% 
  # view() %>% 
  glimpse()

# combine models with univariable analysis result
df_models_all <- dplyr::left_join(
  compile_crude_plusArea %>% 
    dplyr::rename_all(~ paste0("univar_", .)) %>% 
    dplyr::rename(variable = univar_variable,
                  value = univar_value)
  ,
  df_models %>% 
    dplyr::rename_all(~ paste0("multivar_", .)) %>% 
    dplyr::rename(variable_value = multivar_variable_value)
  ,
  by = c("value" = "variable_value")
)


write.csv(df_models_all, "outputs/epi_all_final_models.csv", row.names = F)

# convert multicollinearity test
# df_multicol <- as.data.frame(or_multivariable_all$multicollinearity_test1) %>% 
#   tibble::rownames_to_column(var = "Variable") %>% 
#   dplyr::rename_all(~ paste0("multicol1_", .)) %>% 
#   dplyr::left_join(
#     as.data.frame(or_multivariable_all$multicollinearity_test2) %>% 
#       tibble::rownames_to_column(var = "Variable") %>% 
#       dplyr::rename_all(~ paste0("multicol2_", .))
#     ,
#     by = c("multicol1_Variable" = "multicol2_Variable")
#   ) %>% 
#   dplyr::rename_with(~ tolower(gsub("[^[:alnum:]_]", "", .x))) %>% 
#   # view() %>% 
#   glimpse()
# 
# write.csv(df_multicol, "outputs/epi_all_multicollinearity_test.csv", row.names = F)

# convert model comparison ANOVA into a dataframe (see the *.rds file instead!)
# df_model_comparison <- as.data.frame(or_multivariable_all$model_comparison) %>% 
#   view()

# doesn't matter. Don't think multivariable logistic is the best model
# coz' the data comes from carriage states.
# model_goodness_of_fit <- generate_goodnes_of_fit_report(df_input = df_epi_chars,
#                                                        binary_disease = "final_pneumo_decision",
#                                                        final_model = or_multivariable_all$final_model,
#                                                        h_group = 5) # based on 100 < n data < 1000
# model_goodness_of_fit
# ResourceSelection::hoslem.test(df_epi_chars$final_pneumo_decision, fitted(final_model_reconvert))

# pred <- stats::predict(final_model_reconvert, type = "response")
# roc_curve <- pROC::roc(df_epi_chars$final_pneumo_decision, pred)
# plot(roc_curve)
# pROC::auc(roc_curve)  # Higher AUC = better model

# ctrl <- caret::trainControl(method = "cv", number = 10)
# cv_model <- caret::train(as.formula(final_pneumo_decision ~ 
#                                       contact_cigarettes + contact_otherChildren
#                                     + illness_past24h_runny_nose + n_child_1to2yo), data = df_epi_chars,
#                          method = "glm", family = "binomial", trControl = ctrl)
# cv_model
# cv_model$results
# cv_model$finalModel


# epiAnalyses for genome data (subset to positive pneumo only) #################



