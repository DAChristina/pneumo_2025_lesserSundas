library(tidyverse)
source("global/fun.R")

df_epi <- read.csv("inputs/epiData_with_final_pneumo_decision.csv") %>% 
  dplyr::select(-specimen_id) %>% 
  # conduct corrections for supposedly NUMERICAL and FACTOR (not ordered) columns!
  dplyr::mutate(
    age_month = as.numeric(age_month),
    age_year = as.numeric(age_year),
    # nTotal_people = as.numeric(nTotal_people),
    # nTotal_child_5yo_andBelow = as.numeric(nTotal_child_5yo_andBelow),
    # n_child_1yo_andBelow = as.numeric(n_child_1yo_andBelow),
    # n_child_1to2yo = as.numeric(n_child_1to2yo),
    # n_child_2to4yo = as.numeric(n_child_2to4yo),
    # nTotal_child_5yo_andBelow_sleep = as.numeric(nTotal_child_5yo_andBelow_sleep),
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
    nTotal_people_regroup = factor(nTotal_people_regroup,
                                   levels = c("1-3 (low)", "4-6 (moderate)", ">7 (high)")),
    nTotal_child_5yo_andBelow_regroup = factor(nTotal_child_5yo_andBelow_regroup,
                                               levels = c("0", "1-4")),
    nTotal_child_5yo_andBelow_sleep_regroup = factor(nTotal_child_5yo_andBelow_sleep_regroup,
                                                     levels = c("0", "1-3")),
    vaccination_pcv13_dc_n_regroup = factor(vaccination_pcv13_dc_n_regroup,
                                            levels = c("1-2 mandatory", "3-4 booster")),
    final_pneumo_decision = factor(final_pneumo_decision,
                                   levels = c("negative", "positive"))
  ) %>% 
  glimpse()

# Test epiFunction ehehe
# try OR report works only for categorical data
df_epi_chars <- df_epi %>% 
  dplyr::select(where(~ all(!is.na(.))), # NAs in workLab, workFasta & not interesting columns
                -contains("hospitalised"), # NAs in workLab, workFasta & not interesting columns
                -contains("healthcareVisit"), # NAs in workLab, workFasta & not interesting columns
                -sickness_past24h, # NAs in workLab, workFasta & not interesting columns
                -sickness_past3days_fever_howManyDays, # conflicted values with fever column
                # imbalanced values
                -age_month, # use age_year instead
                -breastMilk_given,
                -n_child_1yo_andBelow,
                -n_child_1to2yo,
                -n_child_2to4yo,
                -sickness_past3days_fever,
                -antibiotic_past3days,
                -antibiotic_past1mo,
                # too diverse, use other grouping columns instead
                -nTotal_people,
                -nTotal_child_5yo_andBelow,
                -nTotal_child_5yo_andBelow_sleep,
                -vaccination_pcv13_dc_n,
                # reduced because multicollinearity occur (GVIF^(1/(2*Df)) should be < 2)
                # I omit them from car::vif output
                -tribe, # multiCol with area (extremely high: > 10)
                # -house_building_regroup, # multicol with window (high: > 5)
                # -house_window_regroup,
                # -house_roof_regroup,
                # -nTotal_child_5yo_andBelow_regroup,
                # -nTotal_child_5yo_andBelow_sleep_regroup,
                -where(is.character)
                ) %>% 
  glimpse()

column_names <- setdiff(names(df_epi_chars), "final_pneumo_decision")
for (column in column_names){
  df_summary <- df_epi_chars %>% 
    dplyr::group_by(!!sym(column), final_pneumo_decision) %>% 
    dplyr::summarise(count = n(), .groups = "drop") %>% 
    glimpse()
}

cols_with_na_sums <- df_epi_chars %>%
  dplyr::summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  # transpose
  tidyr::pivot_longer(everything(),
                      names_to = "columns", values_to = "NAs") %>% 
  dplyr::filter(!str_detect(columns, "work"),
                NAs != 0) %>% 
  glimpse()

or_matrix_all <- generate_or_matrix_report(df_input = df_epi_chars,
                                           binary_disease = "final_pneumo_decision")

or_matrix_table_report <- dplyr::full_join(
  purrr::imap_dfr(or_matrix_all, ~{
    df <- .x$measure
    if (is.null(df)) return(NULL)
    
    df %>%
      as.data.frame() %>%
      dplyr::mutate(aspect = .y, category = rownames(df)) %>%
      dplyr::relocate(aspect, category) # Moves to first columns
  }),
  purrr::imap_dfr(or_matrix_all, ~{
    df <- .x$p.value
    if (is.null(df)) return(NULL)
    
    df %>%
      as.data.frame() %>%
      dplyr::mutate(aspect = .y, category = rownames(df)) %>%
      dplyr::relocate(aspect, category)
  }),
  by = c("aspect", "category")
) %>% 
  dplyr::mutate(
    significance = case_when(
      midp.exact < 0.05 | fisher.exact < 0.05 | chi.square < 0.05 ~ "occur",
      !is.na(midp.exact) & !is.na(fisher.exact) & !is.na(chi.square) ~ "not occur",
      TRUE ~ NA_character_
    )) %>% 
  dplyr::select(aspect, category, estimate, lower, upper, 
                midp.exact, fisher.exact, chi.square, significance)


# try glm OR report
or_univar_all <- generate_univar_report(df_input = df_epi_chars,
                                                   binary_disease = "final_pneumo_decision")

or_univar_model_report <- purrr::imap_dfr(or_univar_all, ~{
  model <- .x$model
  coefficients_df <- broom::tidy(model) %>%
    mutate(aspect = .y) %>%
    rename(category = term)
  
  model_stats <- dplyr::tibble(
    aspect = .y,
    null_deviance = model$null.deviance,
    residual_deviance = model$deviance,
    df_null = model$df.null,
    df_residual = model$df.residual,
    AIC = model$aic
  )
  
  # Combine everything into one table
  dplyr::left_join(coefficients_df, model_stats, by = "aspect")
}) %>% 
  dplyr::mutate(OR = exp(estimate), # estimate is log(OR)
                OR_lower_CI = exp(estimate - 1.96 * std.error),
                OR_upper_CI = exp(estimate + 1.96 * std.error),
                significance = case_when(
                  p.value < 0.05 ~ "occur",
                  !is.na(p.value) ~ "not occur",
                  TRUE ~ NA_character_)
  ) %>% 
  dplyr::arrange(aspect) %>% 
  dplyr::select(aspect, category, estimate, std.error,
                OR, OR_lower_CI, OR_upper_CI,
                statistic, p.value, significance,
                null_deviance, residual_deviance, df_null, df_residual, AIC)


# try glm multivariable OR report
or_multivariable_all <- generate_multivar_report(df_input = df_epi_chars,
                                                 binary_disease = "final_pneumo_decision") 

saveRDS(or_multivariable_all, "outputs/epi_all_multivariable_logistic_regression_model.rds")
# test <- readRDS("outputs/epi_all_multivariable_logistic_regression_model.rds")

# generate pictures
png(file = "pictures/epiAnalyses_all_multivariable_model1.png",
    width = 23, height = 23, unit = "cm", res = 600)
performance::check_model(or_multivariable_all$model1)
dev.off()

png(file = "pictures/epiAnalyses_all_multivariable_model2.png",
    width = 23, height = 23, unit = "cm", res = 600)
performance::check_model(or_multivariable_all$model2)
dev.off()

png(file = "pictures/epiAnalyses_all_multivariable_final_model_wider.png",
    width = 23, height = 23, unit = "cm", res = 600)
final_model_reconvert <- glm(formula(or_multivariable_all$final_model),
                             family = binomial, data = df_epi_chars)
performance::check_model(final_model_reconvert)
dev.off()


# generate justufucation report
df_models <- data.frame(
  Variable = names(coef(or_multivariable_all$model1)),
  Odds_Ratio = or_multivariable_all$odds_ratio1,
  Lower_CI = or_multivariable_all$lower_CI1,
  Upper_CI = or_multivariable_all$upper_CI1,
  P_Value = or_multivariable_all$p_value1,
  row.names = NULL
) %>% 
  dplyr::rename_all(~ paste0("model1_", .)) %>% 
  dplyr::left_join(
    data.frame(
      Variable = names(coef(or_multivariable_all$model2)),
      Odds_Ratio = or_multivariable_all$odds_ratio2,
      Lower_CI = or_multivariable_all$lower_CI2,
      Upper_CI = or_multivariable_all$upper_CI2,
      P_Value = or_multivariable_all$p_value2,
      row.names = NULL
    ) %>% 
      dplyr::rename_all(~ paste0("model2_", .))
    ,
    by = c("model1_Variable" = "model2_Variable")
  ) %>% 
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
  # view() %>% 
  glimpse()

write.csv(df_models, "outputs/epi_all_final_models.csv", row.names = F)

# convert multicollinearity test
df_multicol <- as.data.frame(or_multivariable_all$multicollinearity_test1) %>% 
  tibble::rownames_to_column(var = "Variable") %>% 
  dplyr::rename_all(~ paste0("multicol1_", .)) %>% 
  dplyr::left_join(
    as.data.frame(or_multivariable_all$multicollinearity_test2) %>% 
      tibble::rownames_to_column(var = "Variable") %>% 
      dplyr::rename_all(~ paste0("multicol2_", .))
    ,
    by = c("multicol1_Variable" = "multicol2_Variable")
  ) %>% 
  dplyr::rename_with(~ tolower(gsub("[^[:alnum:]_]", "", .x))) %>% 
  # view() %>% 
  glimpse()

write.csv(df_multicol, "outputs/epi_all_multicollinearity_test.csv", row.names = F)

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


# epiAnalyses for genome data (subset to positive pneumo only) #################



