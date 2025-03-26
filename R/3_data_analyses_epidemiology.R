library(tidyverse)
source("global/fun.R")

df_epi_coded <- read.csv("raw_data/temporary_df_epi_lombok_sumbawa_manual_combine_row_cleaned_coded.csv") %>% 
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
    house_building = as.factor(house_building),
    house_roof = as.factor(house_roof),
    house_window = as.factor(house_window),
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

column_names <- setdiff(names(df_epi_coded), "final_pneumo_decision")

# Test epiFunction ehehe
# try OR report works only for categorical data
df_epi_coded_chars <- df_epi_coded %>% 
  dplyr::select(where(~ !all(is.na(.))), # NAs in workLab, workFasta & not interesting columns
                -sickness_past3days_fever_howManyDays, # conflicted values with fever column
                -contains("coded_"),
                -contains("work"),
                -where(is.character))

or_matrix_all <- generate_or_matrix_report(df_input = df_epi_coded_chars,
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
or_logistic_all <- generate_univar_report(df_input = df_epi_coded_chars,
                                                   binary_disease = "final_pneumo_decision")

or_logistic_model_report <- purrr::imap_dfr(or_logistic_all, ~{
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
or_multivariable_all <- generate_multivar_report(df_input = df_epi_coded_chars, binary_disease = "final_pneumo_decision")

or_multivariable_all3 <- generate_glm_multivariable_report(df_input = df_epi_coded_chars, binary_disease = "final_pneumo_decision")




