library(tidyverse)
source("global/fun.R")

df_epi_gen_pneumo <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
  # dplyr::filter(workPoppunk_qc == "pass_qc") %>%
  dplyr::filter(workWGS_species_pw == "Streptococcus pneumoniae") %>% 
  dplyr::mutate(
    serotype_final_decision = case_when(
      serotype_final_decision == "mixed serotypes/serogroups" ~ "mixed serogroups",
      TRUE ~ serotype_final_decision
    ),
    serotype_final_decision = factor(serotype_final_decision,
                                     levels = c(
                                       # VT
                                       # "3", "6A/6B", "6A/6B/6C/6D", "serogroup 6",
                                       # "14", "17F", "18C/18B", "19A", "19F", "23F",
                                       "1", "3", "4", "5", "7F",
                                       "6A", "6B", "9V", "14", "18C",
                                       "19A", "19F", "23F",
                                       # NVT
                                       # "7C", "10A", "10B", "11A/11D", "13", "15A", "15B/15C",
                                       # "16F", "19B", "20", "23A", "23B", "24F/24B", "25F/25A/38",
                                       # "28F/28A", "31", "34", "35A/35C/42", "35B/35D", "35F",
                                       # "37", "39", "mixed serogroups",
                                       "serogroup 6", "6C", "7C", "10A", "10B",
                                       "11A", "13", "15A", "15B", "15C", "16F",
                                       "17F", "18B", "19B", "20", "23A", "23B",
                                       "23B1", "24F", "25B", "28A", "31", "33B",
                                       "34", "35A", "35B", "35C", "35F", "37",
                                       "37F", "38", "39",
                                       "untypeable")),
    serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                          levels = c("VT", "NVT", "untypeable")),
    serotype_classification_PCV15_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                          levels = c("VT", "NVT", "untypeable"))
  ) %>%
  dplyr::select(-specimen_id,
                -workWGS_no,
                -contains("workFasta")) %>% 
  glimpse()

table(df_epi_gen_pneumo$area)
# I think ideally can be compared by proportion/percentage instead

# test area vs. vaccine types groupings
test <- chisq.test(df_epi_gen_pneumo$area, df_epi_gen_pneumo$serotype_classification_PCV13_final_decision)
test$observed
test$stdres # post-hoc, usually find those with stdRes > |2|
test

# I suspect for statistically significant difference occurs among areas
test_ageG <- fisher.test(df_epi_gen_pneumo$serotype_classification_PCV13_final_decision,
                         df_epi_gen_pneumo$age_year_3groups, workspace = 2e7)
# test_postHoc <- rstatix::pairwise_fisher_test(table(df_epi_gen_pneumo$serotype_classification_PCV13_final_decision,
#                                                     df_epi_gen_pneumo$age_year_3groups),
#                                               p.adjust.method = "bonferroni")
# test_postHoc

test_vacc <- chisq.test(df_epi_gen_pneumo$serotype_classification_PCV13_final_decision,
                        df_epi_gen_pneumo$vaccination_pcv13_dc_n_regroup)
test_vacc$observed
chisq.posthoc.test::chisq.posthoc.test(test_vacc$observed, method = "bonferroni")
# for (j in 1:3) print(chisq.test(test_vacc$observed[, -j])) # p MUST be corrected


# 1. Area vs. everything ########################################################
ptest_matrix_area <- generate_or_chisq_report(df_input = df_epi_gen_pneumo,
                                             binary_disease = "area")

ptest_matrix_area_report <- purrr::imap_dfr(ptest_matrix_area, ~{
  chisq <- .x$chisq_result
  fisher <- .x$fisher_result
  
  # extract stdres
  stdres_tbl <- NULL
  if (!is.null(chisq$stdres)) {
    stdres_tbl <- as.data.frame(as.table(chisq$stdres)) %>%
      rename(row = Var1, col = Var2, stdres = Freq) %>%
      mutate(variable = .y)
  }
  
  summary_tbl <- tidyr::tibble(
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
  
  list(summary = summary_tbl, stdres = stdres_tbl)
}) %>% 
  glimpse()

summary_area_report <- bind_rows(ptest_matrix_area_report$summary) %>% 
  dplyr::mutate(
    significance = case_when(
      fisher_p < 0.05 | chisq_p < 0.05 ~ "occur",
      !is.na(fisher_p) & !is.na(chisq_p) ~ "no",
      TRUE ~ NA_character_
    )
  ) %>% 
  dplyr::distinct(variable, .keep_all = T) %>% 
  glimpse()

stdres_area_report <- bind_rows(ptest_matrix_area_report$stdres) %>% 
  dplyr::select(variable, row, col, stdres) %>% 
  glimpse()

# note: DO NOT join summary & stdres; resulted in a superhuge df with >> 2M rows
write.csv(summary_area_report,
          "outputs/epi_genomes_area_vs_everything_chisq_pValues.csv",
          row.names = F)
write.csv(stdres_area_report,
          "outputs/epi_genomes_area_vs_everything_chisq_pValues_postHoc.csv",
          row.names = F)


# 2. PCV13_VT vs. everything ###################################################
ptest_matrix_PCV13_VT <- generate_or_chisq_report(df_input = df_epi_gen_pneumo,
                                                  binary_disease = "serotype_classification_PCV13_final_decision")

ptest_matrix_PCV13_VT_report <- purrr::imap_dfr(ptest_matrix_PCV13_VT, ~{
  chisq <- .x$chisq_result
  fisher <- .x$fisher_result
  
  # extract stdres
  stdres_tbl <- NULL
  if (!is.null(chisq$stdres)) {
    stdres_tbl <- as.data.frame(as.table(chisq$stdres)) %>%
      rename(row = Var1, col = Var2, stdres = Freq) %>%
      mutate(variable = .y)
  }
  
  summary_tbl <- tidyr::tibble(
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
  
  list(summary = summary_tbl, stdres = stdres_tbl)
}) %>% 
  glimpse()

summary_PCV13_VT_report <- bind_rows(ptest_matrix_PCV13_VT_report$summary) %>% 
  dplyr::mutate(
    significance = case_when(
      fisher_p < 0.05 | chisq_p < 0.05 ~ "occur",
      !is.na(fisher_p) & !is.na(chisq_p) ~ "no",
      TRUE ~ NA_character_
    )
  ) %>% 
  dplyr::distinct(variable, .keep_all = T) %>% 
  glimpse()

stdres_PCV13_VT_report <- bind_rows(ptest_matrix_PCV13_VT_report$stdres) %>% 
  dplyr::select(variable, row, col, stdres) %>% 
  glimpse()

# note: DO NOT join summary & stdres; resulted in a superhuge df with >> 2M rows
write.csv(summary_PCV13_VT_report,
          "outputs/epi_genomes_PCV13_VT_vs_everything_chisq_pValues.csv",
          row.names = F)
write.csv(stdres_PCV13_VT_report,
          "outputs/epi_genomes_PCV13_VT_vs_everything_chisq_pValues_postHoc.csv",
          row.names = F)


# 3. PCV15_VT vs. everything ###################################################
ptest_matrix_PCV15_VT <- generate_or_chisq_report(df_input = df_epi_gen_pneumo,
                                                  binary_disease = "serotype_classification_PCV15_final_decision")

ptest_matrix_PCV15_VT_report <- purrr::imap_dfr(ptest_matrix_PCV15_VT, ~{
  chisq <- .x$chisq_result
  fisher <- .x$fisher_result
  
  # extract stdres
  stdres_tbl <- NULL
  if (!is.null(chisq$stdres)) {
    stdres_tbl <- as.data.frame(as.table(chisq$stdres)) %>%
      rename(row = Var1, col = Var2, stdres = Freq) %>%
      mutate(variable = .y)
  }
  
  summary_tbl <- tidyr::tibble(
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
  
  list(summary = summary_tbl, stdres = stdres_tbl)
}) %>% 
  glimpse()

summary_PCV15_VT_report <- bind_rows(ptest_matrix_PCV15_VT_report$summary) %>% 
  dplyr::mutate(
    significance = case_when(
      fisher_p < 0.05 | chisq_p < 0.05 ~ "occur",
      !is.na(fisher_p) & !is.na(chisq_p) ~ "no",
      TRUE ~ NA_character_
    )
  ) %>% 
  dplyr::distinct(variable, .keep_all = T) %>% 
  glimpse()

stdres_PCV15_VT_report <- bind_rows(ptest_matrix_PCV15_VT_report$stdres) %>% 
  dplyr::select(variable, row, col, stdres) %>% 
  glimpse()

# note: DO NOT join summary & stdres; resulted in a superhuge df with >> 2M rows
write.csv(summary_PCV15_VT_report,
          "outputs/epi_genomes_PCV15_VT_vs_everything_chisq_pValues.csv",
          row.names = F)
write.csv(stdres_PCV15_VT_report,
          "outputs/epi_genomes_PCV15_VT_vs_everything_chisq_pValues_postHoc.csv",
          row.names = F)


# 4. AMR_MDR vs. everything ##################################################
# using serotypes are too slow resulted in multiple crashes.
# serotype or GPSC vs. AMR were not done due to high variability

# test popPUNK qc vs. AMR flag
filtered_gen_epi_qc_simplified <- df_epi_gen_pneumo %>% 
  dplyr::select(serotype_classification_PCV13_final_decision,
                serotype_final_decision,
                workWGS_gpsc_strain,
                workWGS_MLST_dc_ST,
                area,
                workPoppunk_qc,
                contains(c("AMR_logic", "MDR"))
  ) %>% 
  dplyr::mutate(qc_simplified = ifelse(workPoppunk_qc == "pass_qc", "pass", "no")
  ) %>% 
  glimpse()

ptest_matrix_qc_simplified <- generate_or_chisq_report(df_input = filtered_gen_epi_qc_simplified,
                                                        binary_disease = "qc_simplified")

ptest_matrix_qc_simplified_report <- purrr::imap_dfr(ptest_matrix_qc_simplified, ~{
  chisq <- .x$chisq_result
  fisher <- .x$fisher_result
  
  # extract stdres
  stdres_tbl <- NULL
  if (!is.null(chisq$stdres)) {
    stdres_tbl <- as.data.frame(as.table(chisq$stdres)) %>%
      rename(row = Var1, col = Var2, stdres = Freq) %>%
      mutate(variable = .y)
  }
  
  summary_tbl <- tidyr::tibble(
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
  
  list(summary = summary_tbl, stdres = stdres_tbl)
}) %>% 
  glimpse()

summary_qc_simplified_report <- bind_rows(ptest_matrix_qc_simplified_report$summary) %>% 
  dplyr::mutate(
    significance = case_when(
      fisher_p < 0.05 | chisq_p < 0.05 ~ "occur",
      !is.na(fisher_p) & !is.na(chisq_p) ~ "no",
      TRUE ~ NA_character_
    )
  ) %>% 
  dplyr::distinct(variable, .keep_all = T) %>% 
  glimpse()

stdres_qc_simplified_report <- bind_rows(ptest_matrix_qc_simplified_report$stdres) %>% 
  dplyr::select(variable, row, col, stdres) %>% 
  glimpse()

# Statictically significance difference for antifolates and tetracyclines
# Would rather use filtered samples with pass qc for AMR & virulence factor analysis

# test filtered data, PCV13 groups vs. AMR flag
filtered_gen_epi_pass_qc <- df_epi_gen_pneumo %>% 
  dplyr::select(serotype_classification_PCV13_final_decision,
                serotype_final_decision,
                workWGS_gpsc_strain,
                workWGS_MLST_dc_ST,
                area,
                workPoppunk_qc,
                contains(c("AMR_logic", "MDR"))
  ) %>% 
  dplyr::mutate(qc_simplified = ifelse(workPoppunk_qc == "pass_qc", "pass", "no")
  ) %>% 
  dplyr::filter(qc_simplified == "pass") %>% 
  glimpse()

ptest_matrix_AMR_simplified <- generate_or_chisq_report(df_input = filtered_gen_epi_pass_qc,
                                                        binary_disease = "serotype_classification_PCV13_final_decision")

ptest_matrix_AMR_simplified_report <- purrr::imap_dfr(ptest_matrix_AMR_simplified, ~{
  chisq <- .x$chisq_result
  fisher <- .x$fisher_result
  
  # extract stdres
  stdres_tbl <- NULL
  if (!is.null(chisq$stdres)) {
    stdres_tbl <- as.data.frame(as.table(chisq$stdres)) %>%
      rename(row = Var1, col = Var2, stdres = Freq) %>%
      mutate(variable = .y)
  }
  
  summary_tbl <- tidyr::tibble(
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
  
  list(summary = summary_tbl, stdres = stdres_tbl)
}) %>% 
  glimpse()

summary_AMR_simplified_report <- bind_rows(ptest_matrix_AMR_simplified_report$summary) %>% 
  dplyr::mutate(
    significance = case_when(
      fisher_p < 0.05 | chisq_p < 0.05 ~ "occur",
      !is.na(fisher_p) & !is.na(chisq_p) ~ "no",
      TRUE ~ NA_character_
    )
  ) %>% 
  dplyr::distinct(variable, .keep_all = T) %>% 
  glimpse()

stdres_AMR_simplified_report <- bind_rows(ptest_matrix_AMR_simplified_report$stdres) %>% 
  dplyr::select(variable, row, col, stdres) %>% 
  glimpse()

# note: DO NOT join summary & stdres; resulted in a superhuge df with >> 2M rows
write.csv(summary_AMR_simplified_report,
          "outputs/epi_genomes_seroGroups_filtered_vs_AMR_chisq_pValues.csv",
          row.names = F)
write.csv(stdres_AMR_simplified_report,
          "outputs/epi_genomes_seroGroups_filtered_vs_AMR_chisq_pValues_postHoc.csv",
          row.names = F)




























