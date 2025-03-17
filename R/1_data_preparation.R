library(tidyverse)
library(readxl)
source("global/fun.R")

df_epi_lombok <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver2.xlsx",
                                 sheet = "Lombok") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub(" ", "_", SPECIMEN_ID),
                area = "Lombok",
                across(where(is.character), ~ na_if(., "N/A")),
                across(where(is.character), ~ if_else(. == "-", "tidak", .)))
                # across(everything(), as.character),
                # across(everything(), tolower)) %>% 
  # dplyr::select(-contains("koding"),-contains("Ya="))

write.csv(df_epi_lombok, "raw_data/temporary_df_epi_lombok.csv",
          row.names = F)

df_epi_sumbawa <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver2.xlsx",
                                    sheet = "Sumbawa") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub("-", "_", SPECIMEN_ID),
                area = "Sumbawa",
                across(where(is.character), ~ na_if(., "N/A")),
                across(where(is.character), ~ if_else(. == "-", "tidak", .)))
                # across(everything(), as.character),
                # across(everything(), tolower)) %>% 
  # dplyr::select(-contains("koding"),-contains("Ya="))

write.csv(df_epi_sumbawa, "raw_data/temporary_df_epi_sumbawa.csv",
          row.names = F)

setdiff(names(df_epi_lombok), names(df_epi_sumbawa))
setdiff(names(df_epi_sumbawa), names(df_epi_lombok))

# In the end, I manually merge Lombok & Sumbawa dfs (column differences occur)
# Do not trust coded columns.

df_epi_merged <- read.csv("raw_data/temporary_df_epi_lombok_sumbawa_manual_combine_row.csv")








# pending
# column types need to be corrected and re-checked
df_epi_ls <- dplyr::bind_rows(df_epi_lombok #%>% 
                                # dplyr::select(-contains("koding"))
                              , 
                              df_epi_sumbawa #%>% 
                                # dplyr::select(-contains("koding"))
                                ) %>% 
  dplyr::mutate(across(where(is.character), ~ na_if(., "N/A")))
                                
# Test weird unique value
lapply(df_epi_ls, unique)

sapply(df_epi_ls, unique)

df_epi_ls_summarise <- df_epi_ls %>% 
  dplyr::summarise(across(everything(), ~ list(as.data.frame(table(.))))) %>% 
  tidyr::unnest(cols = everything())

view(t(df_epi_ls_summarise))


# Data picking framework for analysis
# Priority columns
df_combined_epi_priority <- dplyr::bind_rows(
  df_epi_lombok %>% 
    dplyr::select(2:15, # laboratory data
                  16:18,20,22,23, # identitas anak
                  25,27,29, # ASI
                  
                  area)
  ,
  df_epi_sumbawa %>% 
    dplyr::select(2:15, # laboratory data
                  16:18,20,22,23, # identitas anak
                  25,27,29, # ASI
                  
                  area)
)









# test available fasta
df_confirm_lombok <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver2.xlsx",
                                        sheet = "need confirmation-lombok") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub(" ", "_", SPECIMEN_ID),
                area = "Lombok")

df_confirm_sumbawa <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver2.xlsx",
                                         sheet = "need confirmation-sumbawa_DC_ed") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub(" ", "_", SPECIMEN_ID),
                area = "Sumbawa")


# Generate report
df_epi_test <- dplyr::bind_rows(
  df_epi_lombok %>% 
    dplyr::select(2:13, area)
  ,
  df_epi_sumbawa %>% 
    dplyr::select(2:13, area)
) %>% 
  # mutate all "N/A" values
  dplyr::mutate(across(where(is.character), ~ na_if(., "N/A"))) %>% 
  dplyr::left_join(read.table("raw_data/test_available_fasta_renamed.txt", header = FALSE) %>% 
                     dplyr::mutate(file_check = "Accepted_by_DC",
                                   SPECIMEN_ID = gsub("Streptococcus_pneumoniae_", "", V1),
                                   SPECIMEN_ID = gsub(".fasta", "", SPECIMEN_ID),
                                   fasta_name = V1) %>% 
                     dplyr::select(-V1)
                   ,
                   by = "SPECIMEN_ID"
                   ) %>% 
  view()

area_report <- df_epi_test %>% 
  dplyr::group_by(area, file_check) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::ungroup() %>% 
  view()

# further report
area_suspect_report <- df_epi_test %>% 
  dplyr::group_by(area, S._pneumoniae_suspect_culture_colony, S._pneumoniae_culture_result, file_check) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::ungroup() %>% 
  view()


write.csv(df_epi_test, "report/temporary_available_fasta_list_in_DC.csv",
         row.names = F)
# excel report
writexl::write_xlsx(list(Sheet1 = df_epi_test,
                         Sheet2 = area_report,
                         Sheet3 = area_suspect_report),
                    "report/temporary_available_fasta_list_in_DC.xlsx")


df_epi_test <- read.csv("report/temporary_available_fasta_list_in_DC.csv") %>% 
  dplyr::mutate(
    final_pneumo_decision = case_when(
      WGS_Result...11 == "Streptococcus pneumoniae" ~ "Positive",
      S._pneumoniae_suspect_culture_colony == "Yes" & S._pneumoniae_culture_result == "Pos" & WGS_Result...11 == "Streptococcus pneumoniae" ~ "Positive",
      S._pneumoniae_suspect_culture_colony == "Yes" & S._pneumoniae_culture_result == "Pos" & WGS_Result...11 != "Streptococcus pneumoniae" ~ "Negative",  # OR "Failed_to_be_extracted"?
      TRUE ~ S._pneumoniae_culture_result
    ),
    final_pneumo_decision = case_when(
      final_pneumo_decision == "Neg" ~ "Negative",
      is.na(final_pneumo_decision) ~ "Negative",
      TRUE ~ final_pneumo_decision  # Ensure other values remain unchanged
    )
  )

# Quick viz serotypes
# Compute percentage
df_serotype_summary <- df_epi_test %>% 
  dplyr::filter(!is.na(SEROTYPE_.WGS.)) %>% 
  dplyr::count(SEROTYPE_.WGS.) %>%
  dplyr::mutate(Percentage = n / sum(n) * 100)

ggplot(df_serotype_summary, aes(x = SEROTYPE_.WGS., y = Percentage, fill = SEROTYPE_.WGS.)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Category", y = "Percentage", 
       title = "All Serotypes") +
  theme_bw() +
  theme(legend.position = "none")

# Compute percentage by area
df_serotype_area_summary <- df_epi_test %>% 
  dplyr::filter(!is.na(SEROTYPE_.WGS.)) %>% 
  dplyr::group_by(area, SEROTYPE_.WGS.) %>%
  dplyr::summarise(Count = n(), .groups = "drop") %>%
  dplyr::mutate(Percentage = Count / sum(Count) * 100)

ggplot(df_serotype_area_summary, aes(x = SEROTYPE_.WGS., y = Percentage, fill = SEROTYPE_.WGS.)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Category", y = "Percentage", 
       title = "All Serotypes") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~ area)

# right_join to see undetected 49 fasta files
df_epi_test_right_join <- dplyr::bind_rows(
  df_epi_lombok %>% 
    dplyr::select(2:13, area)
  ,
  df_epi_sumbawa %>% 
    dplyr::select(2:13, area)
) %>% 
  dplyr::right_join(read.table("raw_data/test_available_fasta_renamed.txt", header = FALSE) %>% 
                     dplyr::mutate(file_check = "Accepted_by_DC",
                                   SPECIMEN_ID = gsub("Streptococcus_pneumoniae_", "", V1),
                                   SPECIMEN_ID = gsub(".fasta", "", SPECIMEN_ID),
                                   fasta_name = V1) %>% 
                     dplyr::select(-V1)
                   ,
                   by = "SPECIMEN_ID"
  ) %>% 
  view()

undetected_fasta_report <- df_epi_test_right_join %>% 
  dplyr::filter(is.na(Processing_Status)) %>% 
  dplyr::mutate(V1 = paste0("/home/ron/pneumo_2025_lesserSundas/raw_data/fasta/", V1)) %>% 
  dplyr::select(V1)

write.table(undetected_fasta_report, "raw_data/test_undetected_49_fasta.txt",
            row.names = F, col.names = F, quote = F)




























df_epi <- read.csv("raw_data/oral_cancer_prediction_dataset.csv") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_"))

df_gen <- df_epi %>% 
  dplyr::filter(Oral.Cancer..Diagnosis. == "Yes") %>% 
  dplyr::mutate(Sero_rand = sample(c(1:100), n(), replace = T),
                GPSC_rand = sample(c(1:200), n(), replace = T),
                MLST_rand = sample(c(30:1000), n(), replace = T)) %>% 
  dplyr::select(ID, Sero_rand, GPSC_rand, MLST_rand)

write.csv(df_gen, "raw_data/generate_genomics.csv", row.names = F)

################################################################################

meta <- dplyr::left_join(
  read.csv("raw_data/oral_cancer_prediction_dataset.csv"),
  read.csv("raw_data/generate_genomics.csv")
  ,
  by = "ID"
)

write.csv(meta, "raw_data/generate_combined_metadata.csv", row.names = F)


df_test <- df_epi %>% 
  select(Country, Age, Gender, Tobacco.Use, Oral.Cancer..Diagnosis.) %>% 
  mutate(Age = as.character(Age)) %>% 
  select(-Age)

# Age failed to be reported; should edit the continuous, numerical value to categorical data instead

# try OR report works only for categorical data
or_matrix_all <- generate_or_matrix_report(df_input = df_test, binary_disease = "Oral.Cancer..Diagnosis.")

or_matrix_table_report <- purrr::map_dfr(names(or_matrix_all), function(var) {
  df <- or_matrix_all[[var]]$measure
  if (is.null(df)) return(NULL)
  df <- as.data.frame(df) # annoying df convertion
  
  df <- df %>% 
    dplyr::mutate(Aspect = var, Category = rownames(df)) 
  
  rownames(df) <- NULL
  return(df)
})

# try glm OR report
or_logistic_all <- generate_glm_logistic_report(df_input = df_test, binary_disease = "Oral.Cancer..Diagnosis.")

or_logistic_table_report <- purrr::imap_dfr(or_logistic_all, ~{
  dplyr::tibble(
    Names = names(.x$odds_ratio),
    Odds_Ratio = .x$odds_ratio,
    Category = .y
  )
}) %>% 
  dplyr::arrange(Category)


# try glm multivariable OR report
or_multivariable_all <- generate_glm_multivariable_report(df_input = df_test, binary_disease = "Oral.Cancer..Diagnosis.")
