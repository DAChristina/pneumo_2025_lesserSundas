library(tidyverse)
library(readxl)
source("global/fun.R")

df_epi_lombok <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa).xlsx",
                                 sheet = "Lombok") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::left_join(readxl::read_excel("raw_data/Data WGS_Lombok.xlsx", sheet = "Sheet1") %>% 
                     dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
                     dplyr::rename(SPECIMEN_ID = No_Isolat),
                   by = "SPECIMEN_ID") %>% 
  dplyr::mutate(SPECIMEN_ID = gsub(" ", "_", SPECIMEN_ID),
                area = "Lombok")

df_epi_sumbawa <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa).xlsx",
                                    sheet = "Sumbawa") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub("-", "_", SPECIMEN_ID),
                area = "Sumbawa")

# pending
# column types need to be corrected and re-checked
df_epi_ls <- dplyr::bind_rows(df_epi_lombok %>% 
                                dplyr::mutate("koding_jenis_atap_Rumah_(_Seng=_1,_Asbes=2_,_Beton=3__,_Kayu=4,_Genteng=_5,_Spandek=6,_lain-lain_(Jerami,_daun_palem)=7" = as.numeric("koding_jenis_atap_Rumah_(_Seng=_1,_Asbes=2_,_Beton=3__,_Kayu=4,_Genteng=_5,_Spandek=6,_lain-lain_(Jerami,_daun_palem)=7")),
                              df_epi_sumbawa %>% 
                                dplyr::mutate("koding_jenis_atap_Rumah_(_Seng=_1,_Asbes=2_,_Beton=3__,_Kayu=4,_Genteng=_5,_Spandek=6,_lain-lain_(Jerami,_daun_palem)=7" = as.numeric("koding_jenis_atap_Rumah_(_Seng=_1,_Asbes=2_,_Beton=3__,_Kayu=4,_Genteng=_5,_Spandek=6,_lain-lain_(Jerami,_daun_palem)=7")))

# test available fasta
df_confirm_lombok <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa).xlsx",
                                        sheet = "need confirmation-lombok") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub(" ", "_", SPECIMEN_ID),
                area = "Lombok")

df_confirm_sumbawa <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa).xlsx",
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


write.csv(df_epi_test, "outputs/temporary_available_fasta_list_in_DC.csv",
         row.names = F)
# excel report
writexl::write_xlsx(list(Sheet1 = df_epi_test,
                         Sheet2 = area_report,
                         Sheet3 = area_suspect_report),
                    "outputs/temporary_available_fasta_list_in_DC.xlsx")


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


# try glm OR report for numerical data
