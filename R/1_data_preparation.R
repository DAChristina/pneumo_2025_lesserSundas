library(tidyverse)
source("global/fun.R")

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
