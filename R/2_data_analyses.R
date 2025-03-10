# hypothetical clean data
df_meta <- read.csv("raw_data/generate_combined_metadata.csv")

# list all column types, separated into numeric or categorisation
# 1. cat use chi square association test OR Fisher's exact
# 2. num normal use SW followed by t OR ANOVA OR Pearson's
# 3. num skewed use KW followed by Dunn's OR Wilcox

dat_numeric <- df_meta %>% 
  dplyr::select(ID, Oral.Cancer..Diagnosis.,
                where(is.numeric))

dat_cat <- df_meta %>% 
  dplyr::select(ID, Oral.Cancer..Diagnosis.,
                where(~ is.factor(.) | is.character(.)))

