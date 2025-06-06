

headers_panvita_strepto <- read.table("raw_data/result_panvita/VFDB_setA_compiled_headers_pro_streptococcus_only.txt",
                                        header = F, sep = "\t") %>%
  dplyr::rename(header = V1) %>% 
  dplyr::mutate(gene    = str_extract(header, "(?<= \\()[^\\)]+(?=\\) )"), # " (" & ") "
                gene_id = str_extract(header, "^[^)]*\\)"), # begin & ")"
                gene_class = str_extract(header, "(?<= - )[^\\(]+(?= \\()"),  # " - " & " ("
                species = str_extract(header, "(?<=\\] \\[)[^\\]]+(?=\\])") # "] [" & "]"
  ) %>% 
  # view() %>% 
  glimpse()

headers_panvita_nonpneumo <- read.table("raw_data/result_panvita/VFDB_setA_compiled_headers_pro.txt",
                                     header = F, sep = "\t") %>%
  dplyr::rename(header = V1) %>%
  dplyr::mutate(gene    = str_extract(header, "(?<= \\()[^\\)]+(?=\\) )"), # " (" & ") "
                gene_id = str_extract(header, "^[^)]*\\)"), # begin & ")"
                gene_class = str_extract(header, "(?<= - )[^\\(]+(?= \\()"),  # " - " & " ("
                species = str_extract(header, "(?<=\\] \\[)[^\\]]+(?=\\])") # "] [" & "]"
  ) %>%
  dplyr::filter(!grepl("Streptococcus", species)) %>% 
  # view() %>%
  glimpse()

# load panvita results
vir_mtx <- read.csv("raw_data/result_panvita/Results_vfdb_13-05-2025_11-27-19/matriz_vfdb.csv", sep = ";") %>% 
  dplyr::select(-X) %>% 
  tidyr::pivot_longer(cols = 2:ncol(.),
                      names_to = "gene",
                      values_to = "aa_percent") %>% 
  # dplyr::left_join(
  #   dat_list_all_data
  #   ,
  #   by = c("Strains" = "id_cleaned")
  # ) %>% 
  dplyr::mutate(gene = gene %>%
                  str_replace("srtC", "srtC-") %>%
                  str_replace("-.", "-") %>%
                  str_replace("\\.", "/")
  ) %>% 
  glimpse()

# test pcpA (cbpA-like) from matrix result
# I don't think pcpA in included in panvita's database
test_pcpA <- vir_mtx %>% 
  dplyr::filter(grepl("pcp", gene)) %>% 
  glimpse()

df_joined <- fuzzyjoin::regex_left_join(
  vir_mtx, headers_panvita_strepto,
  by = c("gene" = "gene")
) %>% 
  dplyr::mutate(
    gene_class = case_when(
      str_detect(gene.x, "pav|pfb|srt|pce|cbp") ~ "Adherence",
      str_detect(gene.x, "cps") ~ "Immune modulation",
      str_detect(gene.x, "gnd") ~ "Nutritional/Metabolic factor",
      TRUE ~ gene_class
    )
  ) %>% 
  dplyr::filter(str_detect(species, "Strepto")) %>% 
  dplyr::right_join(
    read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
      # dplyr::filter(workPoppunk_qc == "pass_qc") %>%
      dplyr::filter(workWGS_species_pw == "Streptococcus pneumoniae") %>% 
      dplyr::select(workFasta_name,
                    serotype_classification_PCV13_final_decision,
                    serotype_final_decision,
                    workWGS_gpsc_strain,
                    workWGS_MLST_dc_ST,
                    area,
                    workPoppunk_qc
                    )
    ,
    by = c("Strains" = "workFasta_name")
  ) %>% 
  dplyr::mutate(
    gene_priority = ifelse(str_detect(gene.x, "pspA|pspC|cbpA|pavA|pcpA|ply|lytA"), "priority", "no"), # pcpA not detected
    gene_present_absent = ifelse(aa_percent == 0, FALSE, TRUE)
  ) %>% 
  dplyr::filter(gene_priority == "priority",
                workPoppunk_qc == "pass_qc") %>% 
  # view() %>% 
  glimpse()

# long
df_joined_long <- df_joined %>% 
  dplyr::mutate(
    gene.x = case_when(
     gene.x == "cbpA/pspC" ~ "cbpA.pspC",
     TRUE ~ gene.x
    ),
    gene_class = case_when(
      gene_class == "Immune modulation" ~ "Immune.modulation",
      TRUE ~ gene_class
    )
  ) %>% 
  tidyr::pivot_wider(
    id_cols = c("serotype_classification_PCV13_final_decision",
                "serotype_final_decision",
                "workWGS_gpsc_strain",
                "workWGS_MLST_dc_ST",
                "area",
                "Strains"),
    names_from = c("gene.x", "gene_class"),
    # names_glue = gene_class,
    values_from = c("gene_present_absent", "aa_percent")
  ) %>% 
  dplyr::mutate(
    gene_present_absent_lytA_Exoenzyme = case_when(
      gene_present_absent_lytA_Exoenzyme ~ "lytA",
      TRUE ~ "NF"
    ),
    gene_present_absent_lytA_Exoenzyme = factor(gene_present_absent_lytA_Exoenzyme, levels = c("NF", "lytA")),
    gene_present_absent_ply_Exotoxin = case_when(
      gene_present_absent_ply_Exotoxin ~ "ply",
      TRUE ~ "NF"
    ),
    gene_present_absent_ply_Exotoxin = factor(gene_present_absent_ply_Exotoxin, levels = c("NF", "ply")),
    gene_present_absent_pavA_Adherence = case_when(
      gene_present_absent_pavA_Adherence ~ "pavA",
      TRUE ~ "NF"
    ),
    gene_present_absent_pavA_Adherence = factor(gene_present_absent_pavA_Adherence, levels = c("NF", "pavA")),
    gene_present_absent_pspA_Immune.modulation = case_when(
      gene_present_absent_pspA_Immune.modulation ~ "pspA",
      TRUE ~ "NF"
    ),
    gene_present_absent_pspA_Immune.modulation = factor(gene_present_absent_pspA_Immune.modulation, levels = c("NF", "pspA")),
    gene_present_absent_cbpA.pspC_Adherence = case_when(
      gene_present_absent_cbpA.pspC_Adherence ~ "cbpA/pspC",
      TRUE ~ "NF"
    ),
    gene_present_absent_cbpA.pspC_Adherence = factor(gene_present_absent_cbpA.pspC_Adherence, levels = c("NF", "cbpA/pspC"))
  ) %>% 
  glimpse()

write.csv(df_joined_long, "inputs/genData_pneumo_panvita_long.csv")



# test visualisation
# faceted by gene
png("pictures/genData_panvita_priority_aa_percent_1allgenes.png",
    width = 20, height = 20, units = "cm", res = 300)
ggplot(df_joined,
       aes(x = serotype_classification_PCV13_final_decision, y = aa_percent, fill = serotype_classification_PCV13_final_decision, colour = serotype_classification_PCV13_final_decision)) +
  geom_violin(trim = TRUE, alpha = 0.4, position = position_dodge(width = 0.8)) +
  # geom_boxplot(alpha = 0.4, aes(fill = source)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1) +
  facet_wrap(~ gene.x, drop = F) +
  theme_bw() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.position = "bottom")
dev.off()

# faceted by gene_class
png("pictures/genData_panvita_priority_aa_percent_2geneClass.png",
    width = 20, height = 15, units = "cm", res = 300)
ggplot(df_joined,
       aes(x = serotype_final_decision, y = aa_percent, fill = serotype_classification_PCV13_final_decision, colour = serotype_classification_PCV13_final_decision)) +
  geom_violin(trim = TRUE, alpha = 0.4, position = position_dodge(width = 0.8)) +
  # geom_boxplot(alpha = 0.4, aes(fill = serotype_classification_PCV13_final_decision)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1) +
  facet_wrap(~ gene_class, drop = F) +
  theme_bw() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.position = "bottom")
dev.off()


################################################################################
# filtered by serotype_classification_PCV13_final_decision only
png("pictures/genData_panvita_priority_aa_percent_3a_adherence.png",
    width = 20, height = 15, units = "cm", res = 300)
ggplot(df_joined %>% 
         dplyr::filter(gene_class == "Adherence"),
       aes(x = serotype_final_decision, y = aa_percent, fill = serotype_classification_PCV13_final_decision, colour = serotype_classification_PCV13_final_decision)) +
  geom_violin(trim = TRUE, alpha = 0.4, position = position_dodge(width = 0.8)) +
  # geom_boxplot(alpha = 0.4, aes(fill = serotype_classification_PCV13_final_decision)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1) +
  facet_wrap(~ gene.x, drop = F) +
  theme_bw() +
  labs(title = "Adherence") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.position = "bottom")
dev.off()

png("pictures/genData_panvita_priority_aa_percent_3b_exoenzyme.png",
    width = 20, height = 8, units = "cm", res = 300)
ggplot(df_joined %>% 
         dplyr::filter(gene_class == "Exoenzyme"),
       aes(x = serotype_final_decision, y = aa_percent, fill = serotype_classification_PCV13_final_decision, colour = serotype_classification_PCV13_final_decision)) +
  geom_violin(trim = TRUE, alpha = 0.4, position = position_dodge(width = 0.8)) +
  # geom_boxplot(alpha = 0.4, aes(fill = serotype_classification_PCV13_final_decision)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1) +
  facet_wrap(~ gene.x, drop = F) +
  theme_bw() +
  labs(title = "Exoenzyme") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.position = "bottom")
dev.off()

png("pictures/genData_panvita_priority_aa_percent_3c_exotoxin.png",
    width = 10, height = 8, units = "cm", res = 300)
ggplot(df_joined %>% 
         dplyr::filter(gene_class == "Exotoxin"),
       aes(x = serotype_final_decision, y = aa_percent, fill = serotype_classification_PCV13_final_decision, colour = serotype_classification_PCV13_final_decision)) +
  geom_violin(trim = TRUE, alpha = 0.4, position = position_dodge(width = 0.8)) +
  # geom_boxplot(alpha = 0.4, aes(fill = serotype_classification_PCV13_final_decision)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1) +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ gene.x, drop = F) +
  theme_bw() +
  labs(title = "Exotoxin") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.position = "bottom")
dev.off()

png("pictures/genData_panvita_priority_aa_percent_3d_exoenzyme.png",
    width = 20, height = 15, units = "cm", res = 300)
ggplot(df_joined %>% 
         dplyr::filter(gene_class == "Immune modulation"),
       aes(x = serotype_final_decision, y = aa_percent, fill = serotype_classification_PCV13_final_decision, colour = serotype_classification_PCV13_final_decision)) +
  geom_violin(trim = TRUE, alpha = 0.4, position = position_dodge(width = 0.8)) +
  # geom_boxplot(alpha = 0.4, aes(fill = serotype_classification_PCV13_final_decision)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1) +
  facet_wrap(~ gene.x, drop = F) +
  theme_bw() +
  labs(title = "Immune modulation") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.position = "bottom")
dev.off()

# png("pictures/genData_panvita_priority_aa_percent_3e_nutritional_metabolicFactor.png",
#     width = 20, height = 10, units = "cm", res = 300)
# ggplot(df_joined %>% 
#          dplyr::filter(gene_class == "Nutritional/Metabolic factor"),
#        aes(x = serotype_final_decision, y = aa_percent, fill = serotype_classification_PCV13_final_decision, colour = serotype_classification_PCV13_final_decision)) +
#   geom_violin(trim = TRUE, alpha = 0.4, position = position_dodge(width = 0.8)) +
#   # geom_boxplot(alpha = 0.4, aes(fill = serotype_classification_PCV13_final_decision)) +
#   geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1) +
#   scale_y_continuous(limits = c(0, 100)) +
#   facet_wrap(~ gene.x, drop = F) +
#   theme_bw() +
#   labs(title = "Nutritional/Metabolic factor") +
#   theme(strip.text = element_text(size = 10, face = "bold"),
#         legend.position = "bottom")
# dev.off()



