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

# Detect duplicated IDs
df_epi_lombok_duplicated_ids <- df_epi_lombok %>% 
  dplyr::count(SPECIMEN_ID) %>% 
  dplyr::filter(SPECIMEN_ID > 1) %>% 
  dplyr::mutate(category = case_when(
    n == 2 ~ "Duplicated",
    n == 3 ~ "Triplicated",
    n == 4 ~ "Quadruplicated",
    n > 4 ~ "More than Quadruplicated"
  )) %>% 
  view()


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

# Detect duplicated IDs
df_epi_sumbawa_duplicated_ids <- df_epi_sumbawa %>% 
  dplyr::count(SPECIMEN_ID) %>% 
  dplyr::filter(SPECIMEN_ID > 1) %>% 
  dplyr::mutate(category = case_when(
    n == 2 ~ "Duplicated",
    n == 3 ~ "Triplicated",
    n == 4 ~ "Quadruplicated",
    n > 4 ~ "More than Quadruplicated"
  )) %>% 
  view()

write.csv(df_epi_sumbawa, "raw_data/temporary_df_epi_sumbawa.csv",
          row.names = F)

setdiff(names(df_epi_lombok), names(df_epi_sumbawa))
setdiff(names(df_epi_sumbawa), names(df_epi_lombok))

# In the end, I manually merge Lombok & Sumbawa dfs (column differences occur)
# Do not trust coded columns.

df_epi_merged <- read.csv("raw_data/temporary_df_epi_lombok_sumbawa_manual_combine_row.csv") %>% 
  dplyr::select(-contains(c("kode_", "Kode_", "koding_","Koding_"))) %>% 
  dplyr::rename_with(~ tolower(gsub("[^[:alnum:]_]", "", .x)))

# Test weird unique value
lapply(df_epi_merged, unique)
df_epi_merged_summarise <- df_epi_merged %>% 
  dplyr::summarise(across(everything(), ~ list(table(.)))) %>% 
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
  view()

# I conducted manual data cleaning for inputted values
# then, pick some interesting columns to be analysed
df_epi_clean <- df_epi_merged %>% 
  dplyr::select(specimen_id, s_pneumoniae_suspect_culture_colony,
                optochin, s_pneumoniae_culture_result, serotype_wgs, # Will be modified to VTs and NVTs
                age_month, # Will be modified soon and classified according to some ageGroups
                area,
                jenis_kelamin, suku,
                apakah_anak_tersebut_pernah_diberi_asi,
                jika_ya_apakah_anak_tersebut_masih_diberi_asi,
                tidak_termasuk_anak_tersebut_berapa_orang_yang_tinggal_1_rumah_dengan_anak_tersebut, # I change "-" to NA
                kecuali_anak_tersebut_berapa_anak_berusia_5_tahun_yang_tinggal_serumah_dengan_anak_tersebut,
                jumlah_anak_berusia_1_tahun_yang_tinggal_serumah, # <1 will change "-", or "tidak" to 0
                jumlah_anak_berusia_antara_1_sampai_dengan_2_tahun_yang_tinggal_serumah, # 1<2, will change "-", or "tidak" to 0
                jumlah_anak_berusia_24_tahun_yang_tinggal_serumah, # 2-4 will change "-", or "tidak" to 0; 2 IDs with "1, 5 tahun" is changed to "1"
                kecuali_anak_tersebut_berapa_anak_yang_berusia_5_tahun_yang_tidur_dalam_1_kamar_dengan_anak_tersebut, # <5 will change "-", or "tidak" to 0
                apakah_anak_pergi_ke_sekolah_taman_kanakkanak_playgroup_pendidikan_anak_usia_dini_ppa_pendidikan_pengembangan_anak_sekolah_minggu_atau_tempat_penitipan_anak_dengan_peserta_lebih_dari_5_orang_anak_lain,
                apakah_anak_tersebut_menghabiskan_setidaknya_1_hari_dalam_seminggu_bergaulberdekatan_dengan_anak_lain_yang_berusia_5_tahun_yang_tidak_tinggal_serumah_dengan_anak_tersebut,
                apakah_di_dalam_rumah_ada_yang_merokok_di_depan_anak,
                
                # pending data clarification
                atap_rumah_terbuat_dari, # is "gaiteng" a typographical error of "genteng" (5) or beton (3)
                # pending data clarification
                
                bangunan_rumah_terbuat_dari,
                tipe_jendela_rumah__tertutup_dengan,
                sumber_bahan_bakar_untuk_memasak,
                dimana_biasanya_anda_memasak,
                apakah_anak_tersebut_pernah_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini,
                berapa_kali_anak_tersebut_dirawat_inap_dalam_3_bulan_terakhir_ini_____kali_dirawat_di_rumah_sakit, # what is "H" and "="? I changed those to "0"
                sakit_di_derita_anak_yang_mengharuskan_anak_di_rawat_inap, # I corrected many variations of "pneumonia" such as "pneuminia" and "pneumoni"
                
                # Specified to sickness
                apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_pneumonia,
                apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_diare,
                
                tidak_termasuk_rawat_inap_di_rumah_sakit_apakah_anak_tersebut_mengunjungi_fasilitas_kesehatan_klinik_rumah_sakit_puskesmas_dalam_3_bulan_terakhir_ini,
                jika_ya_berapa_jika_0_isi_,
                alasan_anak_mengunjungi_fasilitas_kesehatan,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_batuk,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_diare,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_ruam,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_lainlain,
                apakah_anak_sedangpernah_mengalami_demam_dalam_3_hari_terakhir_ini,
                jika_ya_berapa_hari_anak_tersebut_mengalami_demam_jika_tidak_isi_, # review needed
                
                # Sickness in past 24h
                dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami, # review needed
                
                # Antibiotic usage
                apakah_anak_tersebut_pernah_diberi_obat_antibiotik_3_hari_terakhir_ini,
                apakah_anak_tersebut_pernah_diberi_obat_antibiotik_1_bulan_terakhir_ini,
                
                # Vaccination
                sudah_berapa_kali_anak_anda_diberi_vaksin_haemophilus_influenzae_hibpentavalent_dtphbhib,
                sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd
                )
  
  



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

# Use merged data to analyse accepted samples
df_epi_test <- df_epi_merged %>% 
  dplyr::select(2:12, area) %>% 
  dplyr::left_join(read.table("raw_data/test_available_fasta_renamed.txt", header = FALSE) %>% 
                     dplyr::mutate(file_check = "accepted_by_dc",
                                   specimen_id = gsub("Streptococcus_pneumoniae_", "", V1),
                                   specimen_id = gsub(".fasta", "", specimen_id),
                                   fasta_name = V1) %>% 
                     dplyr::select(-V1)
                   ,
                   by = "specimen_id"
  ) %>% 
  view()

area_report <- df_epi_test %>% 
  dplyr::group_by(area, file_check) %>% 
  dplyr::summarise(count = n()) %>% 
  dplyr::ungroup() %>% 
  view()

# further report
area_suspect_report <- df_epi_test %>% 
  dplyr::group_by(area, s_pneumoniae_suspect_culture_colony, s_pneumoniae_culture_result, file_check) %>% 
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
      wgs_result12 == "streptococcus pneumoniae" ~ "positive",
      # optochin == "s" | optochin == "?" ~ "positive",
      optochin == "?" ~ "positive",
      optochin == "r" ~ "negative", # correct notes result based on optochin & culture result
      s_pneumoniae_culture_result == "neg" ~ "negative",
      s_pneumoniae_suspect_culture_colony == "yes" & s_pneumoniae_culture_result == "pos" & wgs_result12 == "streptococcus pneumoniae" ~ "positive",
      s_pneumoniae_suspect_culture_colony == "yes" & s_pneumoniae_culture_result == "pos" & wgs_result12 != "streptococcus pneumoniae" ~ "negative",  # OR "Failed_to_be_extracted"?
      s_pneumoniae_suspect_culture_colony == "yes" & wgs_result12 != "streptococcus pneumoniae" ~ "negative",  # OR "Failed_to_be_extracted"?
      TRUE ~ s_pneumoniae_culture_result
    ),
    final_pneumo_decision = case_when(
      final_pneumo_decision == "neg" ~ "negative",
      is.na(final_pneumo_decision) ~ "negative",
      TRUE ~ final_pneumo_decision  # Ensure other values remain unchanged
    )
  ) %>% 
  dplyr::mutate(
    final_pneumo_decision = case_when(
      final_pneumo_decision == "pos" ~ "positive",
      TRUE ~ final_pneumo_decision
  )) %>% 
  dplyr::select(-wgs_shipment_date) %>% 
  view()

# Isolate non-pneumo data for fasta removal
non_pneumo_fasta <- df_epi_test %>% 
  dplyr::filter(file_check == "accepted_by_dc") %>% 
  dplyr::filter(wgs_result12 != "streptococcus pneumoniae" | is.na(wgs_result12)) %>% 
  dplyr::mutate(fasta_name = paste0("/home/ron/pneumo_2025_lesserSundas/raw_data/fasta/", fasta_name)) %>% 
  dplyr::select(fasta_name) %>% 
  view()

write.table(non_pneumo_fasta, "raw_data/test_non_pneumo_fasta.txt",
            row.names = F, col.names = F, quote = F)

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
