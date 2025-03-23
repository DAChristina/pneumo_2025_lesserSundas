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

# In the end, I manually merge Lombok & Sumbawa dfs
# Column differences occur with various values including shifted columns
# Do not trust coded columns & the "how many vaccination" columns.
# I can't trust n vaccination columns because there are date columns available;
# I manually corrected n vaccination calculations.

# I manually inspect NA values based on data types (numeric & categorical)
# and many aspects from different columns.
# 2 NAs in "tidak_termasuk_anak_tersebut_berapa_orang_yang_tinggal_1_rumah_dengan_anak_tersebut" is filled with median/mode = 3
# 1 NA in "jika_ya_berapa_jika_0_isi_" (healthcare visit); "0" ommited and filled with median/mode = 1 because child is considered sick (batuk)

# Cleaned data is stored in temporary_df_epi_lombok_sumbawa_manual_combine_row.csv:
df_epi_merged <- read.csv("raw_data/temporary_df_epi_lombok_sumbawa_manual_combine_row.csv") %>% 
  dplyr::select(-contains(c("kode_", "Kode_", "koding_","Koding_"))) %>%
  dplyr::rename_with(~ tolower(gsub("[^[:alnum:]_]", "", .x))) %>% 
  dplyr::mutate(across(everything(), ~ ifelse(. == "ya", "yes", 
                                              ifelse(. == "tidak", "no",
                                                     ifelse(. == "tidak tahu", "unknown", .)))))

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
                optochin, s_pneumoniae_culture_result, wgs_result12,
                serotype_wgs, # will be modified to VTs and NVTs
                age_month, # will be modified soon and classified according to some ageGroups
                area,
                jenis_kelamin, suku, # based on the coded value, samawa == sumbawa (10)
                apakah_anak_tersebut_pernah_diberi_asi,
                jika_ya_apakah_anak_tersebut_masih_diberi_asi,
                tidak_termasuk_anak_tersebut_berapa_orang_yang_tinggal_1_rumah_dengan_anak_tersebut, # I change "-" to NA, then filled with median/mode = 3
                kecuali_anak_tersebut_berapa_anak_berusia_5_tahun_yang_tinggal_serumah_dengan_anak_tersebut,
                jumlah_anak_berusia_1_tahun_yang_tinggal_serumah, # <1 I change "-", or "tidak" to 0
                jumlah_anak_berusia_antara_1_sampai_dengan_2_tahun_yang_tinggal_serumah, # 1<2, I change "-", or "tidak" to 0
                jumlah_anak_berusia_24_tahun_yang_tinggal_serumah, # 2-4 I change "-", or "tidak" to 0; 2 IDs with "1, 5 tahun" is changed to "1"
                kecuali_anak_tersebut_berapa_anak_yang_berusia_5_tahun_yang_tidur_dalam_1_kamar_dengan_anak_tersebut, # <5 I change "-", or "tidak" to 0
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
                berapa_kali_anak_tersebut_dirawat_inap_dalam_3_bulan_terakhir_ini_____kali_dirawat_di_rumah_sakit, # what is "H" and "="? I change those to "0"
                sakit_di_derita_anak_yang_mengharuskan_anak_di_rawat_inap, # I corrected many variations of "pneumonia" such as "pneuminia" and "pneumoni"
                
                # specified to sickness
                apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_pneumonia,
                apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_diare,
                
                tidak_termasuk_rawat_inap_di_rumah_sakit_apakah_anak_tersebut_mengunjungi_fasilitas_kesehatan_klinik_rumah_sakit_puskesmas_dalam_3_bulan_terakhir_ini,
                jika_ya_berapa_jika_0_isi_, # 1 NA filled with median/mode = 1; child is considered sick (batuk)
                alasan_anak_mengunjungi_fasilitas_kesehatan,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_batuk,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_diare,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_ruam,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_lainlain,
                apakah_anak_sedangpernah_mengalami_demam_dalam_3_hari_terakhir_ini,
                jika_ya_berapa_hari_anak_tersebut_mengalami_demam_jika_tidak_isi_, # review needed, too many categorical values; I change "-" as 0
                
                # sickness in past 24h
                dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami, # review needed
                
                # antibiotic usage
                apakah_anak_tersebut_pernah_diberi_obat_antibiotik_3_hari_terakhir_ini,
                apakah_anak_tersebut_pernah_diberi_obat_antibiotik_1_bulan_terakhir_ini,
                
                # vaccination
                sudah_berapa_kali_anak_anda_diberi_vaksin_haemophilus_influenzae_hibpentavalent_dtphbhib, # I can't trust the calculations, I inspect n from dates
                sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd # I can't trust the calculations, I inspect n from dates
                ) %>% 
  dplyr::mutate(
    final_pneumo_decision = case_when(
      wgs_result12 == "streptococcus pneumoniae" ~ "positive",
      # optochin == "s" | optochin == "?" ~ "positive", # "s" can be negative based on WGS result
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
      TRUE ~ final_pneumo_decision  # ensure other values remain unchanged
    )
  ) %>% 
  # correct final_pneumo_decision
  dplyr::mutate(
    final_pneumo_decision = case_when(
      final_pneumo_decision == "pos" ~ "positive",
      TRUE ~ final_pneumo_decision
    )) %>% 
  dplyr::mutate(
    # generate age classification
    age_year = case_when(
      age_month < 13 ~ 1,
      age_month >= 13 & age_month < 25 ~ 2,
      age_month >= 25 & age_month < 37 ~ 3,
      age_month >= 37 & age_month < 49 ~ 4,
      age_month >= 49 & age_month < 61 ~ 5
    ),
    age_year_2groups = case_when(
      age_year == 1 ~ "1 and below",
      age_year >= 2 ~ "more than 1"
    ),
    # generate VTs and NVTs according to PCV13
    serotype_wgs = toupper(serotype_wgs),
    serotype_classification_PCV13 = case_when(
      serotype_wgs %in% c("1", "3", "4", "5", "6A", "6B", "7F", 
                          "9V", "14", "18C", "19A", "19F", "23F") ~ "VT",
      serotype_wgs == "UNTYPABLE" ~ "UNTYPABLE",
      !is.na(serotype_wgs) ~ "NVT",
      TRUE ~ NA_character_
    )
  ) %>% 
  # rename epiData
  dplyr::rename(
    sex = jenis_kelamin,
    tribe = suku,
    workLab_culture_suspect = s_pneumoniae_suspect_culture_colony,
    workLab_culture_result = s_pneumoniae_culture_result,
    workLab_optochin = optochin,
    workWGS_species = wgs_result12,
    workWGS_serotype = serotype_wgs,
    workWGS_serotype_classification_PCV13 = serotype_classification_PCV13,
    breastMilk_given = apakah_anak_tersebut_pernah_diberi_asi,
    breastMilk_still_being_given = jika_ya_apakah_anak_tersebut_masih_diberi_asi,
    nTotal_people = tidak_termasuk_anak_tersebut_berapa_orang_yang_tinggal_1_rumah_dengan_anak_tersebut,
    nTotal_child_5yo_andBelow = kecuali_anak_tersebut_berapa_anak_berusia_5_tahun_yang_tinggal_serumah_dengan_anak_tersebut,
    n_child_1yo_andBelow = jumlah_anak_berusia_1_tahun_yang_tinggal_serumah,
    n_child_1to2yo = jumlah_anak_berusia_antara_1_sampai_dengan_2_tahun_yang_tinggal_serumah,
    n_child_2to4yo = jumlah_anak_berusia_24_tahun_yang_tinggal_serumah,
    nTotal_child_5yo_andBelow_sleep = kecuali_anak_tersebut_berapa_anak_yang_berusia_5_tahun_yang_tidur_dalam_1_kamar_dengan_anak_tersebut,
    contact_kindergarten = apakah_anak_pergi_ke_sekolah_taman_kanakkanak_playgroup_pendidikan_anak_usia_dini_ppa_pendidikan_pengembangan_anak_sekolah_minggu_atau_tempat_penitipan_anak_dengan_peserta_lebih_dari_5_orang_anak_lain,
    contact_otherChildren = apakah_anak_tersebut_menghabiskan_setidaknya_1_hari_dalam_seminggu_bergaulberdekatan_dengan_anak_lain_yang_berusia_5_tahun_yang_tidak_tinggal_serumah_dengan_anak_tersebut,
    contact_cigarettes = apakah_di_dalam_rumah_ada_yang_merokok_di_depan_anak,
    house_roof = atap_rumah_terbuat_dari,
    house_building = bangunan_rumah_terbuat_dari,
    house_window = tipe_jendela_rumah__tertutup_dengan,
    contact_cooking_fuel = sumber_bahan_bakar_untuk_memasak,
    contact_cooking_place = dimana_biasanya_anda_memasak,
    hospitalised_last_3mo = apakah_anak_tersebut_pernah_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini,
    hospitalised_last_3mo_n = berapa_kali_anak_tersebut_dirawat_inap_dalam_3_bulan_terakhir_ini_____kali_dirawat_di_rumah_sakit,
    hospitalised_last_3mo_sickness = sakit_di_derita_anak_yang_mengharuskan_anak_di_rawat_inap,
    hospitalised_last_3mo_sickness_pneumonia = apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_pneumonia,
    hospitalised_last_3mo_sickness_diarrhoea = apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_diare,
    healthcareVisit_last_3mo = tidak_termasuk_rawat_inap_di_rumah_sakit_apakah_anak_tersebut_mengunjungi_fasilitas_kesehatan_klinik_rumah_sakit_puskesmas_dalam_3_bulan_terakhir_ini,
    healthcareVisit_last_3mo_n = jika_ya_berapa_jika_0_isi_,
    healthcareVisit_last_3mo_reason = alasan_anak_mengunjungi_fasilitas_kesehatan,
    healthcareVisit_last_3mo_reason_cough = apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_batuk,
    healthcareVisit_last_3mo_reason_diarrhoea = apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_diare,
    healthcareVisit_last_3mo_reason_rash = apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_ruam,
    healthcareVisit_last_3mo_reason_others = apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_lainlain,
    sickness_past3days_fever = apakah_anak_sedangpernah_mengalami_demam_dalam_3_hari_terakhir_ini,
    sickness_past3days_fever_howManyDays = jika_ya_berapa_hari_anak_tersebut_mengalami_demam_jika_tidak_isi_,
    sickness_past24h = dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami,
    antibiotic_past3days = apakah_anak_tersebut_pernah_diberi_obat_antibiotik_3_hari_terakhir_ini,
    antibiotic_past1mo = apakah_anak_tersebut_pernah_diberi_obat_antibiotik_1_bulan_terakhir_ini,
    vaccination_hibpentavalent_n = sudah_berapa_kali_anak_anda_diberi_vaksin_haemophilus_influenzae_hibpentavalent_dtphbhib,
    vaccination_pcv13_n = sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd
  ) %>% 
  # combine to available fasta
  dplyr::left_join(read.table("raw_data/test_available_fasta_renamed.txt", header = FALSE) %>% 
                     dplyr::mutate(workFasta_check = "Accepted_by_DC",
                                   workFasta_name = gsub(".fasta", "", V1),
                                   specimen_id = gsub("Streptococcus_pneumoniae_", "", workFasta_name),
                                   workFasta_name_with_extension = V1) %>% 
                     dplyr::select(-V1)
                   ,
                   by = "specimen_id"
  )

write.csv(df_epi_clean, "raw_data/temporary_df_epi_lombok_sumbawa_manual_combine_row_cleaned.csv")

# Generate numerically-coded values just to mimic the original survey result
# And modify columns into numeric or factor
df_epi_clean <- read.csv("raw_data/temporary_df_epi_lombok_sumbawa_manual_combine_row_cleaned.csv")

df_epi_coded <- df_epi_clean %>% 
  dplyr::mutate(
    coded_sex = case_when(
      sex == "laki-laki" ~ 1,
      sex == "perempuan" ~ 2,
      TRUE ~ NA_real_
      ),
    coded_tribe = case_when(
      tribe == "sasak" ~ 2,
      tribe == "bali" ~ 3,
      tribe == "sumbawa" ~ 10,
      TRUE ~ NA_real_
    ),
    coded_breastMilk_given = case_when(
      breastMilk_given == "yes" ~ 1,
      breastMilk_given == "no" ~ 2,
      TRUE ~ NA_real_
    ),
    coded_breastMilk_still_being_given = case_when(
      breastMilk_still_being_given == "yes" ~ 1,
      breastMilk_still_being_given == "no" ~ 2,
      TRUE ~ NA_real_
    ),
    coded_contact_kindergarten = case_when(
      contact_kindergarten == "yes" ~ 1,
      contact_kindergarten == "no" ~ 2,
      TRUE ~ NA_real_
    ),
    coded_contact_otherChildren = case_when(
      contact_otherChildren == "yes" ~ 1,
      contact_otherChildren == "no" ~ 2,
      TRUE ~ NA_real_
    ),
    coded_contact_cigarettes = case_when(
      contact_cigarettes == "yes" ~ 1,
      contact_cigarettes == "no" ~ 2,
      TRUE ~ NA_real_
    ),
    coded_contact_cooking_fuel = case_when(
      contact_cooking_fuel == "lpg/gas alam" ~ 1,
      contact_cooking_fuel == "kayu" ~ 2,
      contact_cooking_fuel == "minyak tanah" ~ 3,
      TRUE ~ NA_real_
    ),
    coded_contact_cooking_place = case_when(
      contact_cooking_place == "di dalam rumah" ~ 1,
      contact_cooking_place == "di luar rumah" ~ 2,
      TRUE ~ NA_real_
    ),
    coded_house_roof = case_when(
      house_roof == "seng" ~ 1,
      house_roof %in% c("asbes", "batako") ~ 2,
      house_roof == "beton" ~ 3,
      house_roof == "kayu" ~ 4,
      house_roof %in% c("genteng", "genteng logam") ~ 5,
      house_roof == "spandek" ~ 6,
      house_roof %in% c("jerami", "daun palem", "lain-lain") ~ 7,
      TRUE ~ NA_real_
    ),
    coded_house_building = case_when(
      house_building == "batu bata" ~ 1,
      house_building == "kayu" ~ 2,
      house_building %in% c("bambu", "anyaman bambu") ~ 3,
      house_building == "triplek" ~ 4,
      house_building == "batako" ~ 5,
      house_building == "batu" ~ 6,
      TRUE ~ NA_real_
    ),
    coded_house_window = case_when(
      house_window == "kaca/tirai" ~ 1,
      house_window == "kayu" ~ 2,
      house_window == "bambu" ~ 3,
      house_window == "tidak ada/terbuka" ~ 4,
      TRUE ~ NA_real_
    ),
    coded_hospitalised_last_3mo = case_when(
      hospitalised_last_3mo == "yes" ~ 1,
      hospitalised_last_3mo == "no" ~ 2,
      hospitalised_last_3mo == "unknown" ~ 3,
      TRUE ~ NA_real_
    ),
    coded_healthcareVisit_last_3mo = case_when(
      healthcareVisit_last_3mo == "yes" ~ 1,
      healthcareVisit_last_3mo == "no" ~ 2,
      TRUE ~ NA_real_
    ),
    coded_sickness_past3days_fever = case_when(
      sickness_past3days_fever == "yes" ~ 1,
      sickness_past3days_fever == "no" ~ 2,
      sickness_past3days_fever == "unknown" ~ 3,
      TRUE ~ NA_real_
    ),
    coded_antibiotic_past3days = case_when(
      antibiotic_past3days == "yes" ~ 1,
      antibiotic_past3days == "no" ~ 2,
      antibiotic_past3days == "unknown" ~ 3,
      TRUE ~ NA_real_
    ),
    coded_vaccination_hibpentavalent_n = case_when(
      vaccination_hibpentavalent_n == 4 ~ 1,
      vaccination_hibpentavalent_n == 3 ~ 2,
      vaccination_hibpentavalent_n == 2 ~ 3,
      vaccination_hibpentavalent_n == 1 ~ 4,
      vaccination_hibpentavalent_n == 0 ~ 5,
      TRUE ~ NA_real_
    ),
    coded_vaccination_pcv13_n = case_when(
      vaccination_pcv13_n == 4 ~ 1,
      vaccination_pcv13_n == 3 ~ 2,
      vaccination_pcv13_n == 2 ~ 3,
      vaccination_pcv13_n == 1 ~ 4,
      vaccination_pcv13_n == 0 ~ 5,
      TRUE ~ NA_real_
    ),
    coded_final_pneumo_decision = case_when(
      final_pneumo_decision == "positive" ~ 1,
      final_pneumo_decision == "negative" ~ 2,
      TRUE ~ NA_real_
    ),
    coded_area = case_when(
      area == "lombok" ~ 1,
      area == "sumbawa" ~ 2,
      TRUE ~ NA_real_
    )
  ) %>% 
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

# check columns with NA
cols_with_na <- colnames(df_epi_coded)[colSums(is.na(df_epi_coded)) > 0]
cols_with_na_sums <- df_epi_coded %>%
  dplyr::summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  # transpose
  tidyr::pivot_longer(everything(),
                      names_to = "columns", values_to = "NAs") %>% 
  dplyr::filter(!str_detect(columns, "work"),
                NAs != 0)
view(cols_with_na_sums)

get_mmm <- function(x) {
  mea_v <- mean(x, na.rm = TRUE)
  med_v <- median(x, na.rm = TRUE)
  
  uniq_x <- unique(na.omit(x))
  mod_v <- uniq_x[which.max(tabulate(match(x, uniq_x)))]
  
  return(list(mean = mea_v,
              median = med_v,
              mode = mod_v))
  }

# Crucial columns with NA:
# nTotal_people (1)
get_mmm(df_epi_coded$nTotal_people)
# healthcareVisit_last_3mo_n (2)
filtered_0 <- df_epi_coded %>% 
  dplyr::filter(healthcareVisit_last_3mo_n != 0)
get_mmm(filtered_0$healthcareVisit_last_3mo_n)

write.csv(df_epi_coded, "raw_data/temporary_df_epi_lombok_sumbawa_manual_combine_row_cleaned_coded.csv")


# Test epiFunction ehehe
# try OR report works only for categorical data
df_epi_coded_chars <- read.csv("raw_data/temporary_df_epi_lombok_sumbawa_manual_combine_row_cleaned_coded.csv") %>% 
  dplyr::select(where(~ !all(is.na(.))), # NAs in workLab, workFasta & not interesting columns
                -sickness_past3days_fever_howManyDays, # conflicted values with fever column
                -contains("coded_"),
                -X, -specimen_id)

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
or_logistic_all <- generate_glm_logistic_report(df_input = df_epi_coded_chars,
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
or_multivariable_all <- generate_glm_multivariable_report(df_input = df_epi_coded_chars, binary_disease = "final_pneumo_decision")



# Analyse genomic data
genome <- readxl::read_excel("raw_data/Data WGS_Lombok.xlsx")





# Grouping vaccination test
df_epi_clean_grouped <- df_epi_clean %>% 
  group_by(sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd, age_year_2groups, final_pneumo_decision, serotype_classification_PCV13) %>% 
  summarise(count = n()) %>% 
  view() %>% 
  glimpse()

# Selecting final pneumo decision
df_epi_clean_pneumo_decision <- df_epi_clean %>% 
  select(s_pneumoniae_suspect_culture_colony, optochin,
         s_pneumoniae_culture_result, wgs_result12,
         final_pneumo_decision) %>% 
  view()
  
# Demo viz
df_epi_clean_grouped <- df_epi_clean %>% 
  group_by(sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd, final_pneumo_decision) %>% 
  summarise(count = n()) %>% 
  # view() %>% 
  glimpse()

ggplot(df_epi_clean_grouped, 
       aes_string(y = 'count', x = "sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd",
                  fill = 'final_pneumo_decision')) + 
  geom_bar(position = 'fill', stat = 'identity') + # stack or fill
  labs(title = paste("Stacked Barplot of")) + 
  # theme(legend.position="none") +
  # geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  theme(legend.position="none")





# Trial visualisations (boxplot of counts and percentage) simple y = final_pneumo_decision x = columns
# df_epi_clean$final_pneumo_decision <- factor(df_epi_clean$final_pneumo_decision, levels = c('negative', 'positive'))





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
  # correct final_pneumo_decision
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
# notes: Streptococcus_pneumoniae_SWQ_049.fasta is not denoted as pneumo in column wgs_result12,
# I re-analyse the file in pathogenWatch & add the fasta as pneumo in temporary_df_epi_lombok_sumbawa_manual_combine_row.csv


# Quick viz serotypes
# Compute percentage
df_serotype_summary <- df_epi_clean %>% 
  dplyr::filter(!is.na(serotype_wgs)) %>% 
  dplyr::count(serotype_wgs) %>%
  dplyr::mutate(Percentage = n / sum(n) * 100)

ggplot(df_serotype_summary, aes(x = serotype_wgs, y = Percentage,
                                fill = serotype_wgs)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Category", y = "Percentage", 
       title = "All Serotypes") +
  theme_bw() +
  theme(legend.position = "none")

# Compute percentage by area
df_serotype_area_summary <- df_epi_clean %>% 
  dplyr::filter(!is.na(serotype_wgs)) %>% 
  dplyr::group_by(area, serotype_classification_PCV13, serotype_wgs) %>%
  dplyr::summarise(Count = n(), .groups = "drop") %>%
  dplyr::mutate(Percentage = Count / sum(Count) * 100,
                serotype_classification_PCV13 = factor(serotype_classification_PCV13,
                                                       levels = c("UNTYPABLE",
                                                                  "VT", "NVT")))

ggplot(df_serotype_area_summary, aes(x = serotype_wgs, y = Percentage,
                                     fill = area)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Category", y = "Percentage", 
       title = "All Serotypes") +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~ serotype_classification_PCV13, nrow = 3)

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
