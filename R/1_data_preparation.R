library(tidyverse)

# Data cleaning process for epiData ############################################
df_epi_lombok <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver4.xlsx",
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

df_epi_sumbawa <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver4.xlsx",
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
# 1 NA in "jika_ya_berapa_jika_0_isi_" (healthcare visit); "0" ommited and filled with median/mode = 1 because child is considered ill (batuk)

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
                optochin, s_pneumoniae_culture_result, wgs_result11, wgs_result12,
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
                
                atap_rumah_terbuat_dari, # is "gaiteng" a typographical error of "genteng" (5) or beton (3)
                bangunan_rumah_terbuat_dari,
                tipe_jendela_rumah__tertutup_dengan,
                sumber_bahan_bakar_untuk_memasak,
                dimana_biasanya_anda_memasak,
                apakah_anak_tersebut_pernah_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini,
                berapa_kali_anak_tersebut_dirawat_inap_dalam_3_bulan_terakhir_ini_____kali_dirawat_di_rumah_sakit, # what is "H" and "="? I change those to "0"
                sakit_di_derita_anak_yang_mengharuskan_anak_di_rawat_inap, # I corrected many variations of "pneumonia" such as "pneuminia" and "pneumoni"
                
                # specified to illness
                apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_pneumonia,
                apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_diare,
                
                tidak_termasuk_rawat_inap_di_rumah_sakit_apakah_anak_tersebut_mengunjungi_fasilitas_kesehatan_klinik_rumah_sakit_puskesmas_dalam_3_bulan_terakhir_ini,
                jika_ya_berapa_jika_0_isi_, # 1 NA filled with median/mode = 1; child is considered ill (batuk)
                alasan_anak_mengunjungi_fasilitas_kesehatan,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_batuk,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_diare,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_ruam,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_lainlain,
                apakah_anak_sedangpernah_mengalami_demam_dalam_3_hari_terakhir_ini,
                jika_ya_berapa_hari_anak_tersebut_mengalami_demam_jika_tidak_isi_, # review needed, too many categorical values; I change "-" as 0
                
                # illness in past 24h
                # dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami, # review needed
                dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami_batuk,
                dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami_hidung_ingusan,
                dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami_kesulitan_bernapas,
                
                # antibiotic usage
                apakah_anak_tersebut_pernah_diberi_obat_antibiotik_3_hari_terakhir_ini,
                apakah_anak_tersebut_pernah_diberi_obat_antibiotik_1_bulan_terakhir_ini,
                
                # vaccination
                sudah_berapa_kali_anak_anda_diberi_vaksin_haemophilus_influenzae_hibpentavalent_dtphbhib, # I can't trust the calculations, I inspect n from dates
                sudah_berapa_kali_anak_anda_diberi_vaksin_haemophilus_influenzae_hibpentavalent_dtphbhib_dc,
                sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd, # I can't trust the calculations, I inspect n from dates
                sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd_dc
                ) %>% 
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
      age_month < 13 ~ "1 and below",
      age_month >= 13 ~ "more than 1"
    ),
    age_year_3groups = case_when(
      age_month < 13 ~ "1 and below",
      age_month >= 13 & age_month < 25 ~ "1-2",
      age_month >= 25 & age_month < 61 ~ "3-5",
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
    workWGS_success_failed = wgs_result11,
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
    hospitalised_last_3mo_illness = sakit_di_derita_anak_yang_mengharuskan_anak_di_rawat_inap,
    hospitalised_last_3mo_illness_pneumonia = apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_pneumonia,
    hospitalised_last_3mo_illness_diarrhoea = apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_diare,
    healthcareVisit_last_3mo = tidak_termasuk_rawat_inap_di_rumah_sakit_apakah_anak_tersebut_mengunjungi_fasilitas_kesehatan_klinik_rumah_sakit_puskesmas_dalam_3_bulan_terakhir_ini,
    healthcareVisit_last_3mo_n = jika_ya_berapa_jika_0_isi_,
    healthcareVisit_last_3mo_reason = alasan_anak_mengunjungi_fasilitas_kesehatan,
    healthcareVisit_last_3mo_reason_cough = apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_batuk,
    healthcareVisit_last_3mo_reason_diarrhoea = apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_diare,
    healthcareVisit_last_3mo_reason_rash = apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_ruam,
    healthcareVisit_last_3mo_reason_others = apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_lainlain,
    illness_past3days_fever = apakah_anak_sedangpernah_mengalami_demam_dalam_3_hari_terakhir_ini,
    illness_past3days_fever_nDays = jika_ya_berapa_hari_anak_tersebut_mengalami_demam_jika_tidak_isi_,
    # illness_past24h = dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami,
    illness_past24h_cough = dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami_batuk,
    illness_past24h_runny_nose = dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami_hidung_ingusan,
    illness_past24h_difficulty_breathing = dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami_kesulitan_bernapas,
    antibiotic_past3days = apakah_anak_tersebut_pernah_diberi_obat_antibiotik_3_hari_terakhir_ini,
    antibiotic_past1mo = apakah_anak_tersebut_pernah_diberi_obat_antibiotik_1_bulan_terakhir_ini,
    vaccination_hibpentavalent_n = sudah_berapa_kali_anak_anda_diberi_vaksin_haemophilus_influenzae_hibpentavalent_dtphbhib,
    vaccination_hibpentavalent_dc_n = sudah_berapa_kali_anak_anda_diberi_vaksin_haemophilus_influenzae_hibpentavalent_dtphbhib_dc,
    vaccination_pcv13_n = sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd,
    vaccination_pcv13_dc_n = sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd_dc
    
  ) %>% 
  # generate re-grouping imbalanced columns
  dplyr::mutate(
    breastFeed_compiled = case_when(
      breastMilk_given == "yes" & breastMilk_still_being_given == "yes" ~ "currently breastfeed",
      breastMilk_given == "no" & breastMilk_still_being_given == "yes" ~ "currently breastfeed",
      breastMilk_given == "yes" & breastMilk_still_being_given == "no" ~ "ever breastfeed",
      breastMilk_given == "no" & breastMilk_still_being_given == "no" ~ "never breastfeed"
    ),
    house_roof_regroup = case_when(
      house_roof %in% c("batako", "beton", "genteng logam") ~ "others",
      TRUE ~ house_roof
    ),
    house_building_regroup = case_when(
      house_building %in% c("batu", "batu bata") ~ "batu bata",
      house_building %in% c("anyaman bambu", "bambu", "kayu", "triplek") ~ "bambu/triplek",
      TRUE ~ house_building
    ),
    house_window_regroup = case_when(
      house_window %in% c("bambu", "kayu", "tidak ada/terbuka") ~ "bambu/kayu/terbuka",
      TRUE ~ house_window
    ),
    nTotal_people_regroup = case_when(
      is.na(nTotal_people) | nTotal_people < 4 ~ "1-3 (low)",
      nTotal_people >= 4 & nTotal_people < 7 ~ "4-6 (moderate)",
      nTotal_people >= 7 ~ ">6 (high)"
    ),
    nTotal_child_5yo_andBelow_regroup = case_when(
      nTotal_child_5yo_andBelow == 0 ~ "0",
      nTotal_child_5yo_andBelow >= 0 ~ "1-4"
    ),
    nTotal_child_5yo_andBelow_sleep_regroup = case_when(
      nTotal_child_5yo_andBelow_sleep == 0 ~ "0",
      nTotal_child_5yo_andBelow_sleep >= 0 ~ "1-3"
    ),
    illness_past3days_fever_regroup = case_when(
      illness_past3days_fever == "unknown" ~ "no",
      TRUE ~ illness_past3days_fever
    ),
    illness_past3days_fever_nDays_regroup = case_when(
      illness_past3days_fever_nDays == "no" ~ "0",
      illness_past3days_fever_nDays == "yes" ~ "2", # mode
      TRUE ~ illness_past3days_fever_nDays
    ),
    illness_past24h_cough = case_when(
      is.na(illness_past24h_cough) ~ "no",
      TRUE ~ illness_past24h_cough
    ),
    illness_past24h_runny_nose = case_when(
      is.na(illness_past24h_runny_nose) ~ "no",
      TRUE ~ illness_past24h_runny_nose
    ),
    illness_past24h_difficulty_breathing = case_when(
      is.na(illness_past24h_difficulty_breathing) ~ "no",
      TRUE ~ illness_past24h_difficulty_breathing
    ),
    illness_past24h_difficulty_compiled = case_when(
      illness_past24h_cough == "no" & illness_past24h_runny_nose == "no" & illness_past24h_difficulty_breathing == "no" ~ "no",
      TRUE ~ "≥ 1 respiratory illness",
    ),
    vaccination_hibpentavalent_dc_n_regroup = case_when(
      vaccination_hibpentavalent_dc_n < 4 ~ "1-3 mandatory",
      vaccination_hibpentavalent_dc_n >= 4 ~ "4 booster"
    ),
    vaccination_pcv13_dc_n_regroup = case_when(
      vaccination_pcv13_dc_n < 3 ~ "1-2 mandatory",
      vaccination_pcv13_dc_n >= 3 ~ "3-4 booster"
    )
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


column_names <- setdiff(names(df_epi_clean), "workLab_culture_result")
for (column in column_names){
  df_summary <- df_epi_clean %>% 
    dplyr::group_by(!!sym(column), workLab_culture_result) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>% 
    glimpse()
}

write.csv(df_epi_clean, "raw_data/temporary_df_epi_lombok_sumbawa_manual_combine_row_cleaned.csv", row.names = F)

# Generate numerically-coded values just to mimic the original survey result
# And modify columns into numeric or factor
df_epi_clean <- read.csv("raw_data/temporary_df_epi_lombok_sumbawa_manual_combine_row_cleaned.csv")

df_epi_coded <- df_epi_clean %>% 
  # dplyr::mutate(
  #   coded_sex = case_when(
  #     sex == "laki-laki" ~ 1,
  #     sex == "perempuan" ~ 2,
  #     TRUE ~ NA_real_
  #     ),
  #   coded_tribe = case_when(
  #     tribe == "sasak" ~ 2,
  #     tribe == "bali" ~ 3,
  #     tribe == "sumbawa" ~ 10,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_breastMilk_given = case_when(
  #     breastMilk_given == "yes" ~ 1,
  #     breastMilk_given == "no" ~ 2,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_breastMilk_still_being_given = case_when(
  #     breastMilk_still_being_given == "yes" ~ 1,
  #     breastMilk_still_being_given == "no" ~ 2,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_contact_kindergarten = case_when(
  #     contact_kindergarten == "yes" ~ 1,
  #     contact_kindergarten == "no" ~ 2,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_contact_otherChildren = case_when(
  #     contact_otherChildren == "yes" ~ 1,
  #     contact_otherChildren == "no" ~ 2,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_contact_cigarettes = case_when(
  #     contact_cigarettes == "yes" ~ 1,
  #     contact_cigarettes == "no" ~ 2,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_contact_cooking_fuel = case_when(
  #     contact_cooking_fuel == "lpg/gas alam" ~ 1,
  #     contact_cooking_fuel == "kayu" ~ 2,
  #     contact_cooking_fuel == "minyak tanah" ~ 3,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_contact_cooking_place = case_when(
  #     contact_cooking_place == "di dalam rumah" ~ 1,
  #     contact_cooking_place == "di luar rumah" ~ 2,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_house_roof = case_when(
  #     house_roof == "seng" ~ 1,
  #     house_roof %in% c("asbes", "batako") ~ 2,
  #     house_roof == "beton" ~ 3,
  #     house_roof == "kayu" ~ 4,
  #     house_roof %in% c("genteng", "genteng logam") ~ 5,
  #     house_roof == "spandek" ~ 6,
  #     house_roof %in% c("jerami", "daun palem", "lain-lain") ~ 7,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_house_building = case_when(
  #     house_building == "batu bata" ~ 1,
  #     house_building == "kayu" ~ 2,
  #     house_building %in% c("bambu", "anyaman bambu") ~ 3,
  #     house_building == "triplek" ~ 4,
  #     house_building == "batako" ~ 5,
  #     house_building == "batu" ~ 6,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_house_window = case_when(
  #     house_window == "kaca/tirai" ~ 1,
  #     house_window == "kayu" ~ 2,
  #     house_window == "bambu" ~ 3,
  #     house_window == "tidak ada/terbuka" ~ 4,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_hospitalised_last_3mo = case_when(
  #     hospitalised_last_3mo == "yes" ~ 1,
  #     hospitalised_last_3mo == "no" ~ 2,
  #     hospitalised_last_3mo == "unknown" ~ 3,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_healthcareVisit_last_3mo = case_when(
  #     healthcareVisit_last_3mo == "yes" ~ 1,
  #     healthcareVisit_last_3mo == "no" ~ 2,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_illness_past3days_fever = case_when(
  #     illness_past3days_fever == "yes" ~ 1,
  #     illness_past3days_fever == "no" ~ 2,
  #     illness_past3days_fever == "unknown" ~ 3,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_antibiotic_past3days = case_when(
  #     antibiotic_past3days == "yes" ~ 1,
  #     antibiotic_past3days == "no" ~ 2,
  #     antibiotic_past3days == "unknown" ~ 3,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_vaccination_hibpentavalent_n = case_when(
  #     vaccination_hibpentavalent_n == 4 ~ 1,
  #     vaccination_hibpentavalent_n == 3 ~ 2,
  #     vaccination_hibpentavalent_n == 2 ~ 3,
  #     vaccination_hibpentavalent_n == 1 ~ 4,
  #     vaccination_hibpentavalent_n == 0 ~ 5,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_vaccination_pcv13_n = case_when(
  #     vaccination_pcv13_n == 4 ~ 1,
  #     vaccination_pcv13_n == 3 ~ 2,
  #     vaccination_pcv13_n == 2 ~ 3,
  #     vaccination_pcv13_n == 1 ~ 4,
  #     vaccination_pcv13_n == 0 ~ 5,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_final_pneumo_decision = case_when(
  #     final_pneumo_decision == "positive" ~ 1,
  #     final_pneumo_decision == "negative" ~ 2,
  #     TRUE ~ NA_real_
  #   ),
  #   coded_area = case_when(
  #     area == "lombok" ~ 1,
  #     area == "sumbawa" ~ 2,
  #     TRUE ~ NA_real_
  #   )
  # ) %>% 
  # conduct corrections for supposedly NUMERICAL and FACTOR (not ordered) columns!
  dplyr::select(-contains("work"))

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

write.csv(df_epi_coded, "inputs/epiData.csv", row.names = F)


# Data cleaning process for workLab ############################################
# Generate available fasta list first!

df_workLab <- dplyr::bind_rows(
  readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver4.xlsx",
                     sheet = "Lombok") %>% 
    dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
    dplyr::rename_with(~ tolower(gsub("[^[:alnum:]_]", "", .x))) %>% 
    dplyr::mutate(specimen_id = gsub(" ", "_", specimen_id),
                  area = "Lombok",
                  across(where(is.character), ~ na_if(., "N/A")),
                  across(where(is.character), ~ if_else(. == "-", "tidak", .))) %>% 
    dplyr::select(2:13, area)
  ,
  readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver4.xlsx",
                     sheet = "Sumbawa") %>% 
    dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
    dplyr::rename_with(~ tolower(gsub("[^[:alnum:]_]", "", .x))) %>% 
    dplyr::mutate(specimen_id = gsub("-", "_", specimen_id),
                  area = "Sumbawa",
                  across(where(is.character), ~ na_if(., "N/A")),
                  across(where(is.character), ~ if_else(. == "-", "tidak", .))) %>% 
    dplyr::select(2:13, area)
) %>% 
  dplyr::mutate_all(tolower) %>% 
  dplyr::mutate(specimen_id = toupper(specimen_id)) %>% 
  # rename to simplify my life
  dplyr::rename(
    workLab_culture_suspect = s_pneumoniae_suspect_culture_colony,
    workLab_culture_result = s_pneumoniae_culture_result,
    workLab_culture_notes = notes,
    workLab_optochin = optochin,
    workWGS_success_failed = wgs_result11,
    workWGS_species = wgs_result12, # data can't be trusted; use WGS list instead
    workWGS_serotype = serotype_wgs # data can't be trusted; use WGS list instead
  ) %>% 
  # mutate all "N/A" values
  dplyr::mutate(across(where(is.character), ~ na_if(., "N/A"))) %>% 
  # generate final workLab decision based on re-cultivation and other previous workLabs
  dplyr::mutate(
    workLab_culture_result_final = case_when(
      workLab_culture_notes %in% c("no growth") ~ "neg",
      grepl("white", workLab_culture_notes) ~ "neg",
      TRUE ~ workLab_culture_result
    )
  ) %>% 
  dplyr::left_join(read.table("raw_data/test_available_fasta_renamed.txt", header = FALSE) %>% 
                     dplyr::mutate(workFasta_file_check = "Accepted_by_DC",
                                   specimen_id = gsub("Streptococcus_pneumoniae_", "", V1),
                                   specimen_id = gsub(".fasta", "", specimen_id),
                                   workFasta_name_with_extension = V1) %>% 
                     dplyr::select(-V1)
                   ,
                   by = "specimen_id"
  ) %>% 
  dplyr::select(-workWGS_species, -workWGS_serotype,
                -kode_1_positif_2_negatif_3_not_yet) %>% 
  # view() %>%
  glimpse()

write.csv(df_workLab, "inputs/workLab_data.csv", row.names = F)

# Generate report for fasta files not accepted by DC (yet)
# fasta_missing_report <- df_workLab %>% 
#   dplyr::filter(workWGS_species == "Streptococcus pneumoniae" & is.na(workFasta_file_check)) %>% 
#   view() %>% 
#   glimpse()
# 
# write.csv(fasta_missing_report, "report/temporary_missing_fasta_list_in_DC.csv")
# write.table(fasta_missing_report %>% 
#               dplyr::select(specimen_id),
#             "report/temporary_missing_fasta_list_in_DC.txt",
#             row.names = F, quote = F)

# Data cleaning process for genData ############################################
# Apparently "No Isolat" is based on the first line inside <cat *.fasta>.
# I extract "No Isolat" on terminal; see 0_temporary_script

# BE CAREFUL while inspecting Data WGS_Lombok.xlsx;
# 49 "No Isolat" with joined <location ID> & <participat ID>
# I manually edit these inconsistencies

df_gen_all <- dplyr::left_join(
  dplyr::bind_rows(
    read.csv("raw_data/test_fasta_headers.csv", header = F)
    ,
    data.frame(
      V1 = "Streptococcus_pneumoniae_LBK_137.fasta",
      V2 = "contigs_available_but_not_fasta",
      stringsAsFactors = FALSE
    )
  ) %>% 
    stats::setNames(c("workFasta_name_with_extension", "workFasta_genome_name")) %>% 
    dplyr::mutate(workFasta_name = gsub("\\.fasta$", "", workFasta_name_with_extension))
  ,  
  # I manually edit 49 naming inconsistencies and analyse LBK_137 from contigs
  readxl::read_excel("raw_data/Data WGS_Lombok_ver4.xlsx") %>% 
    dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
    dplyr::rename_with(~ tolower(gsub("[^[:alnum:]_]", "", .x))) %>% 
    dplyr::rename_all(~ paste0("workWGS_", .)) %>% 
    dplyr::rename(
      workWGS_species_pw = workWGS_organism_name,
      workWGS_MLST_pw_ST = workWGS_sequence_type,
      workWGS_MLST_pw_aroe = workWGS_aroe,
      workWGS_MLST_pw_gdh = workWGS_gdh,
      workWGS_MLST_pw_gki = workWGS_gki,
      workWGS_MLST_pw_recp = workWGS_recp,
      workWGS_MLST_pw_spi = workWGS_spi,
      workWGS_MLST_pw_xpt = workWGS_xpt,
      workWGS_MLST_pw_ddl = workWGS_ddl,
      
      workWGS_AMR_pbp1a = workWGS_pbp1a,
      workWGS_AMR_pbp2b = workWGS_pbp2b,
      workWGS_AMR_pbp2x = workWGS_pbp2x,
      workWGS_AMR_chloramphenicol = workWGS_chloramphenicol,
      workWGS_AMR_clindamycin = workWGS_clindamycin,
      workWGS_AMR_erythromycin = workWGS_erythromycin,
      workWGS_AMR_fluoroquinolones = workWGS_fluoroquinolones,
      workWGS_AMR_kanamycin = workWGS_kanamycin,
      workWGS_AMR_linezolid = workWGS_linezolid,
      workWGS_AMR_tetracycline = workWGS_tetracycline,
      workWGS_AMR_trimethoprim = workWGS_trimethoprim,
      workWGS_AMR_sulfamethoxazole = workWGS_sulfamethoxazole,
      workWGS_AMR_cotrimoxazole = workWGS_cotrimoxazole,
      workWGS_AMR_amoxicillin = workWGS_amoxicillin,
      workWGS_AMR_ceftriaxone = workWGS_ceftriaxone,
      workWGS_AMR_cefotaxime = workWGS_cefotaxime,
      workWGS_AMR_cefuroxime = workWGS_cefuroxime,
      workWGS_AMR_meropenem = workWGS_meropenem,
      workWGS_AMR_penicillin = workWGS_penicillin
    ) %>% 
    dplyr::mutate(
      workWGS_serotype_regroup = case_when(
        workWGS_serotype == "03" ~ "3",
        workWGS_serotype == "06A" ~ "6A",
        workWGS_serotype == "06B" ~ "6B",
        workWGS_serotype == "06C" ~ "6C",
        workWGS_serotype == "6E(6B)" ~ "serogroup 6",
        workWGS_serotype == "07C" ~ "7C",
        workWGS_serotype %in% c("alternative_aliB_NT", "untypable", "Untypable") ~ "untypeable",
        TRUE ~ workWGS_serotype
      )
    ) %>%
    # annoying inconsistencies AMR values
    dplyr::mutate(
      across(
        .cols = contains("_AMR_"),
        # .fns = ~ str_replace_all(tolower(.x), " ", ""),
        .fns = ~ str_replace_all(tolower(.x), "[_() ]", "")
      )
    ) #%>% 
    # dplyr::mutate(
    #   across(
    #     .cols = contains("AMR"),
    #     .fns = ~ case_when(
    #       str_detect(tolower(.x), "sensitive") ~ "S",
    #       str_detect(tolower(.x), "resistant") ~ "R",
    #       str_detect(tolower(.x), "intermediate") ~ "I",
    #       str_detect(tolower(.x), "nf") ~ "NF",
    #       TRUE ~ as.character(.x)
    #     ))
    # )
  ,
  join_by("workFasta_name" == "workWGS_dc_id")
  ) %>% 
  # contigs
  dplyr::left_join(
    read.table("raw_data/qfile_poppunk.txt", header = F) %>% 
      stats::setNames(c("workFasta_name", "location")) %>% 
      dplyr::mutate(contigs_availability = "contigs_available") %>% 
      dplyr::select(-location)
    ,
    by = "workFasta_name"
  ) %>% 
  # generate specimen_id
  dplyr::mutate(specimen_id = gsub("Streptococcus_pneumoniae_", "", workFasta_name)) %>% 
  dplyr::left_join(
    df_workLab %>% 
      dplyr::select(workFasta_name_with_extension, workWGS_success_failed,
                    workFasta_file_check)
    ,
    by = "workFasta_name_with_extension"
  ) %>% 
  dplyr::left_join(
    read.csv("raw_data/result_assembly_stats/compiled_assembly_stats.csv") %>% 
      dplyr::rename_all(~ paste0("workWGS_stats_", .)) %>% 
      dplyr::mutate(workWGS_stats_pneumo_cutoff = case_when(
        workWGS_stats_sum >= 1900000 & workWGS_stats_sum <= 2300000 ~ "predicted pure pneumo",
        workWGS_stats_sum < 1900000 ~ "< 1.9 Mb",
        workWGS_stats_sum > 2300000 ~ "> 2.3 Mb",
      ))
    ,
    join_by("workFasta_name_with_extension" == "workWGS_stats_my_ID")
  ) %>% 
  dplyr::left_join(
    read.table("raw_data/result_blast_refgenes/blastn_tabular_lytA.txt", head = F) %>% 
      dplyr::rename_with(~ c("file_name", "qseqid", "sseqid", "pident", "length", "mismatch",
                             "gapopen", "qstart", "qend", "sstart", "send",
                             "evalue", "bitscore")) %>% 
      dplyr::rename_all(~ paste0("workBLAST_lytA_", .)) %>% 
      dplyr::mutate(workFasta_name = gsub("_lytA_blastn_tabular.txt", "", workBLAST_lytA_file_name),
                    workBLAST_lytA_predicted_species = case_when(
                      workBLAST_lytA_pident >= 70 ~ "predicted pure pneumo",
                      TRUE ~ "not pneumococcus"
                    )) %>% 
      dplyr::select(-workBLAST_lytA_file_name, -workBLAST_lytA_qseqid, -workBLAST_lytA_sseqid)
    ,
    by = "workFasta_name"
  ) %>% 
  dplyr::left_join(
    read.csv("raw_data/result_mlst/mlst_results.csv", header = F, sep = "\t") %>% 
      stats::setNames(c("workFasta_name_with_extension", "workWGS_MLST_dc_species", "workWGS_MLST_dc_ST",
                        "workWGS_MLST_dc_aroe", "workWGS_MLST_dc_gdh", "workWGS_MLST_dc_gki",
                        "workWGS_MLST_dc_recp", "workWGS_MLST_dc_spi", "workWGS_MLST_dc_xpt", "workWGS_MLST_dc_ddl")) %>% 
      dplyr::mutate(workFasta_name_with_extension = gsub("/home/ron/pneumo_2025_lesserSundas/raw_data/fasta_compiled_all/", "", workFasta_name_with_extension),
                    workWGS_MLST_dc_aroe = gsub("aroE\\(", "", workWGS_MLST_dc_aroe) %>% gsub("\\)$", "", .),
                    workWGS_MLST_dc_gdh = gsub("gdh\\(", "", workWGS_MLST_dc_gdh) %>% gsub("\\)$", "", .),
                    workWGS_MLST_dc_gki = gsub("gki\\(", "", workWGS_MLST_dc_gki) %>% gsub("\\)$", "", .),
                    workWGS_MLST_dc_recp = gsub("recP\\(", "", workWGS_MLST_dc_recp) %>% gsub("\\)$", "", .),
                    workWGS_MLST_dc_spi = gsub("spi\\(", "", workWGS_MLST_dc_spi) %>% gsub("\\)$", "", .),
                    workWGS_MLST_dc_xpt = gsub("xpt\\(", "", workWGS_MLST_dc_xpt) %>% gsub("\\)$", "", .),
                    workWGS_MLST_dc_ddl = gsub("ddl\\(", "", workWGS_MLST_dc_ddl) %>% gsub("\\)$", "", .)
      )
    ,
    by = "workFasta_name_with_extension"
  ) %>% 
  dplyr::left_join(
    read.csv("raw_data/result_pneumokity/Collated_result_data.csv") %>% 
      dplyr::rename_all(~ paste0("workWGS_kity_", .)) %>% 
      dplyr::mutate(
        workWGS_kity_predicted_serotype = gsub("_", "/", workWGS_kity_predicted_serotype),
        workWGS_kity_predicted_serotype_regroup = case_when(
          grepl("Below",  workWGS_kity_predicted_serotype) ~ "untypeable",
          grepl("Mixed",  workWGS_kity_predicted_serotype) ~ "mixed serotypes/serogroups",
          workWGS_kity_predicted_serotype == "Serogroup/6/(6E)" ~ "serogroup 6 (6B/6E)",
          TRUE ~ workWGS_kity_predicted_serotype
        )
      )
    ,
    join_by("workFasta_name" == "workWGS_kity_sampleid")
  ) %>% 
  dplyr::mutate(
    serotype_final_decision = case_when(
      is.na(workWGS_serotype_regroup) ~ workWGS_kity_predicted_serotype_regroup,
      TRUE ~ workWGS_serotype_regroup
      ),
    serotype_final_decision_DC_notes = case_when(
      workWGS_serotype_regroup == workWGS_kity_predicted_serotype_regroup | str_detect(workWGS_kity_predicted_serotype_regroup, fixed(workWGS_serotype_regroup)) ~ "trust pw",
      workWGS_serotype_regroup != workWGS_kity_predicted_serotype_regroup | is.na(workWGS_serotype_regroup) ~ "pw cannot be trusted, use pneumoKITy's result"
    ),
    # solely check serotype_final_decision
    serotype_final_decision = case_when(
      serotype_final_decision == "35A/35C/42" ~ "35C",
      serotype_final_decision == "serogroup 6" ~ "6B",
      serotype_final_decision == "serogroup 24" ~ "24F",
      serotype_final_decision == "10X" ~ "untypeable",
      serotype_final_decision == "15B/15C" ~ "15C",
      TRUE ~ serotype_final_decision
    )
  ) %>% 
  glimpse()

write.csv(df_gen_all, "inputs/genData_all.csv", row.names = F)


# test serotype compile
test_serotype_compile <- df_gen_all %>% 
  dplyr::select(specimen_id,
                contigs_availability,
                contains(c("serotype", "kity")),
                -workWGS_serotype,
                -workWGS_kity_predicted_serotype) %>% 
  # dplyr::filter(serotype_final_decision_DC_notes != "trust pw") %>% 
  view()

# test cleaned AMR
amr <- df_gen_all %>% 
  dplyr::select(contains("_AMR_")) %>% 
  glimpse()

for (col in colnames(amr)) {
  cat("\nTabs: ", col, "\n")
  # print(table(amr[[col]], useNA = "always"))
  print(unique(amr[[col]]))
}

# Species decision based on workWGS_stats_pneumo_cutoff = "predicted pure pneumo";
# NOT pneumo species from pathogenWatch OR MLST!
df_gen_all_sp_comparison <- df_gen_all %>% 
  dplyr::select(workFasta_name_with_extension,
                workFasta_file_check,
                contigs_availability,
                workWGS_success_failed,
                workWGS_species_pw, workWGS_MLST_dc_species,
                workWGS_stats_sum, workWGS_stats_ave, workWGS_stats_N50,
                workWGS_stats_pneumo_cutoff, workBLAST_lytA_predicted_species,
                workWGS_kity_top_hits, workWGS_kity_rag_status) %>% 
  view()


# Isolate non-pneumo fasta files & pneumo fasta files
fasta_non_pneumo <- df_gen_all %>% 
  dplyr::filter(workWGS_stats_pneumo_cutoff != "predicted pure pneumo") %>% 
  # dplyr::mutate(fasta_name = paste0("/home/ron/pneumo_2025_lesserSundas/raw_data/fasta_compiled_all/", workFasta_name_with_extension)) %>% 
  dplyr::mutate(fasta_name = paste0(workFasta_name_with_extension)) %>% 
  dplyr::select(fasta_name)

write.table(fasta_non_pneumo, "inputs/list_fasta_non_pneumo_morethan23.txt",
            row.names = F, col.names = F, quote = F)

fasta_predicted_pneumo <- df_gen_all %>% 
  dplyr::filter(workWGS_stats_pneumo_cutoff == "predicted pure pneumo") %>% 
  # dplyr::mutate(fasta_name = paste0("/home/ron/pneumo_2025_lesserSundas/raw_data/fasta_compiled_all/", workFasta_name_with_extension)) %>% 
  dplyr::mutate(fasta_name = paste0(workFasta_name_with_extension)) %>% 
  dplyr::select(fasta_name)

write.table(fasta_predicted_pneumo, "inputs/list_fasta_predicted_pneumo_19to23.txt",
            row.names = F, col.names = F, quote = F)


# Test duplicated ID
df_gen_all_duplicated_ids <- df_gen_all %>% 
  dplyr::count(workFasta_name_with_extension) %>% 
  dplyr::filter(workFasta_name_with_extension > 1) %>% 
  dplyr::mutate(category = case_when(
    n == 2 ~ "Duplicated",
    n == 3 ~ "Triplicated",
    n == 4 ~ "Quadruplicated",
    n > 4 ~ "More than Quadruplicated"
  )) %>% 
  view()

# Generate final_pneumo_decision from df_workLab & df_gen_all ##################
df_final_pneumo <- read.csv("inputs/workLab_data.csv") %>% 
  dplyr::select(1:12) %>%
  dplyr::left_join(
    read.csv("inputs/genData_all.csv") %>% 
      dplyr::select(specimen_id,
                    # workWGS_success_failed, # trust workWGS_success_failed from workLab_data
                    workWGS_species_pw, workWGS_MLST_dc_species,
                    workWGS_stats_pneumo_cutoff,
                    workBLAST_lytA_predicted_species,
                    workWGS_kity_top_hits, workWGS_kity_rag_status)
    ,
    by = "specimen_id"
  ) %>% 
  dplyr::mutate(
    final_pneumo_decision = case_when(
      workWGS_species_pw == "streptococcus pneumoniae" | workWGS_MLST_dc_species == "spneumoniae" ~ "positive",
      # optochin == "s" | optochin == "?" ~ "positive", # "s" can be negative based on WGS result
      workLab_optochin == "?" ~ "positive",
      workLab_optochin == "r" ~ "negative", # correct notes result based on optochin & culture result
      workLab_culture_result == "neg" ~ "negative",
      workLab_culture_suspect == "yes" & workLab_culture_result == "pos" & workWGS_species_pw == "streptococcus pneumoniae" ~ "positive",
      workLab_culture_suspect == "yes" & workLab_culture_result == "pos" & workWGS_species_pw != "streptococcus pneumoniae" ~ "negative",  # OR "Failed_to_be_extracted"?
      workLab_culture_suspect == "yes" & workWGS_species_pw != "streptococcus pneumoniae" ~ "negative",  # OR "Failed_to_be_extracted"?
      TRUE ~ workLab_culture_result
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
    )
  ) %>% 
  dplyr::mutate(
    final_pneumo_decision = case_when(
      final_pneumo_decision == "pos" ~ "positive",
      TRUE ~ final_pneumo_decision
    )
  ) %>%
  # view() %>% 
  glimpse()

write.csv(df_final_pneumo, "inputs/final_pneumo_decision.csv", row.names = F)


df_final_pneumo_group <- df_final_pneumo %>% 
  dplyr::group_by(workLab_culture_suspect, workLab_optochin,
                  workLab_culture_result,
                  workLab_culture_result_final, workLab_culture_notes,
                  workBLAST_lytA_predicted_species,
                  workWGS_MLST_dc_species, workWGS_species_pw,
                  final_pneumo_decision
                  ) %>% 
  dplyr::summarise(count = n()) %>% 
  view() %>%
  glimpse()

# combine final_pneumo_decision to epiData
final_epiData <- read.csv("inputs/epiData.csv") %>% 
  dplyr::left_join(
    read.csv("inputs/final_pneumo_decision.csv") %>% 
      dplyr::select(specimen_id, final_pneumo_decision)
    ,
    by = "specimen_id"
  ) %>% 
  dplyr::select(# -age_month, -age_year_2groups,
                -house_roof, -house_building, -house_window, # re-grouped based on materials
                -vaccination_hibpentavalent_n, # corrected values based on dates
                -vaccination_pcv13_n)

write.csv(final_epiData, "inputs/epiData_with_final_pneumo_decision.csv", row.names = F)


# generate final PCV13 serotype decision #######################################
# subset only for pneumo QC based on popPUNK output for 1.9Mb-2.3Mb

df_gen_pneumo <- read.csv("inputs/genData_all.csv") %>% 
  # omit bad quality genomes using popPUNK output
  dplyr::left_join(
    read.table("raw_data/result_poppunk/db_all/db_all_qcreport.txt",
               sep = "\t", header = F) %>% 
      stats::setNames(c("workFasta_name", "workPoppunk_qc"))
    ,
    by = "workFasta_name"
  ) %>% 
  dplyr::mutate(
    workPoppunk_qc = case_when(
      is.na(workPoppunk_qc) ~ "pass_qc",
      TRUE ~ workPoppunk_qc
    )
  ) %>% 
  dplyr::filter(workPoppunk_qc == "pass_qc") %>% 
  dplyr::mutate(
    serotype_classification_PCV13 = case_when(
      workWGS_kity_predicted_serotype_regroup %in% c("1", "3", "4", "5", "7F") ~ "VT",
      str_detect(workWGS_kity_predicted_serotype_regroup, "6A|6B|9V|14|18C|19A|19F|23F") ~ "VT",
      workWGS_kity_predicted_serotype_regroup == "untypeable" ~ "untypeable",
      TRUE ~ "NVT"),
    serotype_classification_PCV13_pw = case_when(
      workWGS_serotype %in% c("1", "3", "4", "5", "7F") ~ "VT",
      str_detect(workWGS_serotype, "6A|6B|9V|14|18C|19A|19F|23F") ~ "VT",
      workWGS_serotype == "untypeable" ~ "untypeable",
      TRUE ~ "NVT"),
    serotype_classification_PCV13_final_decision = case_when(
      serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                     "6A", "6B", "9V", "14", "18C",
                                     "19A", "19F", "23F") ~ "VT",
      serotype_final_decision == "untypeable" ~ "untypeable",
      TRUE ~ "NVT"
    )
  ) %>% 
  dplyr::select(-contains("workWGS_stats")) %>% # I don't think assembly-stats is relevant to non-contigs fasta
  # view() %>%
  glimpse()


# combine data to epidata (but specified only to pneumo positive)
df_epi_gen_pneumo <- dplyr::right_join(
  read.csv("inputs/epiData_with_final_pneumo_decision.csv"),
  df_gen_pneumo,
  by = "specimen_id"
) %>% 
  glimpse()

write.csv(df_epi_gen_pneumo,
          "inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv",
          row.names = F)

# test all pneumo data
test_all_pneumo_data <- df_epi_gen_pneumo %>% 
  dplyr::filter(!is.na(serotype_final_decision)) %>% 
  dplyr::select(specimen_id, area, contains("serotype"), contains("species")) %>% 
  glimpse()

test_contigs <- df_epi_gen_pneumo %>% 
  dplyr::full_join(
    read.table("raw_data/qfile_filtered_19to23.txt", header = F) %>% 
      stats::setNames(c("workFasta_name", "location")) %>% 
      dplyr::mutate(contigs = "contigs_available") %>% 
      dplyr::select(-location)
    ,
    by = "workFasta_name"
  ) %>% 
  dplyr::select(workFasta_name, area, contains("serotype"), contains("species"),
                contigs) %>% 
  # view() %>% 
  glimpse()














































workLab_test <- df_workLab %>% 
  dplyr::left_join(
    df_gen_all %>% 
      dplyr::select(workFasta_name_with_extension, 7, 10:12)
    ,
    by = "workFasta_name_with_extension"
  ) %>% 
  view() %>% 
  glimpse()

report_data_inconsistencies <- workLab_test %>% 
  dplyr::filter(!is.na(workWGS_success_failed) & !is.na(workWGS_serotype.x)) %>% 
  view()

write.csv(report_data_inconsistencies, "report/temporary_WGS_inconsistencies_workLab_vs_workWGS.csv")




# Selecting final pneumo decision
workLab_test <- workLab_test %>% 
  group_by(workLab_culture_suspect, workLab_optochin,
           workLab_culture_result, workWGS_species,
           # workFasta_check,
           final_pneumo_decision) %>% 
  summarise(count = n()) %>% 
  view() %>% 
  glimpse()

# Test genome quality & contaminations
df_gen_all_testStats <- df_gen_all %>% 
  dplyr::select(workFasta_name_with_extension,
                workWGS_serotype, workWGS_sequence_type, workWGS_gpsc_strain,
                workWGS_genome_length,
                workWGS_stats_N50, workWGS_kity_top_hits, 
                workWGS_kity_max_percent, workWGS_kity_stage1_result,
                workWGS_kity_rag_status) %>% 
  dplyr::mutate(
    workWGS_kity_singleSerotype1 = case_when(
      !grepl("/", workWGS_kity_stage1_result) & !grepl("_", workWGS_kity_stage1_result) & !grepl("-", workWGS_kity_stage1_result) ~ "single",
      TRUE ~ workWGS_kity_stage1_result
    ),
    workWGS_kity_singleSerotype2 = case_when(
      !grepl("Below", workWGS_kity_stage1_result) & !grepl("Mixed", workWGS_kity_stage1_result) ~ "single",
      TRUE ~ workWGS_kity_stage1_result
    )
  ) %>% 
  view() %>% 
  glimpse()

df_gen_all_testStats_filtered <- df_gen_all_testStats %>% 
  dplyr::filter()

ggplot(df_gen_all_testStats, aes(x = workWGS_kity_rag_status,
                                 y = workWGS_genome_length,
                                 fill = workWGS_kity_stage1_result)) +
  geom_bar(position="stack", stat="identity") +
  theme_bw()

# Annoying data wrangling
kity_percent <- df_gen_all_testStats %>% 
  # workWGS_kity_stage1_result OR workWGS_kity_singleSerotype
  dplyr::count(workWGS_kity_singleSerotype2) %>% 
  dplyr::mutate(percent = round(proportions(n) * 100, 1),
                print = str_c(n, "(", percent, "%)"),
                workWGS_kity_singleSerotype2 = factor(workWGS_kity_singleSerotype2,
                                                         levels = c("single",
                                                      sort(unique(workWGS_kity_singleSerotype2[grepl("^Mixed serotypes-", workWGS_kity_singleSerotype2)])),
                                                      sort(unique(workWGS_kity_singleSerotype2[grepl("^Below", workWGS_kity_singleSerotype2)]))
                )))




ggplot(kity_percent, aes(x = workWGS_kity_singleSerotype2,
                         y = n)) +
  geom_col() +
  geom_text(aes(label = print), vjust = -0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))



# Test MLST comparison
# After inspecting the data, I would rather deal with Tseeman's result
df_gen_all_MLST_test <- df_gen_all %>% 
  dplyr::select(workFasta_name_with_extension,
                workWGS_sequence_type, workWGS_aroe, workWGS_gdh, workWGS_gki,
                workWGS_recp, workWGS_spi, workWGS_xpt, workWGS_ddl) %>% 
  dplyr::left_join(
    read.csv("raw_data/result_mlst/mlst_results.csv", header = F, sep = "\t") %>% 
      stats::setNames(c("workFasta_name_with_extension", "sp", "ST",
                        "workWGS_aroe", "workWGS_gdh", "workWGS_gki",
                        "workWGS_recp", "workWGS_spi", "workWGS_xpt", "workWGS_ddl")) %>% 
      dplyr::mutate(workFasta_name_with_extension = gsub("/home/ron/pneumo_2025_lesserSundas/raw_data/fasta/", "", workFasta_name_with_extension),
                    workWGS_aroe = gsub("aroE\\(", "", workWGS_aroe) %>% gsub("\\)$", "", .),
                    workWGS_gdh  = gsub("gdh\\(", "", workWGS_gdh) %>% gsub("\\)$", "", .),
                    workWGS_gki  = gsub("gki\\(", "", workWGS_gki) %>% gsub("\\)$", "", .),
                    workWGS_recp = gsub("recP\\(", "", workWGS_recp) %>% gsub("\\)$", "", .),
                    workWGS_spi  = gsub("spi\\(", "", workWGS_spi) %>% gsub("\\)$", "", .),
                    workWGS_xpt  = gsub("xpt\\(", "", workWGS_xpt) %>% gsub("\\)$", "", .),
                    workWGS_ddl  = gsub("ddl\\(", "", workWGS_ddl) %>% gsub("\\)$", "", .)
      )
    ,
    by = "workFasta_name_with_extension"
  ) %>% 
  view()






# Grouping vaccination test
df_epi_clean_grouped <- df_epi_clean %>% 
  group_by(sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd, age_year_2groups, final_pneumo_decision, serotype_classification_PCV13) %>% 
  summarise(count = n()) %>% 
  view() %>% 
  glimpse()


  
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








# test available fasta
df_confirm_lombok <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver3.xlsx",
                                        sheet = "need confirmation-lombok") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub(" ", "_", SPECIMEN_ID),
                area = "Lombok")

df_confirm_sumbawa <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver3.xlsx",
                                         sheet = "need confirmation-sumbawa_DC_ed") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub(" ", "_", SPECIMEN_ID),
                area = "Sumbawa")


# Generate report
df_epi_test <- dplyr::bind_rows(
  df_epi_lombok %>% 
    dplyr::rename_with(~ tolower(gsub("[^[:alnum:]_]", "", .x))) %>% 
    dplyr::select(2:13, area)
  ,
  df_epi_sumbawa %>% 
    dplyr::rename_with(~ tolower(gsub("[^[:alnum:]_]", "", .x))) %>% 
    dplyr::select(2:13, area)
) %>% 
  # mutate all "N/A" values
  dplyr::mutate(across(where(is.character), ~ na_if(., "N/A"))) %>% 
  dplyr::left_join(read.table("raw_data/test_available_fasta_renamed.txt", header = FALSE) %>% 
                     dplyr::mutate(file_check = "Accepted_by_DC",
                                   specimen_id = gsub("Streptococcus_pneumoniae_", "", V1),
                                   specimen_id = gsub(".fasta", "", specimen_id),
                                   fasta_name = V1) %>% 
                     dplyr::select(-V1)
                   ,
                   by = "specimen_id"
                   ) %>% 
  view()

# Use merged data to analyse accepted samples
df_epi_test <- df_epi_test %>% 
  dplyr::select(2:13, area) %>% 
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
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::rename_with(~ tolower(gsub("[^[:alnum:]_]", "", .x))) %>% 
  view()

undetected_fasta_report <- df_epi_test_right_join %>% 
  dplyr::filter(is.na(processing_status)) %>% 
  dplyr::mutate(V1 = paste0("/home/ron/pneumo_2025_lesserSundas/raw_data/fasta/", V1)) %>% 
  dplyr::select(V1)

write.table(undetected_fasta_report, "raw_data/test_undetected_49_fasta.txt",
            row.names = F, col.names = F, quote = F)




























