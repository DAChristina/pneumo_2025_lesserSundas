# summarise genome data
compiled_workLab <- read.csv("inputs/workLab_data.csv") %>% 
  glimpse()


compiled_workLab <- dplyr::left_join(
  read.csv("inputs/workLab_data.csv")
  ,
  read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
    dplyr::select(specimen_id, workWGS_species_pw, workPoppunk_qc)
  ,
  by = "specimen_id"
) %>% 
  glimpse()

optochin_perArea <- compiled_workLab %>% 
  # dplyr::filter(!is.na(workWGS_species_pw)) %>% 
  group_by(area, workLab_optochin) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  view() %>% 
  glimpse()

data_nonPneumo <- compiled_workLab %>% 
  dplyr::filter(!is.na(workWGS_species_pw)) %>% 
  glimpse()


grouped_area_plus_nonPneumo <- compiled_workLab %>% 
  dplyr::filter(!is.na(workWGS_species_pw),
                workWGS_species_pw != "Streptococcus pneumoniae") %>% 
  view() %>% 
  glimpse()

grouped_area_pneumo <- compiled_workLab %>% 
  dplyr::filter(!is.na(workWGS_species_pw),
                workWGS_species_pw == "Streptococcus pneumoniae") %>% 
  group_by(area) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  # view() %>% 
  glimpse()

grouped_area_filter <- compiled_workLab %>% 
  dplyr::filter(!is.na(workWGS_species_pw),
                workPoppunk_qc == "pass_qc") %>% 
  group_by(area) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  # view() %>% 
  glimpse()


filter_notyetWGS <- compiled_workLab %>% 
  dplyr::filter(workLab_optochin ==  "s" & is.na(workFasta_file_check) & area == "sumbawa") %>% 
  view() %>% 
  glimpse()

# write temporary report
write.csv(filter_notyetWGS, "report/temporary_list_reWGS_Sumbawa.csv", row.names = F)

writexl::write_xlsx()
