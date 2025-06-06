library(tidyverse)
library(ggtree)
library(ggtreeExtra)
source("global/fun.R")

df_epi_gen_pneumo <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
  dplyr::left_join(
    read.csv("inputs/genData_pneumo_panvita_long.csv") %>% 
      dplyr::select(Strains,
                    contains("gene_present_absent_"))
    ,
    by = c("workFasta_name" = "Strains")
  ) %>% 
  dplyr::filter(workPoppunk_qc == "pass_qc") %>%
  # dplyr::filter(workWGS_species_pw == "Streptococcus pneumoniae") %>% 
  dplyr::rename(label = specimen_id) %>% # annoying ggtree label annotation
  dplyr::mutate(serotype_final_decision = factor(serotype_final_decision,
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
                                                   "untypeable")
                                                 ),
                gpsc_dominant_filter = case_when(
                  workWGS_gpsc_strain == "Not assigned" ~ "Not assigned",
                  workWGS_gpsc_strain == "11" ~ "11",
                  workWGS_gpsc_strain == "14" ~ "14",
                  workWGS_gpsc_strain == "5" ~ "5",
                  workWGS_gpsc_strain == "13" ~ "13",
                  workWGS_gpsc_strain == "642" ~ "642",
                  workWGS_gpsc_strain == "45" ~ "45",
                  TRUE ~ NA_character_
                ),
                # annoying factor setup
                n_child_1to2yo = factor(n_child_1to2yo,
                                        levels = c("0", "1", "2")),
                
                workWGS_AMR_amoxicillin = factor(workWGS_AMR_amoxicillin,
                                                 levels = c("S", "R")),
                workWGS_AMR_ceftriaxone = factor(workWGS_AMR_ceftriaxone,
                                                 levels = c("S/S", "S/I")),
                
                workWGS_AMR_cefotaxime = factor(workWGS_AMR_cefotaxime,
                                                levels = c("S/S", "S/I")),
                workWGS_AMR_cefuroxime = factor(workWGS_AMR_cefuroxime,
                                                levels = c("S", "R")),
                workWGS_AMR_meropenem = factor(workWGS_AMR_meropenem,
                                               levels = c("S", "I", "R")),
                workWGS_AMR_penicillin = factor(workWGS_AMR_penicillin,
                                                levels = c("S/S", "S/R", "I/R")),
                workWGS_AMR_class_cephalosporins = factor(workWGS_AMR_class_cephalosporins,
                                               levels = c("S", "I", "R")),
                workWGS_AMR_class_penicillins = factor(workWGS_AMR_class_penicillins,
                                               levels = c("S", "I", "R")),
                workWGS_AMR_class_antifolates = factor(workWGS_AMR_class_antifolates,
                                                       levels = c("NF",
                                                                  "R (folA_I100L)",
                                                                  "R (folP_57-70)",
                                                                  "R (folA_I100L & folP_57-70)")),
                
                # more annoying factor setup
                workWGS_AMR_logic_class_amphenicols = case_when(
                  workWGS_AMR_logic_class_amphenicols ~ "Resistance amphenicols",
                  TRUE ~ "NF"
                ),
                workWGS_AMR_logic_class_amphenicols = factor(workWGS_AMR_logic_class_amphenicols, levels = c("NF", "Resistance amphenicols")),
                
                workWGS_AMR_logic_class_lincosamides = case_when(
                  workWGS_AMR_logic_class_lincosamides ~ "Resistance lincosamides",
                  TRUE ~ "NF"
                ),
                workWGS_AMR_logic_class_lincosamides = factor(workWGS_AMR_logic_class_lincosamides, levels = c("NF", "Resistance lincosamides")),
                
                workWGS_AMR_logic_class_macrolides = case_when(
                  workWGS_AMR_logic_class_macrolides ~ "Resistance macrolides",
                  TRUE ~ "NF"
                ),
                workWGS_AMR_logic_class_macrolides = factor(workWGS_AMR_logic_class_macrolides, levels = c("NF", "Resistance macrolides")),
                
                workWGS_AMR_logic_class_quinolones = case_when(
                  workWGS_AMR_logic_class_quinolones ~ "Resistance quinolones",
                  TRUE ~ "NF"
                ),
                workWGS_AMR_logic_class_quinolones = factor(workWGS_AMR_logic_class_quinolones, levels = c("NF", "Resistance quinolones")),
                
                workWGS_AMR_logic_class_aminoglycosides = case_when(
                  workWGS_AMR_logic_class_aminoglycosides ~ "Resistance aminoglycosides",
                  TRUE ~ "NF"
                ),
                workWGS_AMR_logic_class_aminoglycosides = factor(workWGS_AMR_logic_class_aminoglycosides, levels = c("NF", "Resistance aminoglycosides")),
                
                workWGS_AMR_logic_class_oxazolidinones = case_when(
                  workWGS_AMR_logic_class_oxazolidinones ~ "Resistance oxazolidinones",
                  TRUE ~ "NF"
                ),
                workWGS_AMR_logic_class_oxazolidinones = factor(workWGS_AMR_logic_class_oxazolidinones, levels = c("NF", "Resistance oxazolidinones")),
                
                workWGS_AMR_logic_class_tetracyclines = case_when(
                  workWGS_AMR_logic_class_tetracyclines ~ "Resistance tetracyclines",
                  TRUE ~ "NF"
                ),
                workWGS_AMR_logic_class_tetracyclines = factor(workWGS_AMR_logic_class_tetracyclines, levels = c("NF", "Resistance tetracyclines")),
                
                workWGS_AMR_logic_class_carbapenems = case_when(
                  workWGS_AMR_logic_class_carbapenems ~ "Resistance carbapenems",
                  TRUE ~ "NF"
                ),
                workWGS_AMR_logic_class_carbapenems = factor(workWGS_AMR_logic_class_carbapenems, levels = c("NF", "Resistance carbapenems")),
                
                workWGS_AMR_logic_class_cephalosporins = case_when(
                  workWGS_AMR_logic_class_cephalosporins ~ "Resistance cephalosporins",
                  TRUE ~ "NF"
                ),
                workWGS_AMR_logic_class_cephalosporins = factor(workWGS_AMR_logic_class_cephalosporins, levels = c("NF", "Resistance cephalosporins")),
                
                workWGS_AMR_logic_class_penicillins = case_when(
                  workWGS_AMR_logic_class_penicillins ~ "Resistance penicillins",
                  TRUE ~ "NF"
                ),
                workWGS_AMR_logic_class_penicillins = factor(workWGS_AMR_logic_class_penicillins, levels = c("NF", "Resistance penicillins")),
                
                workWGS_AMR_logic_class_antifolates = case_when(
                  workWGS_AMR_logic_class_antifolates ~ "Resistance antifolates",
                  TRUE ~ "NF"
                ),
                workWGS_AMR_logic_class_antifolates = factor(workWGS_AMR_logic_class_antifolates, levels = c("NF", "Resistance antifolates")),
                workWGS_AMR_MDR_flag = factor(workWGS_AMR_MDR_flag,
                                              levels = c("non-MDR", "MDR")),
                
                # more annoying factor adjustment
                gene_present_absent_lytA_Exoenzyme = factor(gene_present_absent_lytA_Exoenzyme, levels = c("NF", "lytA")),
                gene_present_absent_ply_Exotoxin = factor(gene_present_absent_ply_Exotoxin, levels = c("NF", "ply")),
                gene_present_absent_pavA_Adherence = factor(gene_present_absent_pavA_Adherence, levels = c("NF", "pavA")),
                gene_present_absent_pspA_Immune.modulation = factor(gene_present_absent_pspA_Immune.modulation, levels = c("NF", "pspA")),
                gene_present_absent_cbpA.pspC_Adherence = factor(gene_present_absent_cbpA.pspC_Adherence, levels = c("NF", "cbpA/pspC"))
                ) %>% 
  # test label shuffling
  dplyr::arrange(
    # serotype_classification_PCV13_final_decision,
    # workWGS_gpsc_strain,
    label
  )
tre_pp <- ape::read.tree("raw_data/result_poppunk/rapidnj_no_GPSC/rapidnj_no_GPSC_core_NJ.tree")
tre_pp$tip.label <- gsub("^Streptococcus_pneumoniae_", "", tre_pp$tip.label)
tre_raxml <- ape::read.tree("raw_data/result_raxml_from_panaroo/RAxML_bestTree.1_output_tree")
tre_raxml$tip.label <- gsub("^Streptococcus_pneumoniae_", "", tre_raxml$tip.label)

# focused on raxml tree: rearrange label coz' ggtree link label to row_names
df_epi_gen_pneumo <- 
  dplyr::left_join(
    data.frame(tre_raxml$tip.label),
    df_epi_gen_pneumo,
    by = c("tre_raxml.tip.label" = "label")
  )

rownames(df_epi_gen_pneumo) <- tre_raxml$tip.label

# test node
ggtree(tre_raxml) + 
  geom_tiplab(size = 2) +
  geom_label2(aes(subset=!isTip, label=node), size=2, color="darkred", alpha=0.5)

# analyse weird subtree:
subtree <- ape::extract.clade(tre_raxml, node = 571)
ggtree(subtree) + 
  geom_tiplab(size = 2) +
  geom_label2(aes(subset=!isTip, label=node), size=3, color="darkred", alpha=0.5)
subtree$tip.label

# basic
show_pp <- ggtree(tre_pp,
                  layout = "fan",
                  open.angle=30,
                  size=0.75,
                  # aes(colour=Clade)
                  ) %<+% 
  df_epi_gen_pneumo +
  # geom_tiplab(size = 2) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  geom_hilight(node=367, fill="pink", alpha=0.5)
# show_pp

show_raxml <- ggtree(tre_raxml,
                  layout = "fan",
                  open.angle=30,
                  size=0.75,
                  # aes(colour=Clade)
) %<+% 
  df_epi_gen_pneumo +
  # geom_tiplab(size = 2) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # geom_tiplab(size = 2) +
  # geom_label2(aes(subset=!isTip, label=node), size=3, color="darkred", alpha=0.5) +
  geom_hilight(node=375, fill="deepskyblue3", alpha=0.5) + # Not assigned
  geom_cladelabel(node=375, colour="grey10",
                  barsize = 0.01,
                  label = "NA",
                  offset.text = 0.02,
                  # angle = 80,
                  align = F) +
  geom_hilight(node=397, fill="skyblue2", alpha=0.5) + # GPSC11, NVT
  geom_cladelabel(node=397, colour="grey10",
                  barsize = 0.01,
                  label = "GPSC11",
                  offset.text = 0.008,
                  angle = -30,
                  align = F) +
  geom_hilight(node=589, fill="indianred3", alpha=0.5) + # GPSC14, VT (23F)
  geom_cladelabel(node=589, colour="grey10",
                  barsize = 0.01,
                  label = "GPSC14",
                  offset.text = 0.008,
                  # angle = -30,
                  align = F) +
  geom_hilight(node=619, fill="indianred3", alpha=0.5) + # GPSC5, VT (19F)
  geom_cladelabel(node=619, colour="grey10", # up 1 layer to 611
                  barsize = 0.01,
                  label = "GPSC5",
                  offset.text = 0.001,
                  align = F) +
  geom_hilight(node=505, fill="indianred3", alpha=0.5) + # GPSC13, VT (6A)
  geom_cladelabel(node=500, colour="grey10",
                  barsize = 0.01,
                  label = "GPSC13",
                  offset.text = 0.008,
                  angle = 50,
                  align = F)
  # geom_hilight(node=478, fill="pink", alpha=0.5) + # GPSC642
  # geom_cladelabel(node=478, colour="grey10",
  #                 barsize = 0.01,
  #                 label = "GPSC642",
  #                 offset.text = 0.001,
  #                 align = F) +
  # geom_hilight(node=577, fill="pink", alpha=0.5) + # GPSC45
  # geom_cladelabel(node=577, colour="grey10",
  #                 barsize = 0.01,
  #                 label = "GPSC45",
  #                 offset.text = 0.01,
  #                 align = F)
# show_raxml

# gen tree #####################################################################
tree_gen_raxml <- show_raxml %<+%
  df_epi_gen_pneumo +
  # vaccine classification
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$serotype_classification_PCV13_final_decision),
    width=0.02,
    offset=0.05
  ) +
  scale_fill_manual(
    name="PCV13 serotype coverage",
    values=c(col_map),
    breaks = c("VT", "NVT", "untypeable"),
    labels = c("VT", "NVT", "untypeable"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol=3, order=1)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # serotype
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$serotype_final_decision),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    name = "Serotype",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 5, order = 2)
  ) +
  theme(
    legend.title=element_text(size=12),
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # GPSC
  # ggnewscale::new_scale_fill() +
  # ggtreeExtra::geom_fruit(
  #   geom=geom_tile,
  #   mapping=aes(fill=df_epi_gen_pneumo$gpsc_dominant_filter),
  #   width=0.02,
  #   offset=0.1
  # ) +
  # scale_fill_viridis_d(
  #   name = "GPSC",
  #   option = "C",
  #   direction = -1,
  #   guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
  #                        ncol = 5, order = 3)
  # ) +
  # theme(
  #   legend.title=element_text(size=12),
  #   legend.text=element_text(size=9),
  #   legend.spacing.y = unit(0.02, "cm")
  # ) 
  # # age groups
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$age_year_3groups),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_manual(
    name="Age groups",
    values=c(col_map),
    labels=c("<1", "1-2", "3-5"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol = 3, order = 4)
  ) +
  theme(
    legend.title=element_text(size=12),
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # area
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$area),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_manual(
    name="Area",
    values=c(col_map),
    labels=c("Lombok", "Sumbawa"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol = 2, order = 5)
  ) +
  theme(
    legend.title=element_text(size=12),
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # illness
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$illness_past24h_difficulty_compiled),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_manual(
    name="Respiratory illness",
    values=c(col_map),
    breaks = c("≥ 1 respiratory illness", "no"),
    labels = c("≥ 1 respiratory illness", "no"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol = 2, order = 6)
  ) +
  theme(
    legend.title=element_text(size=12),
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # contact other children
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$contact_otherChildren),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_manual(
    name="Peer contact",
    values=c(col_map),
    breaks = c("yes", "no"),
    labels = c("yes", "no"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol = 2, order = 7)
  ) +
  theme(
    legend.title=element_text(size=12),
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # contact smoking
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$contact_cigarettes),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_manual(
    name="Cigarettes exposure",
    values=c(col_map),
    breaks = c("yes", "no"),
    labels = c("yes", "no"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol = 2, order = 8)
  ) +
  theme(
    legend.title=element_text(size=12),
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  )
tree_gen_raxml


# AMR tree #####################################################################
tree_amr_raxml <- show_raxml %<+%
  df_epi_gen_pneumo +
  # vaccine classification
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$serotype_classification_PCV13_final_decision),
    width=0.02,
    offset=0.05
  ) +
  scale_fill_manual(
    name="PCV13 serotype coverage",
    values=c(col_map),
    breaks = c("VT", "NVT", "untypeable"),
    labels = c("VT", "NVT", "untypeable"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol=3,
                       order=1)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # serotype
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$serotype_final_decision),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    name = "Serotype",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 5, order = 2)
  ) +
  theme(
    legend.title=element_text(size=12),
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # chloramphenicol
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_logic_class_amphenicols),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    # name = "Chloramphenicol",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 2)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # clindamycin
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_logic_class_lincosamides),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    # name = "Clindamycin",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 3)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # erythromycin
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_logic_class_macrolides),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    # name = "Erythromycin",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 4)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # fluoroquinolones
  # ggnewscale::new_scale_fill() +
  # ggtreeExtra::geom_fruit(
  #   geom=geom_tile,
  #   mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_logic_class_quinolones),
  #   width=0.02,
  #   offset=0.1
  # ) +
  # scale_fill_viridis_d(
  #   # name = "Fluoroquinolones",
  #   option = "C",
  #   direction = -1,
  #   guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
  #                        ncol = 3, order = 5)
  # ) +
  # theme(
  #   legend.title=element_text(size=12), 
  #   legend.text=element_text(size=9),
  #   legend.spacing.y = unit(0.02, "cm")
  # ) +
  # tetracycline
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_logic_class_tetracyclines),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    # name = "Tetracycline",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 6)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # antifolates
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_logic_class_antifolates),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    # name = "Antifolates",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 7)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # meropenem
  # ggnewscale::new_scale_fill() +
  # ggtreeExtra::geom_fruit(
  #   geom = geom_tile,
  #   mapping = aes(fill = df_epi_gen_pneumo$workWGS_AMR_logic_class_carbapenems),
  #   width = 0.02,
  #   offset = 0.1
  # ) +
  # scale_fill_viridis_d(
  #   # name = "Meropenem",
  #   option = "C",
  #   direction = -1,
  #   guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
  #                        ncol = 3, order = 14)
  # ) +
  # theme(
  #   legend.title = element_text(size = 0),
  #   legend.text = element_text(size = 9),
  #   legend.spacing.y = unit(0.02, "cm")
  # ) +
  # penicillins
  # ggnewscale::new_scale_fill() +
  # ggtreeExtra::geom_fruit(
  #   geom = geom_tile,
  #   mapping = aes(fill = df_epi_gen_pneumo$workWGS_AMR_logic_class_penicillins),
  #   width = 0.02,
  #   offset = 0.1
  # ) +
  # scale_fill_viridis_d(
  #   # name = "Penicillins",
  #   option = "C",
  #   direction = -1,
  #   guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
  #                        ncol = 3, order = 15)
  # ) +
  # theme(
  #   legend.title = element_text(size = 0),
  #   legend.text = element_text(size = 9),
  #   legend.spacing.y = unit(0.02, "cm")
  # ) +
  # cephalosporins
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = df_epi_gen_pneumo$workWGS_AMR_logic_class_cephalosporins),
    width = 0.02,
    offset = 0.1
  ) +
  scale_fill_viridis_d(
    # name = "Cephalosporins",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 16)
  ) +
  theme(
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # MDR flag
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = df_epi_gen_pneumo$workWGS_AMR_MDR_flag),
    width = 0.02,
    offset = 0.1
  ) +
  scale_fill_viridis_d(
    name = "MDR Flag",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 17)
  ) +
  theme(
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  theme(
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # Adherence (pavA)
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = df_epi_gen_pneumo$gene_present_absent_pavA_Adherence),
    width = 0.02,
    offset = 0.1
  ) +
  scale_fill_viridis_d(
    name = "Adherence (pavA)",
    option = "C",
    direction = 1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 2, order = 18)
  ) +
  theme(
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # Adherence (cbpA/pspC)
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = df_epi_gen_pneumo$gene_present_absent_cbpA.pspC_Adherence),
    width = 0.02,
    offset = 0.1
  ) +
  scale_fill_viridis_d(
    name = "Adherence (cbpA/pspC)",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 2, order = 19)
  ) +
  theme(
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # Exoenzyme (lytA)
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = df_epi_gen_pneumo$gene_present_absent_lytA_Exoenzyme),
    width = 0.02,
    offset = 0.1
  ) +
  scale_fill_viridis_d(
    name = "Exoenzyme (lytA)",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 2, order = 20)
  ) +
  theme(
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # Exotoxin (ply)
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = df_epi_gen_pneumo$gene_present_absent_ply_Exotoxin),
    width = 0.02,
    offset = 0.1
  ) +
  scale_fill_viridis_d(
    name = "Exotoxin (ply)",
    option = "C",
    direction = 1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 2, order = 21)
  ) +
  theme(
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # Immune modulation (pspA)
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = df_epi_gen_pneumo$gene_present_absent_pspA_Immune.modulation),
    width = 0.02,
    offset = 0.1
  ) +
  scale_fill_viridis_d(
    name = "Immune modulation (pspA)",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 2, order = 22)
  ) +
  # geom_axis_text(angle=-45, hjust=0, size=1.5) +
  theme(
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  )
tree_amr_raxml

filtered_df <- df_epi_gen_pneumo %>% 
  dplyr::select(
    # serotype_classification_PCV13_final_decision,
    # serotype_final_decision,
    contains("workWGS_AMR_logic_class_"),
    -workWGS_AMR_logic_class_counts,
    workWGS_AMR_MDR_flag,
    # contains("gene_present_absent_")
  ) %>% 
  dplyr::mutate(
    workWGS_AMR_MDR_flag = ifelse(workWGS_AMR_MDR_flag == "non-MDR", "NF", "MDR"),
    workWGS_AMR_MDR_flag = factor(workWGS_AMR_MDR_flag,
                                  levels = c("NF", "MDR"))
  ) %>% 
  dplyr::rename_with(
    ~ sub("workWGS_AMR_logic_class_|workWGS_AMR_|gene_present_absent_", "", .)
  ) %>% 
  glimpse()

all_labels <- unique(unlist(filtered_df))
manual <- c("NF" = "palegoldenrod")
others <- setdiff(all_labels, names(manual))

auto_col <- scales::hue_pal()(length(others))
names(auto_col) <- others

final_col <- c(manual, auto_col)

ggtree::gheatmap(show_raxml, filtered_df,
                 offset=0.001, width=1, font.size=3, 
                 colnames_angle=-90, hjust=0) +
  scale_fill_manual(values = final_colors)




