library(tidyverse)
library(ggtree)
source("global/fun.R")

df_epi_gen_pneumo <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
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
                )
tre_pp <- ape::read.tree("raw_data/result_poppunk/rapidnj_no_GPSC/rapidnj_no_GPSC_core_NJ.tree")
tre_pp$tip.label <- gsub("^Streptococcus_pneumoniae_", "", tre_pp$tip.label)
tre_raxml <- ape::read.tree("raw_data/result_raxml_from_panaroo/RAxML_bestTree.1_output_tree")
tre_raxml$tip.label <- gsub("^Streptococcus_pneumoniae_", "", tre_raxml$tip.label)

# test node
ggtree(tre_raxml) + 
  geom_tiplab(size = 2) +
  geom_label2(aes(subset=!isTip, label=node), size=2, color="darkred", alpha=0.5)

# analyse weird subtree:
subtree <- ape::extract.clade(tre_raxml, node = 363)
ggtree(subtree) + 
  geom_tiplab(size = 2) +
  geom_label2(aes(subset=!isTip, label=node), size=2, color="darkred", alpha=0.5)
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
show_pp

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
  geom_hilight(node=367, fill="pink", alpha=0.5)
show_raxml

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
                       ncol=3, order=3)
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
                         ncol = 3)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # GPSC
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_gpsc_strain),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    name = "GPSC",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) # +
  # MLST too diverse ST
  # ggnewscale::new_scale_fill() +
  # ggtreeExtra::geom_fruit(
  #   geom=geom_tile,
  #   mapping=aes(fill=df_epi_gen_pneumo$workWGS_MLST_dc_ST),
  #   width=0.02,
  #   offset=0.1
  # ) +
  # scale_fill_viridis_d(
  #   name = "MLST",
  #   # option = "C",
  #   direction = -1,
  #   guide = guide_legend(keywidth = 0.3, keyheight = 0.3, ncol = 2)
  # ) +
  # theme(
  #   legend.title=element_text(size=12), 
  #   legend.text=element_text(size=9),
  #   legend.spacing.y = unit(0.02, "cm")
  # )
tree_gen_raxml


# epi tree #####################################################################
tree_epi_raxml <- show_raxml %<+%
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
  # age groups
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
    guide=guide_legend(keywidth=0.3, keyheight=0.3, ncol=3,
                       order=2)
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
    guide=guide_legend(keywidth=0.3, keyheight=0.3, ncol=3,
                       order=3)
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
                       ncol=3,
                       order=4)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # illness: cough
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$illness_past24h_cough),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_manual(
    name="Respiratory illness: cough",
    values=c(col_map),
    breaks = c("yes", "no"),
    labels = c("yes", "no"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol=3,
                       order=5)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # total children 1-2 years old
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$n_child_1to2yo),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_brewer(
    name = "Total children aged 1-2 years old",
    palette = "Set2",
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3,
                         order=6)
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
                       ncol=3,
                       order=7)
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
                       ncol=3,
                       order=8)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  )
tree_epi_raxml


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
  # chloramphenicol
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_chloramphenicol),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    name = "Chloramphenicol",
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
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_clindamycin),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    name = "Clindamycin",
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
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_erythromycin),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    name = "Erythromycin",
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
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_fluoroquinolones),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    name = "Fluoroquinolones",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 5)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # tetracycline
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_tetracycline),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    name = "Tetracycline",
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
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_class_antifolates),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    name = "Antifolates",
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
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = df_epi_gen_pneumo$workWGS_AMR_meropenem),
    width = 0.02,
    offset = 0.1
  ) +
  scale_fill_viridis_d(
    name = "Meropenem",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 14)
  ) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # penicillins
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = df_epi_gen_pneumo$workWGS_AMR_class_penicillins),
    width = 0.02,
    offset = 0.1
  ) +
  scale_fill_viridis_d(
    name = "Penicillins",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 15)
  ) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # cephalosporins
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = df_epi_gen_pneumo$workWGS_AMR_class_cephalosporins),
    width = 0.02,
    offset = 0.1
  ) +
  scale_fill_viridis_d(
    name = "Cephalosporins",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 16)
  ) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  )
tree_amr_raxml






