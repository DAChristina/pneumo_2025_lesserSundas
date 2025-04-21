library(tidyverse)
library(tidytree)
library(ggtree)
source("global/fun.R")

df_epi_gen_pneumo <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv")
tre_pp <- ape::read.tree("raw_data/result_poppunk/rapidnj_no_GPSC/rapidnj_no_GPSC_core_NJ.tree")
tre_pp$tip.label <- gsub("^Streptococcus_pneumoniae_", "", tre_pp$tip.label)

tre_raxml <- ape::read.tree("raw_data/result_raxml_from_panaroo/RAxML_bestTree.1_output_tree")
tre_raxml$tip.label <- gsub("^Streptococcus_pneumoniae_", "", tre_raxml$tip.label)

ggtree(tre_raxml) + geom_tiplab(size = 2)

show_pp <- ggtree(tre_pp,
                  layout = "fan",
                  open.angle=30,
                  size=0.75,
                  aes(colour=Clade)) +
  scale_colour_manual(
    name="GPSC31 Clades",
    values=c("gray75","steelblue","darkgreen","red"),
    labels=c("","Clade 1", "Clade 2", "Clade 3"),
    guide=guide_legend(keywidth=0.8,
                       keyheight=0.8,
                       order=1,
                       override.aes=list(linetype=c("0"=NA,
                                                    "Clade1"=1,
                                                    "Clade2"=1,
                                                    "Clade3"=1
                       )
                       )
    )
  ) + 
  ggnewscale::new_scale_colour() %<+%
  df_epi_gen_pneumo


show_pp

show_pp + 
  geom_tiplab(size = 2) + 
  geom_tippoint(aes(color = workWGS_gpsc_strain), size = 2) + 
  theme(legend.position = "right")














 btre_names <- dplyr::left_join(tre_names, combined_data, by = c("tre_BD$tree$tip.label" = "tre.tip.label")) %>% 
  # dplyr::filter(!is.na(clade)) %>% 
  dplyr::select(-ID) %>% 
  dplyr::rename(ID = 'tre_BD$tree$tip.label') %>% 
  dplyr::mutate(current.region.name = ifelse(is.na(current.region.name), "Unknown", current.region.name),
                ageGroup7 = ifelse(is.na(ageGroup7), "Unknown", ageGroup7),
                ageGroup2 = ifelse(is.na(ageGroup2), "Unknown", ageGroup2))

ggtree_ageGroup7 <- ggtree(tre_BD$tree,
                           mrsd = 2014-07-11) %<+%
  tre_names +
  geom_tippoint(aes(color=ageGroup7))
ggtree_ageGroup7


ggtree_ageGroup2 <- ggtree_ageGroup7 %<+%
  tre_names +
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=tre_names$ageGroup2),
    width=15,
    offset=0.05
  ) +
  scale_fill_manual(
    name="Demographic Groups (2)",
    values=c(col_map),
    breaks = c("children", "adults", "Unknown"),
    labels = c("Children (< 15)", "Adults", "Unknown"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3, ncol=2, order=3)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  )
ggtree_ageGroup2


