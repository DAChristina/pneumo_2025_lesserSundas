library(tidyverse)
library(tidytree)
library(ggtree)
source("global/fun.R")

df_epi_gen_pneumo <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv")


tre_BD <- read_rds("outputs/genomics/choosen_n703/method_strictgamma_1e6/mcmc_bacdating.rds") # BactDating output
tree_pp <- system.file("extdata/BEAST",
                       "raw_data/result_poppunk/rapidnj_no_GPSC/rapidnj_no_GPSC_core_NJ.tree",
                       package="treeio")

read_tree_pp <- treeio::read.beast(tree_pp)

read_tree_pp <- ape::read.tree(tree_pp)
read_tree_pp <- ape::read.tree("raw_data/result_poppunk/rapidnj_no_GPSC/rapidnj_no_GPSC_core_NJ.tree")



tree_pp <- system.file("extdata/BEAST",
                       "raw_data/result_poppunk/rapidnj_no_GPSC/rapidnj_no_GPSC_core_NJ.tree",
                       package = "treeio")
read_tree_pp <- ape::read.tree(tree_pp)
read_tree_pp <- treeio::read.newick(tree_pp)



tre_names <- as.data.frame(tre_BD$tree$tip.label)
tre_names <- dplyr::left_join(tre_names, combined_data, by = c("tre_BD$tree$tip.label" = "tre.tip.label")) %>% 
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


