# test poweer

library(pwr)
# two-sample t-test with observed effect size d = 0.5, sample size = 100 per group
test1 <- pwr.t.test(n = 400,
                    d = 0.5,
                   sig.level = 0.05,
                   type = "two.sample",
                   alternative = "two.sided")
test1

test2 <- pwr.t.test(n = 400,
                    power = 0.8,
                    # d = 0.5,
                    sig.level = 0.05,
                    type = "two.sample",
                    alternative = "two.sided")

test2

# test3 based on previous sampling:
library(pwr)

# Inputs
p1 <- 0.23  # Pre-PCV VT carriage
p2 <- 0.10  # Expected post-PCV VT carriage
effect_size <- ES.h(p1, p2)  # Cohen's h for proportions

# Sample size calculation
test3 <- pwr.2p.test(h = effect_size,
                     sig.level = 0.05,
                     power = 0.8,
                     alternative = "two.sided")
test3

library(pwr)

# Function to calculate post-PCV VT carriage given VE and coverage
post_vt_carriage <- function(p1, coverage, VE) {
  (1 - coverage) * p1 + coverage * p1 * (1 - VE) # basically post-VT
}

# Post VT Carriage=(1−coverage)⋅p1​+coverage⋅p1​⋅(1−VE)
# =0.2⋅0.23+0.8⋅0.23⋅(1−0.5)=0.046+0.092=0.138
library(pwr)

p1 <- 0.23 # VT pre-PCV
p2 <- 0.138 # expected VT post-PCV

h <- pwr::ES.h(p1, p2)

# Use power calculation with known n (post-PCV), assume historical p1
power_result <- pwr::pwr.2p.test(h = h,
                                 n = 900,
                                 sig.level = 0.05,
                                 alternative = "two.sided")
power_result


# test required samples
p1 <- 0.23  # baseline VT carriage
coverage <- 0.80
VE_values <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9) # vaccine effectiveness
alpha <- 0.05
power <- 0.8

# Calculate sample size for each VE
results <- data.frame(VE = numeric(),
                      PostCarriage = numeric(),
                      Cohen_h = numeric(),
                      SampleSizePerGroup = numeric(),
                      TotalSampleSize = numeric())

for (VE in VE_values) {
  p2 <- post_vt_carriage(p1, coverage, VE)
  h <- ES.h(p1, p2)
  test <- pwr.2p.test(h = h, power = power, sig.level = alpha, alternative = "two.sided")
  results <- rbind(results, data.frame(
    VE = VE,
    PostCarriage = round(p2, 3),
    Cohen_h = round(h, 3),
    SampleSizePerGroup = ceiling(test$n),
    TotalSampleSize = ceiling(2 * test$n)
  ))
}

print(results)















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


tree_gen_pp <- show_pp %<+%
  df_epi_gen_pneumo +
  # vaccine classification
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$serotype_classification_PCV13_final_decision),
    width=0.001,
    offset=0.05
  ) +
  scale_fill_manual(
    name="PCV13 serotype coverage",
    values=c(col_map),
    breaks = c("VT", "NVT", "untypeable"),
    labels = c("VT", "NVT", "untypeable"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol=3, order=3)
  )
tree_gen_pp



tree_gen_pp <- show_pp %<+%
  df_epi_gen_pneumo +
  # vaccine classification
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$serotype_classification_PCV13_final_decision),
    width=0.001,
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
    width=0.001,
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
    width=0.001,
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
#   width=0.001,
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
tree_gen_pp



# epi tree #####################################################################
tree_epi_pp <- show_pp %<+%
  df_epi_gen_pneumo +
  # vaccine classification
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$serotype_classification_PCV13_final_decision),
    width=0.001,
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
    width=0.001,
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
    width=0.001,
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
    width=0.001,
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
    width=0.001,
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
    width=0.001,
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
    width=0.001,
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
    width=0.001,
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
tree_epi_pp


# AMR tree #####################################################################
tree_amr_pp <- show_pp %<+%
  df_epi_gen_pneumo +
  # vaccine classification
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$serotype_classification_PCV13_final_decision),
    width=0.001,
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
    width=0.001,
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
    width=0.001,
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
    width=0.001,
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
    width=0.001,
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
    width=0.001,
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
    width=0.001,
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
    width = 0.001,
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
    width = 0.001,
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
    width = 0.001,
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
  ) +
  # MDR flag
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = df_epi_gen_pneumo$workWGS_AMR_MDR_flag),
    width = 0.001,
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
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  )
tree_amr_pp
