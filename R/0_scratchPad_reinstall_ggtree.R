# Immune modulation (pspA)
tree_test <- show_raxml %<+%
  df_epi_gen_pneumo +
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
  geom_axis_text(angle=-45, hjust=0, size=1.5) +
  theme(
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  )
tree_test


remove.packages("ggtreeExtra")
# install.packages("ggtreeExtra")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtreeExtra", force = TRUE)







library("ggplot2")
library("ggtree")
library("ggtreeExtra")
library(treeio)

#tree
trfile <- system.file("extdata", "tree.nwk", package="ggtreeExtra")
tr <- read.tree(trfile)
numtip <- length(tr$tip.label)

#data
dat2 <- data.frame(ID=tr$tip.label,
                   Location=c(rep("HK", 50), rep("TW", 36), rep("SX", 30), rep("GD", 48),
                              rep("HN", 20), rep("AH", 20), rep("FJ", 26)),
                   Length=abs(rnorm(n=numtip, mean=0.6)),
                   Group=c(rep("Yes", 200), rep("No", 30)),
                   Abundance=abs(rnorm(n=numtip, mean=10, sd=0.00000001)))

p <- ggtree(tr, layout="circular", size=0.1) + geom_treescale(x=6, y=0, fontsize=1.2, linesize=0.3)

p + geom_fruit(data=dat2,
               geom=geom_bar,
               mapping=aes(y=ID, x=Abundance, fill=Location),
               pwidth=0.1,
               stat="identity",
               orientation="y") +
  scale_fill_manual(values=c("#D15FEE","#EE6A50","#FFC0CB","#8E8E38","#9ACD32","#006400","#8B4513"),
                    guide=guide_legend(keywidth=0.5, keyheight=0.5, order=6)) +
  theme(legend.position=c(0.95, 0.5),
        legend.title=element_text(size=7),
        legend.text=element_text(size=6),
        legend.spacing.y = unit(0.02, "cm")) +
  geom_axis_text(angle=-45, hjust=0)
Error in `geom_bar()`:
  ! Problem while computing position.
ℹ Error occurred in the 5th layer.
Caused by error in `self$var %||% stack_var(data)`:
  ! lazy-load database '/home/ron/R/x86_64-pc-linux-gnu-library/4.5/ggtreeExtra/R/ggtreeExtra.rdb' is corrupt
Run `rlang::last_trace()` to see where the error occurred.
Warning message:
  In self$var %||% stack_var(data) : internal error -3 in R_decompress1





library(ggtreeExtra)
library(ggtree)
library(treeio)
library(ggplot2)

beast_file <- system.file("examples/MCC_FluA_H3.tree", package="ggtree")
genotype_file <- system.file("examples/Genotype.txt", package="ggtree")

tree <- read.beast(beast_file)
genotype <- read.table(genotype_file, sep="\t")

colnames(genotype) <- sub("\\.$", "", colnames(genotype))
genotype$ID <- row.names(genotype)

dat <- reshape2::melt(genotype, id.vars="ID", variable.name = "type", value.name="genotype", factorsAsStrings=FALSE)
dat$genotype <- unlist(lapply(as.vector(dat$genotype),function(x)ifelse(nchar(x)==0,NA,x)))

p <- ggtree(tree) + geom_treescale()

p2 <- p + geom_fruit(data=dat,
                     geom=geom_tile,
                     mapping=aes(y=ID, x=type, fill=genotype),
                     color="white") +
  scale_fill_manual(values=c("steelblue", "firebrick", "darkgreen"),
                    na.translate=FALSE) +
  geom_axis_text(angle=-45, hjust=0, size=1.5) +
  geom_tiplab(align = TRUE, linesize=0, offset = 6, size=2) +
  xlim_tree(xlim=c(0, 36)) +
  scale_y_continuous(limits = c(-1, NA))
p2



