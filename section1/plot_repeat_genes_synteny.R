library(ggplot2)
library(tidyverse)
library(patchwork)

# CBRIG density plots
gene_density <- read.table("caenorhabditis_briggsae_QX1410_v2.1_curated.noMtDNA.scaffolds.fa_gene_density.tsv", col.names=c("chr", "start", "stop", "features", "span", "window_size", "proportion"))
repeat_density <- read.table("caenorhabditis_briggsae_QX1410_v2.1_curated.noMtDNA.scaffolds.fa.repeat_density.tsv", col.names=c("chr", "start", "stop", "features", "span", "window_size", "proportion"))

p1 <- ggplot() +
  geom_point(data=gene_density, aes(x=start, y=proportion), color="blue", alpha=0.03) + 
  geom_point(data=repeat_density, aes(x=start, y=proportion), color="orange", alpha=0.03) + 
  geom_smooth(data=gene_density, aes(x=start, y=proportion), color="blue") +
  geom_smooth(data=repeat_density, aes(x=start, y=proportion), color="orange") +
  theme_bw() + 
  xlab("Position in chromosome (Mb)") + ylab("Proportion") + 
  scale_x_continuous(labels=function(x)x/1e6, expand=c(0.005,0), position = "top") + scale_y_continuous(expand=c(0.01,0)) + coord_cartesian(ylim=c(0, 0.6)) + 
  facet_grid(~chr, scales="free_x")  + theme(text = element_text(size=12), strip.text.y = element_text(angle = 0)) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), strip.background = element_blank(), strip.text.x = element_blank())

# CELEG density plots
gene_density <- read.table("c_elegans.PRJNA13758.WS279.genomic.fa_gene_density.tsv", col.names=c("chr", "start", "stop", "features", "span", "window_size", "proportion")) %>% filter(chr != "MtDNA")
repeat_density <- read.table("c_elegans.PRJNA13758.WS279.genomic.fa.repeat_density.tsv", col.names=c("chr", "start", "stop", "features", "span", "window_size", "proportion")) %>% filter(chr != "MtDNA")

p2 <- ggplot() +
  geom_point(data=gene_density, aes(x=start, y=proportion), color="blue", alpha=0.03) + 
  geom_point(data=repeat_density, aes(x=start, y=proportion), color="orange", alpha=0.03) + 
  geom_smooth(data=gene_density, aes(x=start, y=proportion), color="blue") +
  geom_smooth(data=repeat_density, aes(x=start, y=proportion), color="orange") +
  theme_bw() + 
  xlab("Position in chromosome (Mb)") + ylab("Proportion") + 
  scale_x_continuous(labels=function(x)x/1e6, expand=c(0.005,0)) + scale_y_continuous(expand=c(0.01,0)) + coord_cartesian(ylim=c(0, 0.6)) +
  facet_grid(~chr, scales="free_x") + theme(text = element_text(size=12), strip.text.y = element_text(angle = 0), strip.text.x = element_text(size=14)) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


# synteny plots
synteny_plot <- read.table("CELEG_CBRIG.with_proportions.txt", col.names=c("sp1", "sp1_seq", "sp1_chr", "sp1_start", "sp1_stop", "sp1_ID", "sp2", "sp2_seq", "sp2_chr", "sp2_start", "sp2_stop", "sp2_ID", "sp1_prop", "sp2_prop"))

p3 <- synteny_plot %>% 
  filter(sp1_chr == sp2_chr) %>%
  ggplot(.) + 
  geom_segment(aes(x=sp1_prop, xend=sp2_prop, y=0, yend=10, color=sp1_chr), alpha=0.1) + 
  scale_x_continuous(expand=c(0.005,0), limits = c(0, NA)) + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=NULL, expand=c(0,0)) +
  facet_grid(~sp1_chr, scales="free_x") + theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), legend.position = "none") + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), strip.text.x = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

p <- p1 / p3 / p2 
ggsave("repeat_genes_synteny.pdf", p, width=16, height=9)
ggsave("repeat_genes_synteny.png", p, width=16, height=9)