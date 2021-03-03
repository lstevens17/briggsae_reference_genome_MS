library(ggplot2)
library(tidyverse)
library(gridExtra)

# QX1410
telomeric_repeats <- read.table("telomere_caenorhabditis_briggsae_QX1410_v2.1_curated.noMtDNA.scaffolds.fa.1kw.tsv", header=TRUE, col.names = c("chr", "start", "end", "count", "feature"))
p1 <- telomeric_repeats %>% filter(chr %in% c("I", "II", "III", "IV", "V", "X")) %>% ggplot(data=., aes(x=start, y=count)) + 
  geom_line() + 
  facet_grid(~chr, scales="free") + 
  theme_bw() + 
  ylim(0, 90) + 
  theme(axis.title.x=element_blank()) + 
  ylab("Count") + 
  ggtitle(expression(~italic("C. briggsae")~'QX1410')) + 
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) + 
  scale_x_continuous(labels=function(x)x/1e6)

# VX34
telomeric_repeats <- read.table("telomere_caenorhabditis_briggsae_VX34_v2.1_curated.noMtDNA.scaffolds.fa.1kw.tsv", header=TRUE, col.names = c("chr", "start", "end", "count", "feature"))
p2 <- telomeric_repeats %>% filter(chr %in% c("I", "II", "III", "IV", "V", "X")) %>% ggplot(data=., aes(x=start, y=count)) + 
  geom_line() + 
  facet_grid(~chr, scales="free") + 
  theme_bw() + 
  ylim(0, 90) + 
  theme(axis.title.x=element_blank()) + 
  ylab("Count") + 
  ggtitle(expression(~italic("C. briggsae")~'VX34')) + 
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) + 
  scale_x_continuous(labels=function(x)x/1e6)

# AF16
telomeric_repeats <- read.table("telomere_c_briggsae.PRJNA10731.WS280.genomic.fa.1kw.tsv", header=TRUE, col.names = c("chr", "start", "end", "count", "feature"))
p3 <- telomeric_repeats %>% filter(chr %in% c("I", "II", "III", "IV", "V", "X")) %>% ggplot(data=., aes(x=start, y=count)) + 
  geom_line() + 
  facet_grid(~chr, scales="free") + 
  theme_bw() + 
  ylim(0, 90) + 
  theme(axis.title.x=element_blank()) +
  ylab("Count") +
  ggtitle(expression(~italic("C. briggsae")~'AF16 (PRJNA10731)')) + 
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) + 
  scale_x_continuous(labels=function(x)x/1e6)

# N2 
telomeric_repeats <- read.table("telomere_c_elegans.PRJNA13758.WS280.genomic.fa.1kw.tsv", header=TRUE, col.names = c("chr", "start", "end", "count", "feature"))
p4 <- telomeric_repeats %>% filter(chr %in% c("I", "II", "III", "IV", "V", "X")) %>% ggplot(data=., aes(x=start, y=count)) + 
  geom_line() + 
  facet_grid(~chr, scales="free") + 
  theme_bw() + 
  ylim(0, 90) + 
  xlab("Genome position (Mb)") + 
  ylab("Count") +
  ggtitle(expression(~italic("C. elegans")~'N2 (PRJNA13758)')) + 
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) + 
  scale_x_continuous(labels=function(x)x/1e6)

# plot
p <- grid.arrange(p1, p2, p3, p4, nrow = 4)
ggsave("telomeric_repeat_distribution.pdf", plot = p, width = 14, height = 11, units = "in")
ggsave("telomeric_repeat_distribution.png", plot = p, width = 14, height = 11, units = "in")
