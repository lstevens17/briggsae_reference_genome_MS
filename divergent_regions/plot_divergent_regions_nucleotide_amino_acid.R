library(ggplot2)
library(tidyverse)
library(gridExtra)
library(patchwork)

table <- read.table("AF16_vs_QX1410.gene_start_chr_id.txt", col.names = c("gene", "start", "chr", "id"))

p1 <- table %>% 
  ggplot(aes(x=start, y=id)) + 
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  ylab("Amino acid identity (%)") + xlab("Position in chromosome") + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(labels=function(x)x/1e6)

AF16_vs_QX1410_coords <- read.table("AF16_vs_QX1410.coords_orientation", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2", "ORIENTATION"))

p2 <- AF16_vs_QX1410_coords %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) + 
  #geom_rect(aes(xmin=0, xmax=5e6, ymax=100,, ymin=90), fill="#e8e8e8") +
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6) +
  labs(title = "A", subtitle="AF16 vs QX1410")


table <- read.table("QX1410_vs_VX34.gene_start_chr_id.txt", col.names = c("gene", "start", "chr", "id"))

p3 <- table %>% 
  ggplot(aes(x=start, y=id)) + 
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  ylab("Amino acid identity (%)") + xlab("Position in chromosome") + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(labels=function(x)x/1e6)

AF16_vs_QX1410_coords <- read.table("VX34_vs_QX1410.coords_orientation", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2", "ORIENTATION"))

p4 <- AF16_vs_QX1410_coords %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) + 
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6) +
  labs(title = "B", subtitle="VX34 vs QX1410")


p <- p2 / p1 / p4 / p3

p

#ggsave("divergent_regions_nucleotide_amino_acid.pdf", plot = p, width = 14, height = 11, units = "in")
#ggsave("divergent_regions_nucleotide_amino_acid.png", plot = p, width = 14, height = 11, units = "in")


