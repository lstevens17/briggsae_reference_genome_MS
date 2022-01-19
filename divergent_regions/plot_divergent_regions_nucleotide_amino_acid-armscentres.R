library(ggplot2)
library(tidyverse)
library(gridExtra)
library(patchwork)


## AF16 amino acids 
table <- read.table("AF16_vs_QX1410.gene_start_chr_id.txt", col.names = c("gene", "start", "chr", "id"))

I <- table %>% filter(chr == "I") %>%
  ggplot(aes(x=start, y=id)) + 
  geom_rect(xmin=0, xmax=3191131, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=11208076, xmax=15540809, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  ylab("Amino acid identity (%)") + xlab("Position in chromosome") + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") +
  theme(panel.grid.minor = element_blank(), axis.title.x=element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(labels=function(x)x/1e6)

II <- table %>% filter(chr == "II") %>%
  ggplot(aes(x=start, y=id)) + 
  geom_rect(xmin=0, xmax=3409913, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=13987718, xmax=16595099, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") +
  theme(panel.grid.minor = element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(labels=function(x)x/1e6)

III <- table %>% filter(chr == "III") %>%
  ggplot(aes(x=start, y=id)) + 
  geom_rect(xmin=0, xmax=3964862, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=11824006, xmax=14810976, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  ylab("Amino acid identity (%)") + xlab("Position in chromosome") + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") +
  theme(panel.grid.minor = element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(labels=function(x)x/1e6)

IV <- table %>% filter(chr == "IV") %>%
  ggplot(aes(x=start, y=id)) + 
  geom_rect(xmin=0, xmax=4248176, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=13442121, xmax=17080301, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  ylab("Amino acid identity (%)") + xlab("Position in chromosome") + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") +
  theme(panel.grid.minor = element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank()) +
  scale_x_continuous(labels=function(x)x/1e6)

V <- table %>% filter(chr == "V") %>%
  ggplot(aes(x=start, y=id)) + 
  geom_rect(xmin=0, xmax=6061896, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=15495394, xmax=19933398, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  ylab("Amino acid identity (%)") + xlab("Position in chromosome") + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") +
  theme(panel.grid.minor = element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(labels=function(x)x/1e6)

X <- table %>% filter(chr == "X") %>%
  ggplot(aes(x=start, y=id)) + 
  geom_rect(xmin=0, xmax=7065899, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=18521883, xmax=22220885, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  ylab("Amino acid identity (%)") + xlab("Position in chromosome") + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") +
  theme(panel.grid.minor = element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(labels=function(x)x/1e6)

AF16_aa <- I + II + III + IV + V + X + plot_layout(nrow = 1)

## AF16 nucleotide
AF16_vs_QX1410_coords <- read.table("AF16_vs_QX1410.coords_orientation", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2", "ORIENTATION"))

I <- AF16_vs_QX1410_coords %>% filter(CHROM1 == "I") %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) + 
  geom_rect(xmin=0, xmax=3191131, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=11208076, xmax=15540809, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6)

II <- AF16_vs_QX1410_coords %>% filter(CHROM1 == "II") %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) + 
  geom_rect(xmin=0, xmax=3409913, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=13987718, xmax=16595099, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6) 

III <- AF16_vs_QX1410_coords %>% filter(CHROM1 == "III") %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) +
  geom_rect(xmin=0, xmax=3964862, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=11824006, xmax=14810976, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6)

IV <- AF16_vs_QX1410_coords %>% filter(CHROM1 == "IV") %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) +
  geom_rect(xmin=0, xmax=4248176, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=13442121, xmax=17080301, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6)

V <- AF16_vs_QX1410_coords %>% filter(CHROM1 == "V") %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) +
  geom_rect(xmin=0, xmax=6061896, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=15495394, xmax=19933398, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6)

X <- AF16_vs_QX1410_coords %>% filter(CHROM1 == "X") %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) + 
  geom_rect(xmin=0, xmax=7065899, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=18521883, xmax=22220885, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6)

AF16_nt <- I + II + III + IV + V + X + plot_layout(nrow = 1)

## VX34 amino acids 
table <- read.table("QX1410_vs_VX34.gene_start_chr_id.txt", col.names = c("gene", "start", "chr", "id"))

I <- table %>% filter(chr == "I") %>%
  ggplot(aes(x=start, y=id)) + 
  geom_rect(xmin=0, xmax=3191131, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=11208076, xmax=15540809, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  ylab("Amino acid identity (%)") + xlab("Position in chromosome") + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") +
  theme(panel.grid.minor = element_blank(), axis.title.x=element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(labels=function(x)x/1e6)

II <- table %>% filter(chr == "II") %>%
  ggplot(aes(x=start, y=id)) + 
  geom_rect(xmin=0, xmax=3409913, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=13987718, xmax=16595099, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") +
  theme(panel.grid.minor = element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(labels=function(x)x/1e6)

III <- table %>% filter(chr == "III") %>%
  ggplot(aes(x=start, y=id)) + 
  geom_rect(xmin=0, xmax=3964862, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=11824006, xmax=14810976, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  ylab("Amino acid identity (%)") + xlab("Position in chromosome") + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") +
  theme(panel.grid.minor = element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(labels=function(x)x/1e6)

IV <- table %>% filter(chr == "IV") %>%
  ggplot(aes(x=start, y=id)) + 
  geom_rect(xmin=0, xmax=4248176, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=13442121, xmax=17080301, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  ylab("Amino acid identity (%)") + xlab("Position in chromosome") + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") +
  theme(panel.grid.minor = element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank()) +
  scale_x_continuous(labels=function(x)x/1e6)

V <- table %>% filter(chr == "V") %>%
  ggplot(aes(x=start, y=id)) + 
  geom_rect(xmin=0, xmax=6061896, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=15495394, xmax=19933398, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  ylab("Amino acid identity (%)") + xlab("Position in chromosome") + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") +
  theme(panel.grid.minor = element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(labels=function(x)x/1e6)

X <- table %>% filter(chr == "X") %>%
  ggplot(aes(x=start, y=id)) + 
  geom_rect(xmin=0, xmax=7065899, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=18521883, xmax=22220885, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_point(alpha=0.1, size=0.5) + ylim(90,100) + theme_bw() + 
  ylab("Amino acid identity (%)") + xlab("Position in chromosome") + 
  geom_smooth(method = "loess", span = 0.2, color="black") + 
  facet_grid(~chr, scales="free_x") +
  theme(panel.grid.minor = element_blank(), axis.title.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank()) + 
  scale_x_continuous(labels=function(x)x/1e6)

VX34_aa <- I + II + III + IV + V + X + plot_layout(nrow = 1)

## VX34 nucleotide
VX34_vs_QX1410_coords <- read.table("VX34_vs_QX1410.coords_orientation", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2", "ORIENTATION"))

I <- VX34_vs_QX1410_coords %>% filter(CHROM1 == "I") %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) + 
  geom_rect(xmin=0, xmax=3191131, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=11208076, xmax=15540809, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6)

II <- VX34_vs_QX1410_coords %>% filter(CHROM1 == "II") %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) + 
  geom_rect(xmin=0, xmax=3409913, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=13987718, xmax=16595099, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6) 

III <- VX34_vs_QX1410_coords %>% filter(CHROM1 == "III") %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) +
  geom_rect(xmin=0, xmax=3964862, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=11824006, xmax=14810976, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6)

IV <- VX34_vs_QX1410_coords %>% filter(CHROM1 == "IV") %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) +
  geom_rect(xmin=0, xmax=4248176, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=13442121, xmax=17080301, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6)

V <- VX34_vs_QX1410_coords %>% filter(CHROM1 == "V") %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) +
  geom_rect(xmin=0, xmax=6061896, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=15495394, xmax=19933398, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6)

X <- VX34_vs_QX1410_coords %>% filter(CHROM1 == "X") %>% filter(LEN1 > 1e3) %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) + 
  geom_rect(xmin=0, xmax=7065899, ymin=90, ymax=100, fill="#e3e3e3") + 
  geom_rect(xmin=18521883, xmax=22220885, ymin=90, ymax=100, fill="#e3e3e3") +
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(90,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12)) +
  scale_x_continuous(labels=function(x)x/1e6)

VX34_nt <- I + II + III + IV + V + X + plot_layout(nrow = 1)

p <- AF16_nt / AF16_aa / VX34_nt / VX34_aa

ggsave("divergent_regions_nucleotide_amino_acid-armscentres.pdf", plot=p, width = 14, height = 11, units = "in")
ggsave("divergent_regions_nucleotide_amino_acid-armscentres.png", plot=p, width = 14, height = 11, units = "in")