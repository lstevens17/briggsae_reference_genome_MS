library(ggplot2)
library(tidyverse)
library(gridExtra)

CB4586_vs_N2_coords <- read.table("CB4586_vs_N2.coords", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2"))
XZ1516_vs_N2_coords <- read.table("XZ1516_vs_N2.coords", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2"))
AF16_vs_QX1410_coords <- read.table("AF16_vs_QX1410.coords_orientation", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2", "ORIENTATION"))
VX34_vs_QX1410_coords <- read.table("VX34_vs_QX1410.coords_orientation", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2", "ORIENTATION"))


p1 <- CB4586_vs_N2_coords %>% filter(LEN1 > 1e3) %>% filter(CHROM1 != "MtDNA") %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) + 
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(85,100.2) + 
  ylab("Nucleotide identity (%)") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  scale_x_continuous(labels=function(x)x/1e6) + 
  ggtitle("CB4856 vs N2")

p2 <- XZ1516_vs_N2_coords %>% filter(LEN1 > 1e3) %>% filter(CHROM1 != "MtDNA") %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) + 
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(85,100.2) + 
  scale_x_continuous(labels=function(x)x/1e6) + 
  ylab("Nucleotide identity (%)") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  xlab("Position in chromosome (Mb)") + 
  ggtitle("XZ1516 vs N2")

p3 <- AF16_vs_QX1410_coords %>% filter(LEN1 > 1e3) %>% filter(CHROM1 != "MtDNA") %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) + 
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(85,100.2) + 
  scale_x_continuous(labels=function(x)x/1e6) + 
  ylab("Nucleotide identity (%)") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  xlab("Position in chromosome (Mb)") + 
  ggtitle("AF16 vs QX1410")

p4 <- VX34_vs_QX1410_coords %>% filter(LEN1 > 1e3) %>% filter(CHROM1 != "MtDNA") %>%
  ggplot(aes(xmin=S1, xmax=E1, ymax=IDY+0.2,, ymin=IDY-0.2)) + 
  geom_rect(fill="black") +
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(~CHROM1, scales="free_x") + 
  theme_bw() + ylim(85,100.2) + 
  scale_x_continuous(labels=function(x)x/1e6) + 
  ylab("Nucleotide identity (%)") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  xlab("Position in chromosome (Mb)") + 
  ggtitle("VX34 vs QX1410")
  


p <- grid.arrange(p1, p2, p3, p4, nrow = 2)

#ggsave("AF16_and_VX34_vs_QX1410_nucmer_coords.pdf", plot = p, width = 14, height = 6, units = "in")
#ggsave("AF16_and_VX34_vs_QX1410_nucmer_coords.png", plot = p, width = 14, height = 6, units = "in")