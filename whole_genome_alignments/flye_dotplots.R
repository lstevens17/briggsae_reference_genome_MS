library(ggplot2)
library(tidyverse)
library(gridExtra)

# p1
table <- read.table("QX1410_flye_vs_v2ref.coords_orientation", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2", "ORIENTATION"))
table$CHROM2 <- factor(table$CHROM2, levels = c("contig_27", "contig_23", "contig_17", "contig_12", "contig_32", "contig_2", "scaffold_24", "scaffold_11", "contig_29", "contig_3", "scaffold_14", "scaffold_13"))
p1 <- table %>% filter(LEN1 > 1e3) %>%
  ggplot(.) + 
  geom_segment(aes(x=S1, xend=E1, y=S2, yend=E2, color=ORIENTATION), size=1) + 
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(CHROM2 ~ CHROM1, scales="free", space="free", switch="both") + 
  theme_bw() + 
  theme(legend.position = "none") + 
  theme(panel.border = element_rect(color = "#999999")) + 
  theme(panel.spacing = unit(0, "lines")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(strip.text.y = element_text(size = 8), strip.text.y.left = element_text(angle = 0), strip.text.x = element_text(size = 16)) +
  theme(axis.title=element_text(size=16, face="bold")) + 
  theme(strip.background = element_rect(fill="white", colour="white"))+
  scale_x_continuous(labels=function(x)x/1e6, expand = c(0, 0)) + 
  scale_y_continuous(labels=function(x)x/1e6, expand = c(0, 0)) +
  xlab("QX1410 scaffolded reference genome") + ylab("QX1410 original Flye assembly")

# p2
table <- read.table("VX34_flye_vs_v2ref.coords_orientation", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2", "ORIENTATION"))
table$CHROM2 <- factor(table$CHROM2, levels = c("contig_3", "scaffold_10", "contig_16", "contig_7", "contig_4", "scaffold_44", "contig_6", "contig_24"))
p2 <- table %>% filter(LEN1 > 1e3) %>%
  ggplot(.) + 
  geom_segment(aes(x=S1, xend=E1, y=S2, yend=E2, color=ORIENTATION), size=1) + 
  scale_colour_manual(values=c("black", "red")) + 
  facet_grid(CHROM2 ~ CHROM1, scales="free", space="free", switch="both") + 
  theme_bw() + 
  theme(legend.position = "none") + 
  theme(panel.border = element_rect(color = "#999999")) + 
  theme(panel.spacing = unit(0, "lines")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(strip.text.y = element_text(size = 8), strip.text.y.left = element_text(angle = 0), strip.text.x = element_text(size = 16)) +
  theme(axis.title=element_text(size=16, face="bold")) + 
  theme(strip.background = element_rect(fill="white", colour="white"))+
  scale_x_continuous(labels=function(x)x/1e6, expand = c(0, 0)) + 
  scale_y_continuous(labels=function(x)x/1e6, expand = c(0, 0)) +
  xlab("VX34 scaffolded reference genome") + ylab("VX34 Flye assembly")

p <- grid.arrange(p1, p2, nrow = 1)

ggsave("flye_vs_v2ref_dotplot.pdf", plot = p, width = 14, height = 7, units = "in")
ggsave("flye_vs_v2ref_dotplot.png", plot = p, width = 14, height = 7, units = "in")