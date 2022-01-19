library(ggplot2)
library(tidyverse)
library(gridExtra)

# p1
table <- read.table("AF16_vs_QX1410.coords_orientation", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2", "ORIENTATION"))
table$CHROM2 <- factor(table$CHROM2, levels = c("X", "V", "IV", "III", "II", "I"))
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
  theme(strip.text = element_text(size = 20)) +
  theme(axis.title=element_text(size=16, face="bold")) + 
  theme(strip.background = element_rect(fill="white", colour="white"))+
  scale_x_continuous(labels=function(x)x/1e6, expand = c(0, 0)) + 
  scale_y_continuous(labels=function(x)x/1e6, expand = c(0, 0)) +
  xlab(expression(~italic("C. briggsae")~'QX1410')) + ylab(expression(~italic("C. briggsae")~'AF16 (PRJNA10731)'))

# p2
table <- read.table("VX34_vs_QX1410.coords_orientation", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2", "ORIENTATION"))
table$CHROM2 <- factor(table$CHROM2, levels = c("X", "V", "IV", "III", "II", "I"))
p2 <- table %>% filter(LEN1 > 1e3)  %>%√v
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
  theme(strip.text = element_text(size = 20)) +
  theme(axis.title=element_text(size=16, face="bold")) + 
  theme(strip.background = element_rect(fill="white", colour="white"))+
  scale_x_continuous(labels=function(x)x/1e6, expand = c(0, 0)) + 
  scale_y_continuous(labels=function(x)x/1e6, expand = c(0, 0)) +
  xlab(expression(~italic("C. briggsae")~'QX1410')) + ylab(expression(~italic("C. briggsae")~'VX34'))

p <- grid.arrange(p2, p1, nrow = 1)

ggsave("VX34_AF16_vs_QX1410_dotplots.pdf", plot = p, width = 14, height = 7, units = "in")
ggsave("VX34_AF16_vs_QX1410_dotplots.png", plot = p, width = 14, height = 7, units = "in")

# p1
table <- read.table("AF16_vs_QX1410.coords_orientation", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2", "ORIENTATION"))
table$CHROM2 <- factor(table$CHROM2, levels = c("X", "V", "IV", "III", "II", "I"))
p1 <- table %>% filter(LEN1 > 1e3) %>% filter(CHROM1 == "IV") %>% filter(CHROM2 == "IV") %>%
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
  theme(strip.text = element_text(size = 20)) +
  theme(axis.title=element_text(size=16, face="bold")) +
  theme(strip.background = element_rect(fill="white", colour="white"))+
  scale_x_continuous(labels=function(x)x/1e6, expand = c(0, 0)) +
  scale_y_continuous(labels=function(x)x/1e6, expand = c(0, 0)) +
  xlab(expression(~italic("C. briggsae")~'QX1410')) + ylab(expression(~italic("C. briggsae")~'AF16 (PRJNA10731)'))

# p2
table <- read.table("VX34_vs_QX1410.coords_orientation", col.names = c("S1", "E1", "S2", "E2", "LEN1", "LEN2", "IDY", "LENR", "LENQ", "CHROM1", "CHROM2", "ORIENTATION"))
table$CHROM2 <- factor(table$CHROM2, levels = c("X", "V", "IV", "III", "II", "I"))
p2 <- table %>% filter(LEN1 > 1e3)  %>% filter(CHROM1 == "IV") %>% filter(CHROM2 == "IV") %>%
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
  theme(strip.text = element_text(size = 20)) +
  theme(axis.title=element_text(size=16, face="bold")) +
  theme(strip.background = element_rect(fill="white", colour="white"))+
  scale_x_continuous(labels=function(x)x/1e6, expand = c(0, 0)) +
  scale_y_continuous(labels=function(x)x/1e6, expand = c(0, 0)) +
  xlab(expression(~italic("C. briggsae")~'QX1410')) + ylab(expression(~italic("C. briggsae")~'VX34'))

p <- grid.arrange(p2, p1, nrow = 1)

ggsave("VX34_AF16_vs_QX1410_dotplots.IV.pdf", plot = p, width = 14, height = 7, units = "in")
ggsave("VX34_AF16_vs_QX1410_dotplots.IV.png", plot = p, width = 14, height = 7, units = "in")
