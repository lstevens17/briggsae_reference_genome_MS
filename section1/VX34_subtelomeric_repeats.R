library(ggplot2)
library(tidyverse)
library(patchwork)

# VX34
mer14_density <- read.table("VX34_14mer_density.tsv", col.names=c("chr", "start", "stop", "features", "span", "window_size", "proportion")) %>% mutate(pos=(start+stop)/2)
mer19_density <- read.table("VX34_19mer_density.tsv", col.names=c("chr", "start", "stop", "features", "span", "window_size", "proportion")) %>% mutate(pos=(start+stop)/2)

IL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="I"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="I"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + xlab("Position (Mb)") + ylab("Counts of kmers\nper 1kb window") + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4))  + ggtitle("IL")

IR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="I"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="I"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + xlab("Position (Mb)") + ylab("Counts of kmers\nper 1kb window") + scale_x_continuous(labels=function(x)x/1e6, limits=c(15585124-5e4, 15585124))  + ggtitle("IR")

IIL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="II"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="II"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + xlab("Position (Mb)") + ylab("Counts of kmers\nper 1kb window") + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4))  + ggtitle("IIL")

IIR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="II"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="II"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + xlab("Position (Mb)") + ylab("Counts of kmers\nper 1kb window") + scale_x_continuous(labels=function(x)x/1e6, limits=c(16696426-5e4, 16696426))  + ggtitle("IIR")

IIIL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="III"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="III"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + xlab("Position (Mb)") + ylab("Counts of kmers\nper 1kb window") + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4))  + ggtitle("IIIL")

IIIR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="III"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="III"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + xlab("Position (Mb)") + ylab("Counts of kmers\nper 1kb window") + scale_x_continuous(labels=function(x)x/1e6, limits=c(14818584-5e4, 14818584))  + ggtitle("IIIR")

IVL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="IV"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="IV"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + xlab("Position (Mb)") + ylab("Counts of kmers\nper 1kb window") + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4))  + ggtitle("IVL")

IVR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="IV"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="IV"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + xlab("Position (Mb)") + ylab("Counts of kmers\nper 1kb window") + scale_x_continuous(labels=function(x)x/1e6, limits=c(17304409-5e4, 17304409))  + ggtitle("IVR")

VL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="V"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="V"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + xlab("Position (Mb)") + ylab("Counts of kmers\nper 1kb window") + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4))  + ggtitle("VL")

VR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="V"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="V"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + xlab("Position (Mb)") + ylab("Counts of kmers\nper 1kb window") + scale_x_continuous(labels=function(x)x/1e6, limits=c(19997254-5e4, 19997254))  + ggtitle("VR")

XL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="X"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="X"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + xlab("Position (Mb)") + ylab("Counts of kmers\nper 1kb window") + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4))  + ggtitle("XL")

XR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="X"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="X"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + xlab("Position (Mb)") + ylab("Counts of kmers\nper 1kb window") + scale_x_continuous(labels=function(x)x/1e6, limits=c(22536999-5e4, 22536999))  + ggtitle("XR")

p1 <- IL + IR + IIL + IIR + IIIL + IIIR + IVL + IVR + VL + VR + XL + XR + plot_layout(nrow = 2)

ggsave("VX34_subtelomeric_repeat.pdf", p1, width=16, height=4.5)
ggsave("VX34_subtelomeric_repeat.png", p1, width=16, height=4.5)