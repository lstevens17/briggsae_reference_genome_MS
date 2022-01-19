library(ggplot2)
library(tidyverse)
library(patchwork)

##############
### QX1410 ###
##############
mer14_density <- read.table("QX1410_14mer_density.tsv", col.names=c("chr", "start", "stop", "features", "span", "window_size", "proportion")) %>% mutate(pos=(start+stop)/2)
mer19_density <- read.table("QX1410_19mer_density.tsv", col.names=c("chr", "start", "stop", "features", "span", "window_size", "proportion")) %>% mutate(pos=(start+stop)/2)

IL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="I"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="I"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4))  + ggtitle("IL")

IR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="I"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="I"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(15540809-5e4, 15540809)) + ggtitle("IR")

IIL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="II"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="II"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4))  + ggtitle("IIL")

IIR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="II"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="II"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(16595099-5e4, 16595099)) + ggtitle("IIR")

IIIL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="III"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="III"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4))  + ggtitle("IIIL")

IIIR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="III"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="III"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(14810976-5e4, 14810976)) + ggtitle("IIIR")

IVL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="IV"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="IV"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4)) + ggtitle("IVL")

IVR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="IV"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="IV"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(17080301-5e4, 17080301)) + ggtitle("IVR")

VL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="V"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="V"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4)) + ggtitle("VL")

VR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="V"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="V"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(19933398-5e4, 19933398)) + ggtitle("VR")

XL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="X"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="X"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4))  + ggtitle("XL")

XR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="X"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="X"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(22220885-5e4, 22220885)) + ggtitle("XR")

p1 <- IL + IR + IIL + IIR + IIIL + IIIR + IVL + IVR + VL + VR + XL + XR + plot_layout(nrow = 2)

############
### VX34 ###
############
mer14_density <- read.table("VX34_14mer_density.tsv", col.names=c("chr", "start", "stop", "features", "span", "window_size", "proportion")) %>% mutate(pos=(start+stop)/2)
mer19_density <- read.table("VX34_19mer_density.tsv", col.names=c("chr", "start", "stop", "features", "span", "window_size", "proportion")) %>% mutate(pos=(start+stop)/2)

IL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="I"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="I"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4)) + ggtitle("IL")

IR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="I"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="I"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(15585124-5e4, 15585124)) + ggtitle("IR")

IIL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="II"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="II"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4)) + ggtitle("IIL")

IIR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="II"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="II"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(16696426-5e4, 16696426)) + ggtitle("IIR")

IIIL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="III"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="III"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4)) + ggtitle("IIIL")

IIIR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="III"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="III"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(14818584-5e4, 14818584)) + ggtitle("IIIR")

IVL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="IV"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="IV"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4)) + ggtitle("IVL")

IVR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="IV"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="IV"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(17304409-5e4, 17304409)) + ggtitle("IVR")

VL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="V"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="V"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4)) + ggtitle("VL")

VR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="V"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="V"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(19997254-5e4, 19997254)) + ggtitle("VR")

XL <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="X"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="X"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(0, 5e4)) + ggtitle("XL")

XR <- ggplot() + 
  geom_col(data=subset(mer14_density, chr=="X"), aes(x=pos, y=features), color="blue", size=1) +
  geom_col(data=subset(mer19_density, chr=="X"), aes(x=pos, y=features), color="orange", size=1) +
  ylim(0, 30) + theme_bw() + theme(axis.title = element_blank())  + scale_x_continuous(labels=function(x)x/1e6, limits=c(22536999-5e4, 22536999)) + ggtitle("XR")

p2 <- IL + IR + IIL + IIR + IIIL + IIIR + IVL + IVR + VL + VR + XL + XR + plot_layout(nrow = 2)

p <- p1 / p2

ggsave("both_strains_subtelomeric_repeat.pdf", p, width=16, height=9)
ggsave("both_strains_subtelomeric_repeat.png", p, width=16, height=9)