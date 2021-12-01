#!/usr/bin/env Rscript
library(ggplot2)
library(tidyr)
library(dplyr)

# get args
args = commandArgs(trailingOnly=TRUE)

# sp1
sp1.synteny <- read.table(args[1])
colnames(sp1.synteny) <- c("chromosome", "syn", "break", "perc")
sp1.synteny$chromosome <- factor(sp1.synteny$chromosome)
sp1.synteny.df <- gather(sp1.synteny, X, prop, perc, factor_key=TRUE)
sp1.total <- filter(sp1.synteny.df, chromosome == "Total")
sp1.total <- sp1.total$prop
sp1.synteny.df <- filter(sp1.synteny.df, chromosome != "Total")
sp1.synteny.df$reproductive_mode <- (rep("selfer", times=6))

# sp2
sp2.synteny <- read.table(args[2])
colnames(sp2.synteny) <- c("chromosome", "syn", "break", "perc")
sp2.synteny$chromosome <- factor(sp2.synteny$chromosome)
sp2.synteny.df <- gather(sp2.synteny, X, prop, perc, factor_key=TRUE)
sp2.total <- filter(sp2.synteny.df, chromosome == "Total")
sp2.total <- sp2.total$prop
sp2.synteny.df <- filter(sp2.synteny.df, chromosome != "Total")
sp2.synteny.df$reproductive_mode <- (rep("outcrosser", times=6))

# join
df <- rbind(sp1.synteny.df, sp2.synteny.df)

p <- ggplot(data=df, aes(x=chromosome, y=prop, color=reproductive_mode)) + 
  geom_line(color="black") + 
  geom_point(size=4) +
  #annotate("text", label = paste0(sp1.total, "%"), x = 2.1, y = 100, size = 8, colour = "#0092c1") +
  #annotate("text", label = paste0(sp2.total, "%"), x = 1, y = 100, size = 8, colour = "#ff9d00") + 
  theme_bw() + 
  scale_color_manual(values = c("#ff8800", "#0092c1")) + 
  xlab("Chromosome") + ylab("% gene pairs syntenic") + labs(color = "Reproductive mode") + 
  theme(legend.position = c(0.15, 0.87), 
        legend.background = element_rect(size=0.3, linetype="solid", colour ="black"), 
        legend.title = element_text(size = 14, face="bold"),
        legend.text = element_text(size = 12), 
        axis.title = element_text(size=14, face="bold"), 
        axis.text = element_text(size=12))

ggsave(args[3], p, width=9.00, height=5.73)
