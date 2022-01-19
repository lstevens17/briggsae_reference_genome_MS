library(ggplot2)
library(tidyverse)
library(forcats)
library(patchwork)

# AF16
arms_centres <- read.table("AF16_arms_centres.tsv", col.names=c("chr", "start", "stop", "count", "span", "size", "dens"))

arms_centres$label <- rep(c("left arm","centre","right arm"),times=6)

p1 <- arms_centres %>%
  ggplot(aes(x=fct_inorder(label), y=dens)) +
  geom_col() + 
  facet_grid(~chr) + 
  xlab("Domain") + ylab("SNP density") + 
  theme_bw() + 
  ylim(0,0.022) + 
  labs(title = "A", subtitle="AF16 vs QX1410") + 
  theme(plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12))

# VX34
arms_centres <- read.table("VX34_arms_centres.tsv", col.names=c("chr", "start", "stop", "count", "span", "size", "dens"))

arms_centres$label <- rep(c("left arm","centre","right arm"),times=6) 

p2 <- arms_centres %>%
  ggplot(aes(x=fct_inorder(label), y=dens)) +
  geom_col() + 
  facet_grid(~chr) + 
  xlab("Domain") + ylab("SNP density") + 
  theme_bw() + 
  ylim(0,0.022) + 
  labs(title = "B", subtitle="VX34 vs QX1410") + 
  theme(plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12))

# CB4856
arms_centres <- read.table("CB4856_arms_centres.tsv", col.names=c("chr", "start", "stop", "count", "span", "size", "dens"))

arms_centres$label <- rep(c("left arm","centre","right arm"),times=6)

p3 <- arms_centres %>%
  ggplot(aes(x=fct_inorder(label), y=dens)) +
  geom_col() + 
  facet_grid(~chr) + 
  xlab("Domain") + ylab("SNP density") + 
  theme_bw() + 
  ylim(0,0.022) + 
  labs(title = "C", subtitle="CB4856 vs N2") + 
  theme(plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12))

# XZ1516
arms_centres <- read.table("XZ1516_arms_centres.tsv", col.names=c("chr", "start", "stop", "count", "span", "size", "dens"))

arms_centres$label <- rep(c("left arm","centre","right arm"),times=6)

p4 <- arms_centres %>%
  ggplot(aes(x=fct_inorder(label), y=dens)) +
  geom_col() + 
  facet_grid(~chr) + 
  xlab("Domain") + ylab("SNP density") + 
  theme_bw() + 
  ylim(0,0.022) + 
  labs(title = "D", subtitle="XZ1516 vs N2") + 
  theme(plot.title = element_text(face = "bold", hjust = -0.05, size = 18, margin = margin(t = -10, b = 0)), plot.subtitle = element_text(size = 12))

p <- p1 / p2 / p3 / p4
ggsave("SNP_density_by_domain.png", plot = p, width = 10, height = 10, units = "in")
ggsave("SNP_density_by_domain.pdf", plot = p, width = 10, height = 10, units = "in")