rm(list=ls())
library(tidyverse)
library(showtext)
library(extrafont)
library(cowplot)
library(ggpubr)
library(here)

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

data_x2 <- read.csv(here("X2_24_04_12.csv")) 
head(data_x2)

data_x2$Value_200nM_C14_Gi1 <- as.numeric(gsub("%", "", data_x2$Value_200nM_C14_Gi1)) 
data_x2$Value_200nM_C14_Gi1 <-  data_x2$Value_200nM_C14_Gi1/100

data_x2$Value_200nM_C14_Gq <- as.numeric(gsub("%", "", data_x2$Value_200nM_C14_Gq))
data_x2$Value_200nM_C14_Gq <- data_x2$Value_200nM_C14_Gq/100

data_x2$Value_200nM_C14_miniGi <- as.numeric(gsub("%", "", data_x2$Value_200nM_C14_miniGi))
data_x2$Value_200nM_C14_miniGi <- data_x2$Value_200nM_C14_miniGi/100

data_x2$Value_200nM_C14_miniGq <- as.numeric(gsub("%", "", data_x2$Value_200nM_C14_miniGq))
data_x2$Value_200nM_C14_miniGq <- data_x2$Value_200nM_C14_miniGq/100

head(data_x2)

data_x2 <- data_x2 |> 
  select(-1)

data_plot <- data_x2[, c(1, 5, 8, 11)] |> 
  pivot_longer(cols = -compound, names_to = "treatment", values_to = "Value")

data_plot

ggplot(data = data_plot, 
       mapping = aes(x = compound, y = Value)) +
  geom_point(size= 3) +
  labs(title = "GPCR", x= NULL, y = "BRET % of the response") +
  ylim(0.4, 1.2) +
  #geom_hline(yintercept = c(1.2, 0.8, 0.6), linetype = "dashed", color = "black", size=1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size=1.5) +
  
  expand_limits(x = c(-1, 101)) +      #2侧的点收缩，不至于到方框里
  #coord_flip() +
  #theme(panel.background = element_rect(fill = "white")) +   #背景是白色
  theme(axis.title.x = element_text(size = 18, color = "black",family = "Arial")) + 
  # theme(axis.text.x = element_text(family = "Roboto Condensed", size = 11, color = "black", face = "bold", angle = 60, hjust = 1))+
  theme(axis.text.x = element_blank()) +
  theme(axis.title.y = element_text(size = 20, color ="black")) +
  theme(axis.text.y.left = element_text(size = 12, color = "black", family = "Arial")) +
  # theme(panel.border = element_rect(color = "black", fill = NA, size = 2)) + #加方框
  theme(plot.margin = margin(5,10, 5,10)) + #整个图片的留白
  theme(plot.title = element_text(family = "Arial", hjust = 0.5, face = "bold", size = 22, color = "black")) +
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 10, color = "black",family = "Arial")) +
  facet_wrap(~treatment, nrow = 3)


