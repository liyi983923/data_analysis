---
title: "Nice Figure Tutorial-barplot2"
author: "**Yi Li**"
date: "2024-03-23"
toc: true
number-sections: true
format: 
   html:
     code-fold: false
     code-line-numbers: true
editor: visual
editor_options: 
  chunk_output_type: inline
---

Loading the packing in this section

{r}
#| label: load the package
#| include: false
#| code-summary: "package"
#| message: false

library(showtext)
library(extrafont)
library(forcats)
library(tidyverse)

read, clean the data

{r}
# reading the table
table2 <- read.table("/Users/liyi/Desktop/R_project/nice_figure_tutorial/data/input2.txt", header=T, sep="\t",comment.char = "", check.names =FALSE)
head(table2)
class(table2)

reshape of table

{r}
# 按FDR排序
labels <- table2[order(table2$FDR,decreasing =T),"Term"]
class(table2$Term)

table2$Term = factor(table2$Term,levels=labels)
class(table2$Term)

make a plot with ggplot2

{r}
# add nice font 
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

ggplot(data=table2)+
  geom_bar(aes(x=Term, y=Count, fill=FDR), stat='identity') +
  coord_flip() + 
  scale_fill_gradient(low="#9fc4db", high = "#982b2b") + 
  labs(
    x = NULL,
    y = "Gene counts",
    title = "GO testing for barplot2",
    caption = "Source: the online source from website in 2024"
  ) +
  scale_y_continuous(expand=c(0, 0)) + 
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text.x=element_text(color="black", size=10),axis.text.y=element_text(color="black", size=10)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(family = "Roboto Condensed", hjust = 0.5, face = "bold", size = 16, color = "black")) +
  theme(plot.subtitle = element_text(family = "Roboto Condensed", hjust = 0.5, size = 13, color = "#651222")) +
  theme(axis.text.y = element_text(family = "Roboto Condensed", hjust = 1, size = 8, face = "bold", color = "black")) +
 # theme(axis.title.y.left = element_text(margin = margin(r = 10), size = 12, color = "black", family = "Roboto Condensed", face = "bold")) +
 # theme(axis.ticks.y = element_blank())+
  theme(axis.text.x = element_text(family = "Roboto Condensed", hjust = 0.5, face = "bold", size = 10, color = "#89558d")) +
  theme(axis.title.x = element_text(family = "Roboto Condensed", face = "bold", size = 13, color = "#89558d"))+
  theme(plot.caption = element_text(family = "Roboto Condensed", hjust = 1.4, face = "bold", size = 8, color = "#173565")) +
  theme(legend.text = element_text(family = "Roboto Condensed", size = 8, face = "bold", color = "#173565")) +
  theme(legend.title = element_text(family = "Roboto Condensed", size = 10, face = "bold", color = "#173565"))
  



