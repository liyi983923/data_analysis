---
title: "Nice Figure Tutorial-barplot"
author: "**Yi Li**"
date: "2024-03-21"
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
table1 <- read.table("/Users/liyi/Desktop/R_project/nice_figure_tutorial/data/input1", header=T, sep="\t",comment.char = "", check.names =FALSE)
head(table1)

# calculate the number of gene
table1 <- table(c(as.vector(table1[,1]),as.vector(table1[,2])))    
head(table1)
class(table1)
str(table1)

# make table1 as a data frame and order of table1
table1 <- as.data.frame(table1)
head(table1)
class(table1)
colnames(table1) <- c("gene", "freq")
head(table1)
table1 <- table1 |> 
  arrange(desc(freq))
head(table1)

# select the top30 genes to draw the barplot
table1 <- table1 |> 
  slice(1:30) 
head(table1)


make a plot with ggplot2

{r}
# make a barplot with ggplot+geom_col/geom_bar
ggplot(data = table1,
       mapping = aes(x = freq, y = gene)) +
  geom_col()

# we are going to reoder the gene based on its counts with forcats package
ggplot(data = table1,
       mapping = aes(x = freq, y = fct_reorder(gene, freq))) +
  geom_col()

# add nice font 
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# make a new figure
p <- ggplot(data = table1,
       mapping = aes(x = freq, y = fct_reorder(gene, freq))) 


modify the figure

{r}
p +
  geom_col(fill = "#aa3538", width = 0.8) +
  
  labs(
    x = NULL,
    y = "The name of genes induced by XX",
    title = "Top 30 Most Expression genes",
    subtitle = "# the examples of barplot",
    caption = "Source: the online source from website in 2024"
  ) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(family = "Roboto Condensed", hjust = 0.5, face = "bold", size = 16, color = "black")) +
  theme(plot.subtitle = element_text(family = "Roboto Condensed", hjust = 0.5, size = 13, color = "#651222")) +
  theme(axis.text.y = element_text(margin = margin(r = -22), family = "Roboto Condensed", hjust = 1, size = 8, color = "black")) +
  theme(axis.title.y.left = element_text(margin = margin(r = 10), size = 12, color = "black", family = "Roboto Condensed", face = "bold")) +
  theme(axis.ticks.y = element_blank())+
  theme(axis.text.x = element_text(family = "Roboto Condensed", hjust = 0.5, face = "bold", size = 10, color = "black")) +
  theme(plot.caption = element_text(family = "Roboto Condensed", hjust = 1, face = "bold", size = 8, color = "#173565"))



