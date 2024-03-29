rm(list=ls())
library(fmsb)
library(tidyverse)
library(showtext)
library(extrafont)
library(cowplot)
library(patchwork)
library(gridExtra)

color <- c("#982b2b", "#f47720", "#459943", "#E9262E", "#E3863C","#1F77B4", "#F58F7A")
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

data <- read.csv(file.choose(), header = T, sep = ",", row.names = 1)
data
class(data)
glimpse(data)

df1 <- data.frame(Gi1=c(0, 100), Gi2=c(0, 100), Gi3=c(0, 100), Gz=c(0, 100), Ggus=c(0, 100), GoA=c(0, 100), GoB=c(0, 100), Gq=c(0, 100), G11=c(0, 100), G12=c(0, 100), G14=c(0, 100), G15=c(0, 100))
df1
rownames(df1) <- c("min", "max")
df1

df <- rbind(df1, data)
df

Siponimod <- df[c("min", "max", "Siponimod"), ]
Siponimod


op <- par(family = "Roboto Condensed", font=4, font.main=2)
par(mfrow = c(1 ,2)) 

p1 <- radarchart(Siponimod, 
                 pcol = "#982b2b",  # 多边形的颜色
                 plwd = 3,          # 多边形线条的宽度
                 plty = "solid",    # 多边形线条的类型
                 cglcol ="#1F77B4", 
                 cglwd = 3,  # 圆形网格线的类型
                 cglty = "dotted", # 多边形线条的类型
                 axislabcol = "#E9262E",
                 vlcex = 1.2  # 顶点标签字体大小的缩放因子
                 )

title(main = "Siponimod", 
      cex.main=2,          # font size of the title
      line = -23.5
      )

# add label "A" to chart Siponimod
text(0, 0, "A", cex=2, adj = c(11,-8.1))





############################################
Fingolimod <- df[c("min", "max", "Fingolimod hydrochloride"), ]
Fingolimod  

p2 <- radarchart(Fingolimod, 
                 pcol = "#982b2b",  # 多边形的颜色
                 plwd = 3,          # 多边形线条的宽度
                 plty = "solid",    # 多边形线条的类型
                 cglcol ="#1F77B4", 
                 cglwd = 3,  # 圆形网格线的类型
                 cglty = "dotted", # 多边形线条的类型
                 axislabcol = "#E9262E",
                 vlcex = 1.2  # 顶点标签字体大小的缩放因子
                )

title(main = "Fingolimod hydrochloride", 
      cex.main=2,          # font size of the title
      line = -23.5
)

# add label "B" to chart Siponimod
text(0, 0, "B", cex=2, adj = c(11.5,-8.1))

# 添加整体标题并调整字体大小
title(main = "The G protein bias of SP1R1", outer = TRUE, line = -2.5, cex.main=2.5)


