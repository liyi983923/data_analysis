rm(list=ls())
setwd("/Users/liyi/Desktop/R_project/BMG testing/")
library(tidyverse)
library(showtext)
library(extrafont)
library(cowplot)
library(ggpubr)
library(lubridate)
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# color <- c("#982b2b", "#f47720", "#459943", "#E9262E", "#E3863C""#1F77B4", "#F58F7A")
            
######################################## 1. Ca flux figure with M1 receptor #####################################################################
M1_Ca <- read.csv(file = "/Users/liyi/Desktop/R_project/BMG testing/M1-Calcium.csv", header = T, sep = ",", row.names = 1)
head(M1_Ca)
class(M1_Ca)
# NK1R <- tibble::rownames_to_column(NK1R, "Samples")
M1_Ca <- as_tibble(M1_Ca)
M1_Ca
M1_Ca <- M1_Ca %>% pivot_longer(cols =1:180 , names_to = "time", values_to = "value")
M1_Ca

M1_Ca$time <- gsub('[X]', '', M1_Ca$time)
M1_Ca

class(M1_Ca$time)
M1_Ca$time <- as.numeric(M1_Ca$time)
M1_Ca$time <- M1_Ca$time + 1



P_M1_Ca <- ggplot(data = M1_Ca,
       mapping = aes(x = time, y = value)) +
  geom_point(size = 2.5, color = "#982b2b") +
  theme(panel.background = element_rect(fill = "white")) +   #背景是白色
  theme(axis.title.x = element_text(size = 20, color = "black", ), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + #把x轴上的小分子名字去掉
  theme(axis.title.y = element_text(size = 20, color ="black")) +
  theme(axis.text.y.left = element_text(size = 20, color = "black")) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 2)) + #加方框
  theme(plot.margin = margin(5,10, 5,10)) + #整个图片的留白
  theme(plot.title = element_text(family = "Roboto Condensed", hjust = 0.5, face = "bold", size = 20, color = "black")) +
  #scale_color_manual(values = c("#1F77B4")) +
  labs(title = "Calcium Flux", x= "Time after 1uM Ach", y = "Flurensence") 

P_M1_Ca



######################################## 2. MOR NanoBit #####################################################################
MOR_Bit <- read.csv(file = "/Users/liyi/Desktop/R_project/BMG testing/MOR_Nanobit.csv", header = T, sep = ",", row.names = 1)
head(MOR_Bit)
class(MOR_Bit)
# NK1R <- tibble::rownames_to_column(NK1R, "Samples")
MOR_Bit <- as_tibble(MOR_Bit)
MOR_Bit
MOR_Bit <- MOR_Bit %>% 
  t() %>% 
  as.data.frame() %>% 
  setNames(c("HBSS", "1uM DAMGO"))
MOR_Bit

MOR_Bit <- tibble::rownames_to_column(MOR_Bit, "time")
MOR_Bit

MOR_Bit$time <- gsub("[X]", "", MOR_Bit$time)
MOR_Bit
class(MOR_Bit)
MOR_Bit <- as_tibble(MOR_Bit)
MOR_Bit

MOR_Bit <- MOR_Bit %>% pivot_longer(cols =!time , names_to = "sample", values_to = "value")
MOR_Bit
class(MOR_Bit)

MOR_Bit <- tibble::rownames_to_column(MOR_Bit, "order")
MOR_Bit
MOR_Bit <- MOR_Bit[, c(1,3,4)]
MOR_Bit
class(MOR_Bit$order)
MOR_Bit$order <- as.numeric(MOR_Bit$order)



P_MOR <- ggplot(data = MOR_Bit,
                mapping = aes(x = order, y = value, color=sample)) +
  geom_point(size = 2) +
  geom_line()+
  theme(panel.background = element_rect(fill = "white")) +   #背景是白色
  theme(axis.title.x = element_text(size = 20, color = "black", ), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + #把x轴上的小分子名字去掉
  theme(axis.title.y = element_text(size = 20, color ="black")) +
  theme(axis.text.y.left = element_text(size = 20, color = "black")) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 2)) + #加方框
  theme(plot.margin = margin(5,10, 5,10)) + #整个图片的留白
  theme(plot.title = element_text(family = "Roboto Condensed", hjust = 0.5, face = "bold", size = 20, color = "black")) +
  theme(legend.title = element_blank()) + #去掉legend的title
  theme(legend.text = element_text(family ="Roboto Condensed" )) +
  theme(legend.position = c(0.8, 0.2), legend.direction = "vertical", legend.box = "horizontal")+ #改变legend的位置
  scale_color_manual(values = c("#982b2b", "#459943"))+
  labs(title = "hMOR-SmBit/β-arrestin2-LgBit", x= "Time after 1uM DAMGO", y = "Luminesence")

P_MOR
#"#982b2b", "#f47720", "#459943"

######################################## 3. NK1R BRET assay #####################################################################
NK1R_BRET <- read.csv(file = "/Users/liyi/Desktop/R_project/BMG testing/NK1R BRET.csv", header = T, sep = ",", row.names = 1)
class(NK1R_BRET)
str(NK1R_BRET)
NK1R_BRET <- as_tibble(NK1R_BRET)
NK1R_BRET


NK1R_BRET <- tibble::rownames_to_column(NK1R_BRET, "sample")
NK1R_BRET

NK1R_BRET <- NK1R_BRET %>% 
  mutate(sample = paste("sample", as.character(sample), sep = " ")) #改sample name从1 到sample 1
NK1R_BRET

NK1R_BRET <- NK1R_BRET %>% 
  mutate(treatment = c("ctr", "ctr", "100nM sp", "100nM sp", "1uM sp", "1uM sp")) #加一列treatment到这个表格，但是默认在最后一列的位置
NK1R_BRET

NK1R_BRET <- NK1R_BRET[, c(1, 182, 2:181)]#重新改变数据格式，将treatment改到第二列。
NK1R_BRET

# ### 单个时间点的mean，sd，p value值计算，但是好像用function往下做，有问题。后面有时间再来解决这个问题。
# result_summary <- NK1R_BRET %>%
#   group_by(treatment) %>%
#   summarize(mean_X0.min.31.s = mean(X0.min.31.s),
#             sd_X0.min.31.s = sd(X0.min.31.s),
#             p_value_X0.min.31.s = t.test(X0.min.31.s)$p.value)

timepoint <- colnames(NK1R_BRET)
timepoint <- timepoint[3:182]
timepoint
head(timepoint)
str(timepoint)

# 把时间的输出格式改成1min2s
# 使用 `for()` 循环批量处理
formatted_time_points <- c()
for (x in timepoint) {
  # 去掉 X
  x <- gsub("X", "", x)
  
  # 分解字符串
  split_x <- strsplit(x, "\\.")[[1]]
  
  # 将秒数转换为整数
  seconds <- as.integer(split_x[3])
  seconds
  
  # 将分钟数转换为整数
  minutes <- as.integer(split_x[1])
  
  # # 将分钟数和秒数相加
  # total_time <- minutes * 60 + seconds
  # 
  # 将总时间格式化为 "s" 格式
   formatted_time <- paste(1,minutes, seconds, sep = ":")
  # formatted_time <- paste(minutes, "m", seconds, "s", sep = "")

   formatted_time_points <- c(formatted_time_points, formatted_time)
}

# 输出结果
formatted_time_points

str(formatted_time_points)
str(NK1R_BRET)

colnames(NK1R_BRET)[3:182] <- formatted_time_points
NK1R_BRET

# 把数据格式改过来
NK1R_BRET <- NK1R_BRET %>% 
   pivot_longer(cols = -c(sample, treatment),
               names_to = "time",
               values_to = "value")
NK1R_BRET
NK1R_BRET$time
str(NK1R_BRET$time)

# NK1R_BRET$time <- strptime(NK1R_BRET$time, format = "%m%S")
# NK1R_BRET$time

# NK1R_BRET$time <-hms(NK1R_BRET$time)
# NK1R_BRET$time

NK1R_BRET$time <- paste("2023-02-01 ", NK1R_BRET$time, sep = "") #R中的时间是2023-02-01 10：12：11
NK1R_BRET$time
class(NK1R_BRET$time)
NK1R_BRET$time <- ymd_hms(NK1R_BRET$time) #用lubridate把时间标好
#NK1R_BRET$time <- strptime(NK1R_BRET$time, format = "%Y-%m-%d %h:%m:%S")
NK1R_BRET$time




P_NK1R_BRET<- ggplot(data = NK1R_BRET,
             mapping = aes(x = time, y = value, color=treatment)) +
  geom_point(size = 2) +
  geom_line() +
  theme(panel.background = element_rect(fill = "white")) +   #背景是白色
  theme(axis.title.x = element_text(size = 20, color = "black", ), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + #把x轴上的小分子名字去掉
  theme(axis.title.y = element_text(size = 20, color ="black")) +
  theme(axis.text.y.left = element_text(size = 20, color = "black")) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 2)) + #加方框
  theme(plot.margin = margin(5,10, 5,10)) + #整个图片的留白
  theme(plot.title = element_text(family = "Roboto Condensed", hjust = 0.5, face = "bold", size = 20, color = "black")) +
  theme(legend.title = element_blank()) + #去掉legend的title
  theme(legend.text = element_text(family ="Roboto Condensed" )) +
  theme(legend.position = c(0.15, 0.85), legend.direction = "vertical", legend.box = "horizontal")+ #改变legend的位置
  theme(legend.box = "transparent") + #去掉图例边框
  #scale_x_continuous() +
  scale_color_manual(values = c("#982b2b", "#f47720", "#459943"))+
  labs(title = "NK1R-nLuc/Venus-alpha Gq", x= "Time after sp treatment", y = "netBRET")

P_NK1R_BRET



######################################## 4. D2 GAP assay #####################################################################
GAP28 <- read.csv(file = "/Users/liyi/Desktop/R_project/BMG testing/GAP28.csv", header = T, sep = ",", row.names = 1)

class(GAP28)
str(GAP28)
GAP28 <- as_tibble(GAP28)
GAP28


GAP28 <- tibble::rownames_to_column(GAP28, "sample")
GAP28

GAP28 <- GAP28 %>% 
  mutate(sample = paste("sample", as.character(sample), sep = " ")) #改sample name从1 到sample 1
GAP28

GAP28 <- GAP28 %>% 
  mutate(RGS = c("RGS7", "RGS7", "pcDNA3.1", "pcDNA3.1")) #加一列treatment到这个表格，但是默认在最后一列的位置
GAP28

GAP28 <- GAP28 %>% 
  mutate(treatment = c("HBSS", "haloperidol", "HBSS", "haloperidol"))
GAP28

GAP28 <- GAP28[, c(1, 402, 403, 2:401)]#重新改变数据格式，将treatment改到第二列。
GAP28

timepoint <- colnames(GAP28)
timepoint <- timepoint[4:403]
timepoint
head(timepoint)
str(timepoint)


# 把时间的输出格式改成1min2s
# 使用 `for()` 循环批量处理
formatted_time_points <- c()
for (x in timepoint) {
  # 去掉 X
  x <- gsub("X", "", x)
  
  # 分解字符串
  split_x <- strsplit(x, "\\.")[[1]]
  
  # 将秒数转换为整数
  seconds <- as.integer(split_x[3])
  seconds
  
  # 将分钟数转换为整数
  minutes <- as.integer(split_x[1])
  
  # # 将分钟数和秒数相加
  # total_time <- minutes * 60 + seconds
  # 
  # 将总时间格式化为 "s" 格式
  formatted_time <- paste(1,minutes, seconds, sep = ":")
  # formatted_time <- paste(minutes, "m", seconds, "s", sep = "")
  
  formatted_time_points <- c(formatted_time_points, formatted_time)
}

# 输出结果
formatted_time_points

str(formatted_time_points)
str(GAP28)

colnames(GAP28)[4:403] <- formatted_time_points
GAP28

# 把数据格式改过来
GAP28 <- GAP28 %>% 
  pivot_longer(cols = -c(sample, RGS, treatment),
               names_to = "time",
               values_to = "value")
GAP28
GAP28$time
str(GAP28$time)

# NK1R_BRET$time <- strptime(NK1R_BRET$time, format = "%m%S")
# NK1R_BRET$time

# NK1R_BRET$time <-hms(NK1R_BRET$time)
# NK1R_BRET$time

GAP28$time <- paste("2023-02-01 ", GAP28$time, sep = "") #R中的时间是2023-02-01 10：12：11
GAP28$time
class(GAP28$time)
GAP28$time <- ymd_hms(GAP28$time) #用lubridate把时间标好
#NK1R_BRET$time <- strptime(NK1R_BRET$time, format = "%Y-%m-%d %h:%m:%S")
GAP28$time

P_GAP28<- ggplot(data = GAP28,
                     mapping = aes(x = time, y = value, color = sample)) +
  geom_point(size = 2) +
  geom_line() +
  theme(panel.background = element_rect(fill = "white")) +   #背景是白色
  theme(axis.title.x = element_text(size = 20, color = "black", ), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + #把x轴上的小分子名字去掉
  theme(axis.title.y = element_text(size = 20, color ="black")) +
  theme(axis.text.y.left = element_text(size = 20, color = "black")) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 2)) + #加方框
  theme(plot.margin = margin(5,10, 5,10)) + #整个图片的留白
  theme(plot.title = element_text(family = "Roboto Condensed", hjust = 0.5, face = "bold", size = 20, color = "black")) +
  theme(legend.title = element_blank()) + #去掉legend的title
  theme(legend.text = element_text(family ="Roboto Condensed" )) +
    theme(legend.position = c(0.85, 0.8), legend.direction = "vertical", legend.box = "horizontal")+ #改变legend的位置
  #scale_x_continuous() +
  scale_color_manual(values = c( "#982b2b", "#f47720", "#459943", "#E9262E"))+
  labs(title = "GAP assay with D2 receptor", x= "Treatment of Dopamine and Haloperidol", y = "netBRET") +
  labs(legend.text = c("a", "b", "c", "d"))

P_GAP28


################################################ 整图 ############################################################

p <- plot_grid(P_M1_Ca, P_MOR, P_NK1R_BRET, P_GAP28, labels = c("A", "B", "C", "D"), label_size = 20)
p

p_n1 <- ggdraw() +
  draw_plot(P_M1_Ca, 0.03, 0, width = 0.43, height = 0.45)+
  draw_plot(P_MOR, 0.5, 0, width = 0.43, height = 0.45) +
  draw_plot(P_NK1R_BRET, 0.03, 0.5, width = 0.43, height = 0.45) +
  draw_plot(P_GAP28, 0.5, 0.5, width = 0.43, height = 0.45) +
  draw_plot_label(label = c("A", "B", "C", "D"),
                  x = c(0.03, 0.5, 0.03, 0.5),
                  y = c(0.98, 0.98, 0.5, 0.5),
                  size = 25, family ="Roboto Condensed", fontface = "bold")
p_n1

ggsave(p_n1, filename = "figure.pdf", width = 12, height = 9, units = "in", dpi = 600)










