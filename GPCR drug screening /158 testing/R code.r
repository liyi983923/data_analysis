rm(list=ls())
setwd("/Users/liyi/Desktop/R_project/158//")
library(tidyverse)
library(showtext)
library(extrafont)
library(cowplot)
library(ggpubr)

######################################## data21 figure with whole compounds #####################################################################
data1 <- read.csv(file.choose(), header = T, sep = ",", row.names = 1)
head(data1)
class(data1)
data1 <- tibble::rownames_to_column(data1, "compound")
head(data1)

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()


p1 <- ggplot(data = data1,
  mapping = aes(x=compound, y=netBRET)) +
  geom_point(size= 4, color="black") +
  geom_hline(yintercept = median(data1$netBRET), linetype = "dashed", color = "red", size=2) +
  expand_limits(x = c(-15, 300)) +      #2侧的点收缩，不至于到方框里
  theme(panel.background = element_rect(fill = "white")) +   #背景是白色
  theme(axis.title.x = element_text(size = 20, color = "black", ), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + #把x轴上的小分子名字去掉
  theme(axis.title.y = element_text(size = 20, color ="black")) +
  theme(axis.text.y.left = element_text(size = 20, color = "black")) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 2)) + #加方框
  theme(plot.margin = margin(5,10, 5,10)) + #整个图片的留白
  theme(plot.title = element_text(family = "Roboto Condensed", hjust = 0.5, face = "bold", size = 20, color = "black")) +
  labs(title = "netBRET", x= "Compounds for GPR158", y = "% of the response") +
  ylim(0.8, 1.2)
p1

######################################## data2 figure 1mM Glycine figure #####################################################################
data2 <- read.csv(file.choose(), header = T, sep = ",")
head(data2, c(6,6))
data2

data2 <- as_tibble(data2)
data2
data2 <- data2 %>% 
  select(-(X0.min: X0.min.48.s))
data2

### 单个时间点的mean，sd，p value值计算
# result_summary <- data2 %>%
#   group_by(Protein.exprssioin, Treatment) %>%
#   summarize(mean_X1.min.23.s = mean(X1.min.23.s),
#             sd_X1.min.23.s = sd(X1.min.23.s),
#             p_value_X1.min.23.s = t.test(X1.min.23.s)$p.value)

#### 写一个function，把124个时间点的mean，sd， p value全部算出来
calculate_summary <- function(data, group_col, treatment_col, columns_to_calculate) {
  result_summary <- data %>%
    group_by(!!sym(group_col), !!sym(treatment_col)) %>%
    summarise(across(all_of(columns_to_calculate),
                     list(
                       mean = ~mean(.),
                       sd = ~sd(.),
                       p_value = ~t.test(.)$p.value
                     ),
                     .names = "{.col}_{.fn}"
    ))
  
  return(result_summary)
}

#### 把时间点，既column name抽出来
timepoint <- colnames(data2)
timepoint <- timepoint[3:103]
timepoint

#### 最后用函数把全部124个时间点的mean，sd， p value全部算出来
data2_summary <- calculate_summary(data2, "Protein.exprssioin", "Treatment", timepoint)

# View the summary results
print(data2_summary)

mean <- data2_summary %>% 
  select(Protein.exprssioin, Treatment,ends_with("_mean")) %>% 
  pivot_longer(cols = -c(Protein.exprssioin, Treatment),
               names_to = "time",
               values_to = "mean")
mean

sd <- data2_summary %>% 
  select(ends_with("_sd")) %>% 
  pivot_longer(cols = -Protein.exprssioin,
               names_to = "time",
               values_to = "sd")
sd <- sd[, 3]
sd

p_value<- data2_summary %>% 
  select(ends_with("p_value")) %>% 
  pivot_longer(cols = -Protein.exprssioin,
               names_to = "time",
               values_to = "p_value")
p_value <- p_value[, 3]
p_value

data2_tidy <- bind_cols(mean, sd, p_value)
data2_tidy


timepoint <- data2_tidy$time
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
class(formatted_time_points)
a <- formatted_time_points
a[a == "1:11:NA"] <- "1:11:0"
a[a == "1:10:NA"] <- "1:10:0"
a[a == "1:9:NA"] <- "1:9:0"
a[a == "1:8:NA"] <- "1:8:0"
a[a == "1:7:NA"] <- "1:7:0"
a[a == "1:6:NA"] <- "1:6:0"

data2_tidy$time <- a

data2_tidy$time <- paste("2023-02-01", data2_tidy$time, sep = "—")#R中的时间是2023-02-01 10：12：11
data2_tidy$time
class(data2_tidy$time)
data2_tidy$time <- ymd_hms(data2_tidy$time)  #用lubridate把时间标好
data2_tidy$time






p2 <- formatted_time_pointsp2 <- ggplot(data = data2_tidy,
  mapping = aes(x = time, y = mean, color = Protein.exprssioin, shape = Treatment)) +
  geom_point(size = 4) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.title.x = element_text(size = 20, color = "black", ), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + #把x轴上的小分子名字去掉
  theme(axis.title.y = element_text(size = 20, color ="black")) +
  theme(axis.text.y.left = element_text(size = 20, color = "black")) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 2)) + #加方框
  theme(plot.title = element_text(family = "Roboto Condensed", hjust = 0.5, face = "bold", size = 20, color = "black")) +
  theme(legend.title = element_blank()) + #去掉legend的title
  theme(legend.text = element_text(family ="Roboto Condensed" )) +
  theme(legend.position = c(0.5, 0.15), legend.direction = "vertical", legend.box = "horizontal")+ #改变legend的位置
  labs(title = "netBRET of 1mM Glycine treatment", x= "Time of Treatment", y = "netBRET") +
  scale_color_manual(values = c("#982b2b", "#f47720", "#459943"))+
  ylim(0.1,0.25)
p2

######################################## data3 figure 208 figure #####################################################################
data3 <- read.csv(file.choose(), header = T, sep = ",")
head(data3, c(6,6))
data3

data3 <- as_tibble(data3)
data3
data3 <- data3 %>% 
  select(-(X0.min: X0.min.54.s))
data3

#### 写一个function，把124个时间点的mean，sd， p value全部算出来
calculate_summary <- function(data, group_col, treatment_col, columns_to_calculate) {
  result_summary <- data %>%
    group_by(!!sym(group_col), !!sym(treatment_col)) %>%
    summarise(across(all_of(columns_to_calculate),
                     list(
                       mean = ~mean(.),
                       sd = ~sd(.),
                       p_value = ~t.test(.)$p.value
                     ),
                     .names = "{.col}_{.fn}"
    ))
  
  return(result_summary)
}

#### 把时间点，既column name抽出来
timepoint <- colnames(data3)
timepoint <- timepoint[3:102]
timepoint

#### 最后用函数把全部124个时间点的mean，sd， p value全部算出来
data3_summary <- calculate_summary(data3, "name", "Treatment", timepoint)

# View the summary results
print(data3_summary)

mean3 <- data3_summary %>% 
  select(name, Treatment,ends_with("_mean")) %>% 
  pivot_longer(cols = -c(name, Treatment),
               names_to = "time",
               values_to = "mean")
mean3

sd3 <- data3_summary %>% 
  select(ends_with("_sd")) %>% 
  pivot_longer(cols = -name,
               names_to = "time",
               values_to = "sd")
sd3 <- sd3[, 3]
sd3

p_value3<- data3_summary %>% 
  select(ends_with("p_value")) %>% 
  pivot_longer(cols = -name,
               names_to = "time",
               values_to = "p_value")
p_value3 <- p_value3[, 3]
p_value3

data3_tidy <- bind_cols(mean3, sd3, p_value3)
data3_tidy


timepoint <- data3_tidy$time
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
class(formatted_time_points)
a <- formatted_time_points
# a[a == "1:11:NA"] <- "1:11:0"
# a[a == "1:10:NA"] <- "1:10:0"
# a[a == "1:9:NA"] <- "1:9:0"
# a[a == "1:8:NA"] <- "1:8:0"
# a[a == "1:7:NA"] <- "1:7:0"
# a[a == "1:6:NA"] <- "1:6:0"
# 
data3_tidy$time <- a

data3_tidy$time <- paste("2023-02-01", data3_tidy$time, sep = "—")#R中的时间是2023-02-01 10：12：11
data3_tidy$time
class(data3_tidy$time)
data3_tidy$time <- ymd_hms(data3_tidy$time)  #用lubridate把时间标好
data3_tidy$time


p3 <- ggplot(data = data3_tidy,
             mapping = aes(x = time, y = mean, shape = Treatment, color = name)) +
  geom_point(size = 4) +
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.title.x = element_text(size = 20, color = "black", ), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + #把x轴上的小分子名字去掉
  theme(axis.title.y = element_text(size = 20, color ="black")) +
  theme(axis.text.y.left = element_text(size = 20, color = "black")) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 2)) + #加方框
  theme(plot.title = element_text(family = "Roboto Condensed", hjust = 0.5, face = "bold", size = 20, color = "black")) +
  theme(legend.title = element_blank()) + #去掉legend的title
  theme(legend.text = element_text(color = "black", size = 9)) +
  theme(legend.position = c(0.5, 0.15), legend.direction = "vertical", legend.box = "horizontal")+ #改变legend的位置
  theme(legend.text = element_text(family ="Roboto Condensed")) +
  labs(title = "netBRET of 208 treatment", x= "Time of Treatment", y = "netBRET") +
  scale_color_manual(values = c("#982b2b", "#f47720", "#459943"))+
  ylim(0.1,0.27)
p3





######################################## cowplot to make figures #####################################################################

p <- plot_grid(p2, p3, p1, labels = c("A", "B", "C"), label_size = 20)
p

p_n1 <- ggarrange(p2, p3, p1, labels = c("A", "B", "C"), ncol = 3, nrow = 1, font.label = list(size=20, color="black", face="bold", family="Roboto Condensed"))
p_n1

p_n2 <- ggdraw() +
  draw_plot(p2, 0,0, width = 0.3, height = 0.9)+
  draw_plot(p3, 0.33, 0, width = 0.3, height = 0.9) +
  draw_plot(p1, 0.66, 0, width = 0.3, height = 0.9) +
  draw_plot_label(label = c("A", "B", "C"),
                  x = c(0.16, 0.5, 0.82),
                  y = c(0.98, 0.98, 0.98),
                  size = 25, family ="Roboto Condensed", fontface = "bold")
p_n2



