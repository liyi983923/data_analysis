rm(list = ls())
library(tidyverse)
library(ggraph)
library(igraph)
library(RColorBrewer) 
library(showtext)
library(extrafont)
library(here)

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

here()

d1 <- read_csv(here("data/GPCRome.csv"))
d1

d2 <- d1[, c(14, 15, 16,2)]
d2

row_subclass_olfactory <- which(d2$`Sub-class` == "Olfactory receptors")
row_subclass_olfactory

d3 <- d2[-row_subclass_olfactory, ]
d3 <- d3 |> 
  arrange(Class, `Sub-class`,`Sub-sub-class`,`Approved symbol`)
str(d3)

unique(d3$Class)
unique(d3$`Sub-class`)
unique(d3$`Sub-sub-class`)
unique(d3$`Approved symbol`)

#选择class A的list,这样的list里是class A 在减去了olfactory receptor
d4 <- d3 |> 
  filter(Class == "Class A")




data1 <- data.frame(from="GPCR", to=paste(c(unique(d2$Class))))
data1

data2 <- d3[, c(1,2, 4)]
data2

#用dplyr中distinct方法 removing the duplicates in column Sub-class, and .keep_all function retained in the resulting dataframe
data2a <- d3 |> 
  filter(!is.na(`Sub-class`)) |>
  dplyr::distinct(`Sub-class`, .keep_all = T) |> 
  select( ,c(1 ,2))

data2a
colnames(data2a) <- c("from", "to")
data2a

data2_classA <- data2a |> 
  filter(from == "Class A")
data2_classA


data2b <- data2 |> 
  filter(is.na(`Sub-class`)) |> 
  select(, c(1, 3))
data2b
colnames(data2b) <- c("from", "to")
data2b







# 下面是用subclass:sub-subclass:symbol映射
data3 <- d3[, c(1, 2,3)]
data3

data3 <- data3 |> 
  filter(!is.na(`Sub-sub-class`)) |>
  dplyr::distinct(`Sub-sub-class`, .keep_all = T)
data3

colnames(data3) <- c("Class", "from", "to")
data3

data3_classA <- data3 |> 
  filter(Class == "Class A")



data4 <- d3[, c(1, 3, 4)]
data4

data4 <- data4 |> 
  filter(!is.na(`Sub-sub-class`)) 
data4
colnames(data4) <- c("Class","from", "to")

data4_classA <- data4 |> 
  filter(Class == "Class A")
  
  
  
  
#下面是subclass直接到symbol映射
data5 <- d3 |> 
  filter(is.na(`Sub-sub-class`), !is.na(`Sub-class`)) |> 
  select(1, 2, 4)
data5
colnames(data5) <- c("Class","from", "to")

data5_classA <- data5 |> 
  filter(Class == "Class A")

# 生成edge ------------------------------------------------------------------

edge <- rbind(data1, data2a, data2b, data3[2:3], data4[2:3], data5[2:3])
edge <- as.tibble(edge)
edge
class(edge)


#### 在生成数据作图时，最终的排列顺序是有data.frame d3中的顺序来排的，所以，在vertices中也要按同样的顺序排列，因为之后要表angle，对应每一个GPCR
vertices <- data.frame(name = c("GPCR", edge$to))
vertices$name[95:485] <- d3$`Approved symbol`[1:391]

class(vertices)
vertices
vertices <- as.tibble(vertices)



vertices

vertices$id=NA
myleaves=which(is.na( match(vertices$name, edge$from) ))
#be sure to change the above argument to your hierarchy/edge dataframe
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - (360 * (vertices$id / nleaves))
vertices$hjust<-ifelse(vertices$angle < -90, 1, 0)
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
vertices


#加一行信息，确定是否是分类信息or还是最终的基因名字，因为作图时基因名字做一次，主要分类信息再做一次。
vertices$is_main = NA

vertices

for (i in 1:nrow(vertices)) {
  if (is.na(vertices$angle[i])) {
    vertices$is_main[i] <- "yes" #这里必须用 <- ，不可以用==
  } else {
    vertices$is_main[i] <- "no"
  }
}


color <- c("#982b2b", "#f47720", "#459943", "#E9262E", "#E3863C","#1F77B4", "#F58F7A")

mygraph <- graph_from_data_frame(edge, vertices = vertices)

#graph dendrogram with leaf names
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(edge_color = "black", size=1, alpha=1, edge_width = 0.33, linetype = "solid") +


  geom_node_point(aes(filter = is_main == "no", x = x*1.01, y=y*1.01),  size = 1.35, shape = 1) + # alpha = 0 是完全透明

  
  geom_node_text(aes(x = x*1.025, y=y*1.025, filter = leaf, label=name,
                     angle = angle, hjust=hjust), family = "Roboto Condensed", size=1.5, alpha=1)+
  geom_node_text(aes(filter = name == "GPCR", label = name), size = 5, hjust = 0.5, angle = 0, family = "Roboto Condensed", fontface = "bold") +
  geom_node_text(aes(filter = name == "Class A", label = name), size = 4, hjust = 0.5, vjust=0, angle = 0, family = "Roboto Condensed", color = "black") +
  geom_node_text(aes(filter = name == "Class B", label = name), size = 4, hjust = 0.5, angle = -15, family = "Roboto Condensed", color = "black") +
  geom_node_text(aes(filter = name == "Class C", label = name), size = 4, hjust = -0.8, angle = -40, family = "Roboto Condensed", color = "black") +
  geom_node_text(aes(filter = name == "Class F", label = name), size = 4, hjust = 0.5, angle = -50, family = "Roboto Condensed", color = "black") +
  geom_node_text(aes(filter = name == "Taste receptors", label = "Taste"), size = 4, hjust = 0.5, angle = -68, family = "Roboto Condensed", color = "black") +
  geom_node_text(aes(filter = name == "Vomeronasal receptors", label = "Vomeronasal"), size = 4, hjust = 0, angle = -88, family = "Roboto Condensed", color = "black") +
  
  
  coord_fixed() +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm")
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))




# make plot with class A --------------------------------------------------
edge_classA <- rbind(data2_classA, data3_classA[2:3], data4_classA[2:3], data5_classA[2:3])

edge_classA <- as.tibble(edge_classA)
edge_classA
class(edge_classA)




#### 在生成数据作图时，最终的排列顺序是有data.frame d3中的顺序来排的，所以，在vertices中也要按同样的顺序排列，因为之后要表angle，对应每一个GPCR
vertices_classA <- data.frame(name = c("Class A", edge_classA$to))
vertices_classA$name[63:347] <- vertices$name[95:379]

class(vertices_classA)
vertices_classA
vertices <- as.tibble(vertices_classA)
head(vertices_classA)



vertices_classA$id=NA
myleaves_classA=which(is.na( match(vertices_classA$name, edge_classA$from) ))
#be sure to change the above argument to your hierarchy/edge dataframe
nleaves_classA=length(myleaves_classA)
vertices_classA$id[ myleaves_classA ] = seq(1:nleaves_classA)
vertices_classA$angle= 90 - (360 * (vertices_classA$id / nleaves_classA))
vertices_classA$hjust<-ifelse(vertices_classA$angle < -90, 1, 0)
vertices_classA$angle<-ifelse(vertices_classA$angle < -90, vertices_classA$angle+180, vertices_classA$angle)
vertices_classA



#加一行信息，确定是否是分类信息or还是最终的基因名字，因为作图时基因名字做一次，主要分类信息再做一次。
vertices_classA$is_main = NA

vertices_classA

for (i in 1:nrow(vertices_classA)) {
  if (is.na(vertices_classA$angle[i])) {
    vertices_classA$is_main[i] <- "yes" #这里必须用 <- ，不可以用==
  } else {
    vertices_classA$is_main[i] <- "no"
  }
}


mygraph_classA <- graph_from_data_frame(edge_classA, vertices = vertices_classA)

#graph dendrogram with leaf names
ggraph(mygraph_classA, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(edge_color = "black", size=1, alpha=1, edge_width = 0.33, linetype = "solid") +
  
  geom_node_point(aes(filter = is_main == "no", x = x*1.01, y=y*1.01),  size = 1.5,  color = "red") + # alpha = 0 是完全透明
 
  geom_node_text(aes(x = x*1.025, y=y*1.025, filter = leaf, label=name,
                     angle = angle, hjust=hjust), family = "Roboto Condensed", size=2, alpha=1) +
  geom_node_text(aes(filter = name == "Class A orphans", label=name), family = "Roboto Condensed", size=3, alpha=1, angle = -44, hjust = 1.5, vjust = 1.3, fontface = "bold") +
  geom_node_text(aes(filter = name == "Amine receptors", label="Amine"), family = "Roboto Condensed", size=3, alpha=1, angle =68, hjust = 1, vjust =1, fontface = "bold") +
  geom_node_text(aes(filter = name == "Amine receptors,Cholinergic receptors", label="Muscarinic"), family = "Roboto Condensed", size=3, alpha=1, angle =42, hjust =-1, vjust =-0.4, fontface = "bold") +
  geom_node_text(aes(filter = name == "Chemerin receptors", label="Chemerin"), family = "Roboto Condensed", size=3, alpha=1, angle =37, hjust =1, vjust =0.3, fontface = "bold") +
  geom_node_text(aes(filter = name == "Chemokine receptors", label="Chemokine"), family = "Roboto Condensed", size=3, alpha=1, angle =18, hjust =1, vjust =1.3, fontface = "bold") +
  geom_node_text(aes(filter = name == "Complement component GPCRs", label="Complement"), family = "Roboto Condensed", size=3, alpha=1, angle =86, hjust =0, vjust =0.5, fontface = "bold") +
  geom_node_text(aes(filter = name == "F2R receptors", label="F2R"), family = "Roboto Condensed", size=3, alpha=1, angle =86, hjust =0, vjust = 0.5, fontface = "bold") +
  geom_node_text(aes(filter = name == "Formyl peptide receptors", label="Formyl peptide"), family = "Roboto Condensed", size=3, alpha=1, angle =78, hjust =0, vjust =0.5, fontface = "bold") +
  geom_node_text(aes(filter = name == "Glycoprotein hormone receptors", label="Glycoprotein"), family = "Roboto Condensed", size=3, alpha=1, angle =70, hjust = 0.2, vjust =0.5, fontface = "bold") +
  geom_node_text(aes(filter = name == "Hydroxycarboxylic acid receptors", label="Hydroxycarboxylic acid"), family = "Roboto Condensed", size=3, alpha=1, angle =66, hjust = 0.1, vjust =0.4, fontface = "bold") +
  geom_node_text(aes(filter = name == "Lipid like receptors", label="Lipid"), family = "Roboto Condensed", size=3, alpha=1, angle =45, hjust = 0.1, vjust =-0.5, fontface = "bold") +
  geom_node_text(aes(filter = name == "Melatonin receptors", label="Melatonin"), family = "Roboto Condensed", size=3, alpha=1, angle =20, hjust = 0.1, vjust =0.5, fontface = "bold") +
  geom_node_text(aes(filter = name == "Nucleotide like receptors", label="Nucleotide"), family = "Roboto Condensed", size=3, alpha=1, angle =20, hjust = 2.2, vjust =0.2, fontface = "bold") +
  geom_node_text(aes(filter = name == "Opsin receptors", label="Opsin"), family = "Roboto Condensed", size=3, alpha=1, angle =8, hjust = 0, vjust =-0.3, fontface = "bold") +
  geom_node_text(aes(filter = name == "Oxoglutarate receptor", label="Oxoglutarate"), family = "Roboto Condensed", size=3, alpha=1, angle =2, hjust = 0, vjust =0.5, fontface = "bold") +
  geom_node_text(aes(filter = name == "Peptide receptors", label="Peptide"), family = "Roboto Condensed", size=3, alpha=1, angle =-35, hjust = 0, vjust =-0.3, fontface = "bold") +
  geom_node_text(aes(filter = name == "Prokineticin receptors", label="Prokineticin"), family = "Roboto Condensed", size=3, alpha=1, angle =-72, hjust = 0, vjust =0.5, fontface = "bold") +
  geom_node_text(aes(filter = name == "Purinergic receptors", label="Purinergic"), family = "Roboto Condensed", size=3, alpha=1, angle =-78, hjust = 2, vjust =0.3, fontface = "bold") +
  geom_node_text(aes(filter = name == "Relaxin family peptide receptors", label="Relaxin"), family = "Roboto Condensed", size=3, alpha=1, angle =-86, hjust = 0, vjust =0.5, fontface = "bold") +
  geom_node_text(aes(filter = name == "Succinate receptor", label="Succinate"), family = "Roboto Condensed", size=3, alpha=1, angle =-90, hjust = 0, vjust =0.35, fontface = "bold") +
  
    coord_fixed() +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm")
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))








