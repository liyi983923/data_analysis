rm(list=ls())
library(gt)
library(gtExtras)
library(tidyverse)
library(here)
library(skimr)

data <- read.csv(here("data/GPCR panel-24.05.30.csv"))
data <- as.tibble(data)
head(data)

BRET1 <- data |> 
  filter(Methods == "BRET1")



BRET1_pre_y <- BRET1 |> 
  filter(Pretreatment  == "yes") |> 
  slice(-1, -2) |> 
  select(-2, -3, -4, -7, -10, -13, -15)

colnames_BRET1_pre_y <- colnames(BRET1_pre_y) |> 
  paste("w.treat", spe = "")
colnames(BRET1_pre_y) <- colnames_BRET1_pre_y


BRET1_pre_n <- BRET1 |> 
  filter(Pretreatment == "no") |> 
  slice(2, 4, 7, 10, 13:20) |> 
  select(-4, -7, -10, -13, -15)


DF <- inner_join(BRET1_pre_n, BRET1_pre_y, by = c("GPCR"= "GPCR w.treat "))
DF

DF1 <- BRET1_pre_n |> 
  slice(1,2,3,10)

DF2 <- data.frame(agonist_w.treat = rep(NA, 4), 
  netBRET.agonist_w.treat = rep(NA, 4), 
  Significance.agonist_w.treat = rep(NA, 4), 
  netBRET.QG186_w.treat = rep(NA, 4),        
  Significance.QG186_w.treat = rep(NA, 4), 
  netBRET.261b_w.treat = rep(NA, 4), 
  Significance.261b_w.treat = rep(NA, 4))


DF_new <- cbind(DF1, DF2)
DF_new
DF

colnames(DF) <- colnames(DF_new)

DF_BRET1 <- rbind(DF, DF_new) |> 
  slice(9, 10, 11, 1:6, 12, 7, 8) |> 
  select(1:3, 11, 5, 7, 9, 12, 14, 16)

DF_BRET1[4,2] <- "arrestin"
class(DF_BRET1)

DF_BRET1[10, 5] <- "NA"
DF_BRET1[1:3, 5] <- "NA"





b1 <- DF_BRET1 |> 
    gt() |> 
  gt_theme_538() |> 
  tab_header(title = str_to_upper("GPCR panel assay in T5 project"),
                             subtitle=md("In 2024, we start to do **GPCR panel assay with known GPCRs**. We chose some GPCR receptors in our lab list and did some pilot experiments.")) |> 
  tab_source_note(source_note="Data source: data was performed by 田有溪") |> 
  cols_label(
    agonist_w.treat = "agonist",
    netBRET.agonist = "agonist",
    netBRET.QG186 = "QG186",
    netBRET.261b = "261b",
    netBRET.agonist_w.treat = "agonist",
    netBRET.QG186_w.treat = "QG186",
    netBRET.261b_w.treat = "261b") |> 
  tab_spanner(label = "No pretreatment(/HBSS)", columns = netBRET.agonist: netBRET.261b) |> 
  tab_spanner(label = "Pretreatment (/Agonist)", columns = netBRET.agonist_w.treat:netBRET.261b_w.treat)  |> 
  tab_footnote(
    footnote = md("in this assay, we only testing T5 compounds and agoinst individulally. So, all the values are normalized with HBSS. We found that all the agonist present the significant induction."),
    locations = cells_column_spanners("No pretreatment(/HBSS)")
  ) |> 
  tab_footnote(
    footnote = md("in this assay, we firstly preteatment with agoinsts, which induce the GPCRs activity in reference. So, all the vaules in this assay is normalized with agonist respectively."),
    locations = cells_column_spanners( "Pretreatment (/Agonist)")) |> 
  gt_color_rows(columns=netBRET.agonist: netBRET.261b,  palette="RColorBrewer::Blues") |> 
  gt_color_rows(columns=netBRET.agonist_w.treat:netBRET.261b_w.treat, palette="RColorBrewer::Oranges") |> 
  tab_options(
    table.font.size = "18px",
    heading.title.font.size = "32px") |> 
  
  tab_options(
    heading.subtitle.font.size = 17,
    heading.align = "left",
    column_labels.border.bottom.width = 3,
  ) |> 

  tab_style(
    style = list(
      cell_text(font=google_font(name = "Libre Franklin"), weight='800',align = "left",color='#982b2b')),
    locations = cells_title(groups = "title")) |> 
  
  cols_width(
    GPCR ~ px(100),
    Effector ~ px(100),
    Methods ~ px(100),
    agonist_w.treat ~ px(150),
    netBRET.agonist:netBRET.261b_w.treat ~ px(80)
  )
  
gtsave(b1, "b1.png", expand = 50)
