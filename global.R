# Packages  ------------------------------
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(pdftools)
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(readr)
library(naniar)
library(plotly)
library(highcharter)
library(ggplot2)
library(googlesheets4)
library(sparkline)
library(htmlwidgets)
library(DT)
library(rdrop2)

# Sourcing  ------------------------------
source("helpers.R")

# Google sheet authentication ------------
gs4_auth(cache = ".secrets", email = "salvage.test1@gmail.com")

# Reading data  ------------

CMI_Refrence_name<- read_sheet("https://docs.google.com/spreadsheets/d/1sqZxzGNsn7Zoj0a_1Kam9fZy0aquleIqEjD067qZG1M/edit#gid=527635793",
                               sheet = "CMI Refrence name")
ABC_Refrence_name<- read_sheet("https://docs.google.com/spreadsheets/d/1sqZxzGNsn7Zoj0a_1Kam9fZy0aquleIqEjD067qZG1M/edit#gid=527635793",
                               sheet = "ABC Refrence name")
for (i in 1:nrow(ABC_Refrence_name)){
    ABC_Refrence_name[i,1] <- gsub("\n", "",ABC_Refrence_name[i,1]) 
}

for (i in 1:nrow(CMI_Refrence_name)){
  CMI_Refrence_name[i,1] <- gsub("\n", "",CMI_Refrence_name[i,1]) 
}

#token <- drop_auth()
#saveRDS(token, file = "token.rds")

drop_download("national_salvage/CMI_Histo_example.rds", dtoken = token, overwrite = T)
drop_download("national_salvage/ABC_Histo_example.rds", dtoken = token, overwrite = T)

#drop_upload("CMI_Histo_example.rds",path = "national_salvage")
#drop_upload("CMI_Price_Variation.rds",path = "national_salvage")
#drop_upload("ABC_Histo_example.rds",path = "national_salvage")
#drop_upload("ABC_Price_Variation.rds",path = "national_salvage")



#CMI_Price_variation <- read_rds("CMI_Price_Variation.rds")
#ABC_Price_variation <- read_rds("ABC_Price_Variation.rds")

CMI_Price_variation <- read_rds("CMI_Histo_example.rds")
ABC_Price_variation <- read_rds("ABC_Histo_example.rds")



