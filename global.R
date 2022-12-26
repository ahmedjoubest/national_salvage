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

#CMI_Price_variation <- read_rds("CMI_Price_Variation.rds")
#ABC_Price_variation <- read_rds("ABC_Price_Variation.rds")

CMI_Price_variation <- read_rds("CMI_Histo_example.rds")
ABC_Price_variation <- read_rds("ABC_Histo_example.rds")



