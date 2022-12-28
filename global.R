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

# Dropbox authentication ------------

token <- drop_auth()
# saveRDS(token, file = "token.rds")
#token<- read_rds("token.rds")

# Reading data  ------------

CMI_ABC_reference_name<- read_sheet("https://docs.google.com/spreadsheets/d/1sqZxzGNsn7Zoj0a_1Kam9fZy0aquleIqEjD067qZG1M/edit#gid=527635793",
                                    sheet = "Reference name all items")

CMI_Refrence_name<- Reference_Name_All_Items(CMI_ABC_reference_name)[[1]] %>% as.data.frame()
ABC_Refrence_name<- Reference_Name_All_Items(CMI_ABC_reference_name)[[2]] %>% as.data.frame()

for (i in 1:nrow(ABC_Refrence_name)){
    ABC_Refrence_name[i,1] <- gsub("\n", "",ABC_Refrence_name[i,1]) 
}

for (i in 1:nrow(CMI_Refrence_name)){
  CMI_Refrence_name[i,1] <- gsub("\n", "",CMI_Refrence_name[i,1]) 
}


#drop_download("National_Salvage_V_Test/CMI_Histo_example.rds", dtoken = token, overwrite = T)
#drop_download("National_Salvage_V_Test/ABC_Histo_example.rds", dtoken = token, overwrite = T)

CMI_Price_variation <- read_rds("CMI_Histo_example.rds")
ABC_Price_variation <- read_rds("ABC_Histo_example.rds")


#drop_upload("CMI_Histo_example.rds",path = "National_Salvage_V_Test")
#drop_upload("ABC_Histo_example.rds",path = "National_Salvage_V_Test")
