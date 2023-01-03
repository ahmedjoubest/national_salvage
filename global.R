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
library(googledrive)
library(sparkline)
library(htmlwidgets)
library(DT)
library(rdrop2)
library(shinyalert)


# Sourcing  ------------------------------

source("helpers.R")


# Google sheet authentication ------------
#"https://docs.google.com/spreadsheets/d/1ABYeL_aWjM8RZcevTXWnTAyl_meV2RVTdQFKvfIeOXI/edit#gid=1594085636"
gs4_auth(cache = ".secrets", email = "salvage.test1@gmail.com")




CMI_ABC_Reference_name<- read_sheet("https://docs.google.com/spreadsheets/d/1ABYeL_aWjM8RZcevTXWnTAyl_meV2RVTdQFKvfIeOXI/edit#gid=0",
                                     sheet = "Reference name all items")
 
 CMI_Price_variation <- read_sheet("https://docs.google.com/spreadsheets/d/1ABYeL_aWjM8RZcevTXWnTAyl_meV2RVTdQFKvfIeOXI/edit#gid=0",
                                   sheet = "CMI Historical Data") %>% as.data.frame()
 ABC_Price_variation <- read_sheet("https://docs.google.com/spreadsheets/d/1ABYeL_aWjM8RZcevTXWnTAyl_meV2RVTdQFKvfIeOXI/edit#gid=0",
                                   sheet = "ABC Historical Data") %>% as.data.frame()
 
 rownames(CMI_Price_variation)<-CMI_Price_variation[,1]
 CMI_Price_variation<-CMI_Price_variation[,-1]
 
 rownames(ABC_Price_variation)<-ABC_Price_variation[,1]
 ABC_Price_variation<-ABC_Price_variation[,-1]
 
 Reference_Name <-Reference_Name_All_Items(CMI_ABC_Reference_name)
 CMI_Reference_name <- Reference_Name[[1]]
 ABC_Reference_name <- Reference_Name[[2]]
 CMI_ABC_Df<- Reference_Name[[3]]
 
 