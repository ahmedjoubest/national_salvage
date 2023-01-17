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
library(lubridate)


# Sourcing  ------------------------------

source("helpers.R")


# Google sheet authentication ------------

#"https://docs.google.com/spreadsheets/d/1ABYeL_aWjM8RZcevTXWnTAyl_meV2RVTdQFKvfIeOXI/edit#gid=1594085636"

gs4_auth(cache = ".secrets", email = "salvage.test1@gmail.com")






