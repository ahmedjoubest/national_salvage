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

# Sourcing  ------------------------------
source("helpers.R")

# Google sheet authentication ------------
gs4_auth(cache = ".secrets", email = "salvage.test1@gmail.com")