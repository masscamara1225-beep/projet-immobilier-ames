# global.R - chargement commun de l'application Shiny
# Projet Ames - Camara & Logbo

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
library(highcharter)
library(here)

train <- read.csv(here("data", "train_clean.csv"), stringsAsFactors = FALSE)
train$OverallQual <- factor(train$OverallQual, levels = 1:10, ordered = TRUE)

# objets partages
median_global <- median(train$SalePrice)