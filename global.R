library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
library(highcharter)
library(here)
library(shiny)
library(shinydashboard)
library(shinycssloaders)

train <- read.csv(here("data", "train_clean.csv"), stringsAsFactors = FALSE)
train$OverallQual <- factor(train$OverallQual, levels = 1:10, ordered = TRUE)

median_global <- median(train$SalePrice)

kpi_card <- function(label, value, hint = NULL, accent = NULL) {
  tags$div(
    class = "kpi",
    style = if (!is.null(accent)) paste0("--a:", accent, ";") else NULL,
    tags$div(class = "kpi-label", label),
    tags$div(class = "kpi-value", value),
    if (!is.null(hint)) tags$div(class = "kpi-hint", hint)
  )
}

card <- function(..., title = NULL) {
  tags$div(
    class = "card-min",
    if (!is.null(title)) tags$div(class = "card-min-header", title),
    tags$div(class = "card-min-body", ...)
  )
}

page_header <- function(title, meta = NULL) {
  tags$div(
    class = "ph",
    tags$h2(class = "ph-title", title),
    if (!is.null(meta)) tags$p(class = "ph-meta", meta)
  )
}

section_subtitle <- function(text) {
  tags$p(class = "section-subtitle", text)
}

note_box <- function(text) {
  tags$div(class = "note", tags$strong("Lecture · "), text)
}

COULEURS <- list(
  orange = "#E8721C",
  vert   = "#047857",
  bleu   = "#1E40AF",
  rouge  = "#B91C1C"
)

