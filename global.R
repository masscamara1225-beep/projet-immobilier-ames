## ============================================================
## global.R
## ============================================================

library(tidyverse)
library(scales)
library(plotly)
library(highcharter)
library(here)
library(shiny)
library(shinydashboard)
library(shinycssloaders)

train <- read_csv(here("data", "train_clean.csv"), show_col_types = FALSE)

train$MSSubClass  <- as.factor(train$MSSubClass)
train$MoSold      <- as.factor(train$MoSold)
train$YrSold      <- as.factor(train$YrSold)
train$OverallQual <- factor(train$OverallQual, levels = 1:10, ordered = TRUE)
train$OverallCond <- factor(train$OverallCond, levels = 1:10, ordered = TRUE)

if ("X1stFlrSF" %in% names(train)) {
  train <- train %>% rename(FirstFlrSF = X1stFlrSF, SecondFlrSF = X2ndFlrSF, ThreeSsnPorch = X3SsnPorch)
} else if ("1stFlrSF" %in% names(train)) {
  train <- train %>% rename(FirstFlrSF = `1stFlrSF`, SecondFlrSF = `2ndFlrSF`, ThreeSsnPorch = `3SsnPorch`)
}

median_global <- median(train$SalePrice)

kpi_card <- function(label, value, hint = NULL, accent = NULL) {
  tags$div(class = "kpi", style = if (!is.null(accent)) paste0("--a:", accent, ";") else NULL,
           tags$div(class = "kpi-label", label),
           tags$div(class = "kpi-value", value),
           if (!is.null(hint)) tags$div(class = "kpi-hint", hint)
  )
}

card <- function(..., title = NULL) {
  tags$div(class = "card-min",
           if (!is.null(title)) tags$div(class = "card-min-header", title),
           tags$div(class = "card-min-body", ...)
  )
}

page_header <- function(title, meta = NULL) {
  tags$div(class = "ph",
           tags$h2(class = "ph-title", title),
           if (!is.null(meta)) tags$p(class = "ph-meta", meta)
  )
}

section_subtitle <- function(text) { tags$p(class = "section-subtitle", text) }

COULEURS <- list(orange = "#E8721C", vert = "#047857", bleu = "#1E40AF", rouge = "#B91C1C", jaune = "#B45309")

labels_variables <- c(
  "SalePrice" = "Prix de vente", "GrLivArea" = "Surface habitable",
  "OverallQual" = "Qualité globale", "OverallCond" = "État général",
  "GarageCars" = "Places de garage", "GarageArea" = "Surface du garage",
  "TotalBsmtSF" = "Surface du sous-sol", "YearBuilt" = "Année de construction",
  "YearRemodAdd" = "Année de rénovation", "Neighborhood" = "Quartier",
  "BldgType" = "Type de bâtiment", "Condition1" = "Condition environnementale",
  "LotArea" = "Superficie du terrain", "LotFrontage" = "Façade sur rue",
  "FullBath" = "Salles de bain complètes", "HalfBath" = "Salles d'eau",
  "TotRmsAbvGrd" = "Nombre de pièces", "KitchenQual" = "Qualité de la cuisine",
  "ExterQual" = "Qualité extérieure", "MoSold" = "Mois de vente", "YrSold" = "Année de vente",
  "SaleCondition" = "Condition de la vente", "SaleType" = "Type de vente",
  "ScoreEnv" = "Score environnemental", "Era" = "Ère de construction",
  "IncNodePurity" = "Importance prédictive", "Variable" = "Variable",
  "FirstFlrSF" = "Surface du 1er étage", "SecondFlrSF" = "Surface du 2e étage",
  "BsmtFinSF1" = "Surface aménagée du sous-sol", "WoodDeckSF" = "Surface de terrasse",
  "OpenPorchSF" = "Surface de véranda", "Fireplaces" = "Nombre de cheminées"
)

labels_condition1 <- c(
  "Artery" = "Route artérielle", "Feedr" = "Route secondaire", "Norm" = "Normale",
  "RRNn" = "Voie ferrée proche (nord-sud)", "RRAn" = "Voie ferrée adjacente (nord-sud)",
  "PosN" = "Espace vert proche", "PosA" = "Espace vert adjacent",
  "RRNe" = "Voie ferrée proche (est-ouest)", "RRAe" = "Voie ferrée adjacente (est-ouest)"
)

labels_bldgtype <- c(
  "1Fam" = "Maison individuelle", "2fmCon" = "Bi-familiale convertie",
  "Duplex" = "Duplex", "Twnhs" = "Townhouse", "TwnhsE" = "Townhouse (extrémité)"
)

palette_radar_fixe <- c(COULEURS$vert, COULEURS$bleu, COULEURS$orange)

colonnes_pertinentes <- c("Id", "Neighborhood", "SalePrice", "ScoreEnv", "Era",
                          "OverallQual", "GrLivArea", "GarageCars", "TotalBsmtSF",
                          "YearBuilt", "KitchenQual")

revele_box <- function(content) {
  tags$div(class = "revele-wrap",
           tags$div(class = "revele-title", "Ce que ça révèle"),
           content
  )
}

viz_card <- function(titre, type, role, output_ui, revele, extra_avant = NULL, id = NULL) {
  card(title = titre,
       if (!is.null(id)) tags$div(id = id),
       output_ui,
       if (!is.null(extra_avant)) extra_avant,
       tags$div(class = "viz-caption", paste0(type, " — ", role)),
       tags$div(style = "margin-top:10px;", revele_box(revele))
  )
}

reco_card <- function(numero, profil, titre, chiffre_cle, texte, graphiques, accent, image = NULL) {
  tags$div(style = paste0("background:white; border-radius:10px; padding:20px 22px; margin-bottom:14px;
             border-left:6px solid ", accent, "; box-shadow:0 1px 5px rgba(0,0,0,.06);"),
           tags$div(style = "display:flex; justify-content:space-between; align-items:flex-start; gap:16px; flex-wrap:wrap;",
                    tags$div(style = "display:flex; align-items:center; gap:14px; flex:1; min-width:220px;",
                             if (!is.null(image)) tags$img(src = image, onerror = "this.style.display='none';",
                                                           style = "width:52px; height:52px; border-radius:50%; object-fit:cover; object-position:center; flex-shrink:0;"),
                             tags$div(
                               tags$div(style = paste0("font-size:11px; text-transform:uppercase; letter-spacing:0.5px; color:", accent, "; font-weight:700;"),
                                        paste0("R", numero, " · ", profil)),
                               tags$h4(style = "margin:4px 0 0 0; color:#18181E;", titre)
                             )
                    ),
                    tags$div(style = paste0("background:", accent, "1A; color:", accent, "; font-weight:800; font-size:19px;
                 padding:8px 16px; border-radius:8px; white-space:nowrap;"), chiffre_cle)
           ),
           tags$p(style = "color:#3A342A; font-size:13.5px; line-height:1.55; margin-top:12px;", texte),
           tags$div(style = "color:#9C8F78; font-size:11.5px; margin-top:6px;", graphiques)
  )
}

## bien_card — le conseil personnalisé s'affiche EN BAS de la carte,
## séparé visuellement par une ligne pointillée
bien_card <- function(quartier, surface, prix, qualite, accent, conseil = NULL) {
  tags$div(class = "bien-card", style = paste0("border-top:4px solid ", accent, ";"),
           tags$div(class = "bien-quartier-tag", style = paste0("background:", accent, "1A; color:", accent, ";"), quartier),
           tags$div(class = "bien-row", tags$span(class = "bien-label", "Surface"), tags$span(class = "bien-value", paste(surface, "pi²"))),
           tags$div(class = "bien-row", tags$span(class = "bien-label", "Qualité"), tags$span(class = "bien-value", paste0(qualite, "/10"))),
           tags$div(class = "bien-row", tags$span(class = "bien-label", "Prix"), tags$span(style = paste0("color:", accent, "; font-weight:800; font-size:15px;"), scales::dollar(prix))),
           if (!is.null(conseil)) tags$div(style = "margin-top:8px; padding-top:8px; border-top:1px dashed #E5E7EB; font-size:11px; color:#7A6F60;",
                                           paste0("Pertinent pour qui recherche avant tout ", conseil, "."))
  )
}

banner_img <- function(src, hauteur = "220px") {
  tags$div(style = paste0("width:100%; height:", hauteur, "; border-radius:12px; margin-bottom:16px;
             background-image:url('", src, "'); background-size:cover; background-position:center;
             background-color:#F0EBDF;"))
}

get_safe <- function(vecteur, cle, defaut) {
  if (!is.null(cle) && !is.na(cle) && cle %in% names(vecteur)) vecteur[[cle]] else defaut
}