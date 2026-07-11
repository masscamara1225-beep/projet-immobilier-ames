## ============================================================
## app.R — Marché Immobilier Ames
## ============================================================

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(plotly)
library(highcharter)
library(visNetwork)
library(igraph)
library(ggridges)
library(GGally)
library(randomForest)
library(fmsb)
library(corrplot)
library(scales)
library(here)
library(leaflet)
library(DT)

source("global.R")

theme_ames <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = "#FFFFFF", color = NA),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.grid.major = element_line(color = "#EDEDED", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      text = element_text(color = "#18181E"),
      axis.text  = element_text(color = "#7A6F60", size = 10),
      axis.title = element_text(color = "#18181E", size = 11),
      plot.title = element_text(color = "#18181E", face = "bold", size = 12)
    )
}

noms_quartiers <- c(
  "Blmngtn"="Bloomington Heights","Blueste"="Bluestem","BrDale"="Briardale","BrkSide"="Brookside",
  "ClearCr"="Clear Creek","CollgCr"="College Creek","Crawfor"="Crawford","Edwards"="Edwards",
  "Gilbert"="Gilbert","IDOTRR"="Iowa DOT & Rail Road","MeadowV"="Meadow Village","Mitchel"="Mitchell",
  "NAmes"="North Ames","NoRidge"="Northridge","NPkVill"="Northpark Villa","NridgHt"="Northridge Heights",
  "NWAmes"="Northwest Ames","OldTown"="Old Town","SWISU"="South & West of Iowa State University",
  "Sawyer"="Sawyer","SawyerW"="Sawyer West","Somerst"="Somerset","StoneBr"="Stone Brook",
  "Timber"="Timberland","Veenker"="Veenker"
)
choix_quartiers <- setNames(names(noms_quartiers), noms_quartiers)[order(noms_quartiers)]

logo_path <- case_when(
  file.exists("www/images/logo.png") ~ "images/logo.png",
  file.exists("www/images/logo.jpg") ~ "images/logo.jpg",
  TRUE ~ NA_character_
)

train <- train %>% mutate(
  Condition1_label = factor(recode(Condition1, !!!labels_condition1), levels = unname(labels_condition1)),
  BldgType_label = recode(BldgType, !!!labels_bldgtype),
  ScoreEnv_label = case_when(
    ScoreEnv == 2  ~ "Positif (parc)", ScoreEnv == 0  ~ "Normal",
    ScoreEnv == -1 ~ "Route secondaire", ScoreEnv == -2 ~ "Route/rail"
  )
)

cond_df <- train %>%
  group_by(Condition1_label) %>% summarise(med = median(SalePrice), Condition1 = first(Condition1)) %>%
  mutate(impact_col = case_when(
    Condition1 %in% c("PosA","PosN") ~ COULEURS$vert,
    Condition1 == "Norm"             ~ COULEURS$bleu,
    TRUE                              ~ COULEURS$rouge
  )) %>% arrange(med)

num_vars <- train %>% select(where(is.numeric)) %>% select(-Id) %>% select(SalePrice, everything())
cor_m <- cor(num_vars, use = "complete.obs")

train_rf <- train %>% select(-Id) %>% mutate(across(where(is.character), as.factor)) %>% drop_na()
rf_model <- readRDS(here("data", "rf_model.rds"))
imp_df   <- readRDS(here("data", "imp_df.rds"))

top8 <- train %>% count(Neighborhood, sort = TRUE) %>% slice_head(n = 8) %>% pull(Neighborhood)
pct_top8 <- round(100 * sum(train$Neighborhood %in% top8) / nrow(train))
bubble_data <- train %>% filter(Neighborhood %in% top8) %>% mutate(nom_complet = recode(Neighborhood, !!!noms_quartiers))

comparaison_modeles_df <- data.frame(
  Modèle = c("Régression linéaire","Stepwise (AIC)","Ridge","Lasso","Random Forest"),
  RMSE = c(34833,32649,33507,34584,28324)
) %>% arrange(RMSE) %>% mutate(RMSE = scales::dollar(RMSE))

carte_data <- readRDS(here("data", "carte_data.rds"))

profil_quartiers <- train %>% group_by(Neighborhood) %>%
  summarise(prix = median(SalePrice), surface = median(GrLivArea),
            qualite = median(as.numeric(OverallQual)), garage = median(GarageCars), recence = median(YearBuilt))

matrice_scaled <- profil_quartiers %>% select(-Neighborhood) %>% scale()
rownames(matrice_scaled) <- profil_quartiers$Neighborhood

hc <- hclust(dist(matrice_scaled), method = "ward.D2")
clusters_quartiers <- cutree(hc, k = 3)
stopifnot("Le clustering ne produit pas 3 groupes distincts" = length(unique(clusters_quartiers)) == 3)

prix_moyen_par_cluster <- sapply(sort(unique(clusters_quartiers)), function(cl) {
  mean(profil_quartiers$prix[clusters_quartiers == cl])
})
names(prix_moyen_par_cluster) <- as.character(sort(unique(clusters_quartiers)))
ordre_cl <- order(-prix_moyen_par_cluster)
labels_cluster <- setNames(c("Premium", "Moyen", "Abordable"), names(prix_moyen_par_cluster)[ordre_cl])

stopifnot("Un label de segment manque" = all(c("Premium","Moyen","Abordable") %in% labels_cluster))
stopifnot("Un quartier reste sans segment" = !anyNA(labels_cluster[as.character(clusters_quartiers)]))

ordre_dessin_clusters <- unique(clusters_quartiers[hc$order])
couleurs_par_label <- c("Premium" = COULEURS$vert, "Moyen" = COULEURS$bleu, "Abordable" = COULEURS$orange)
couleurs_cluster_dessin <- sapply(as.character(ordre_dessin_clusters),
                                  function(cl) get_safe(couleurs_par_label, labels_cluster[cl], COULEURS$orange))

segments_df <- profil_quartiers %>%
  mutate(cluster = clusters_quartiers[Neighborhood],
         label_segment = labels_cluster[as.character(cluster)],
         nom_complet = recode(Neighborhood, !!!noms_quartiers))

representants_segments <- segments_df %>%
  group_by(label_segment) %>% slice_max(prix, n = 1, with_ties = FALSE) %>% ungroup()

audit_nettoyage <- data.frame(
  Variable = c("PoolQC","MiscFeature","Alley","Fence","FireplaceQu",
               "GarageType/Finish/Qual/Cond","GarageYrBlt","BsmtQual/Cond/Exposure/FinType1/2",
               "LotFrontage","MasVnrType/Area","Electrical"),
  NA_avant = c("1453 (99.5%)","1406 (96.3%)","1369 (93.8%)","1179 (80.8%)","690 (47.3%)",
               "81 (5.5%)","81 (5.5%)","37-38 (2.5%)","259 (17.7%)","8 (0.5%)","1 (0.07%)"),
  Traitement = c("Absent","Absent","Absent","Absent","Absent","Absent","Année de construction",
                 "Absent","Médiane du quartier","Absent / 0","Mode (SBrkr)")
)

palette_quartiers <- setNames(
  colorRampPalette(c(COULEURS$orange, COULEURS$bleu, "#7A6F60", COULEURS$vert, "#B45309", "#5B4B8A"))(25),
  sort(unique(noms_quartiers))
)
palette_bldgtype <- setNames(c("#E8721C","#1E40AF","#7A6F60","#047857","#B45309"), unname(labels_bldgtype))
palette_era <- setNames(colorRampPalette(c("#FBDFC0", COULEURS$orange))(5), levels(train$Era))
palette_condition9 <- setNames(colorRampPalette(c(COULEURS$vert, COULEURS$bleu, COULEURS$orange, COULEURS$rouge))(9), unname(labels_condition1))
images_segment <- c("Premium"="images/quartier_premium.png","Moyen"="images/quartier_moyen.png","Abordable"="images/quartier_abordable.png")
accents_biens <- c(COULEURS$bleu, COULEURS$vert, COULEURS$orange, "#5B4B8A", "#7A6F60")

## ============================================================
## UI
## ============================================================
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(disable = TRUE),
  
  dashboardSidebar(
    width = 240,
    tags$div(class = "sidebar-brand",
             if (!is.na(logo_path)) {
               tags$img(class = "logo-img", src = logo_path)
             } else {
               tags$div(class = "logo-placeholder", "IA")
             },
             tags$div(tags$div(class = "brand-main", "Immobilier Ames"), tags$div(class = "brand-sub", "Enquête sur le prix des maisons")),
             tags$div(id = "sidebar-hamburger-btn", class = "sidebar-hamburger", tags$span(), tags$span(), tags$span())
    ),
    sidebarMenu(
      id = "main_tabs",
      tags$div(class = "side-section", tags$span("L'enquête")),
      menuItem("Accueil", tabName = "accueil", icon = icon("house")),
      menuItem("Acte 0 · Préparation", tabName = "acte0", icon = icon("magnifying-glass")),
      menuItem("Acte 1 · Marché", tabName = "acte1", icon = icon("chart-column")),
      menuItem("Acte 2 · Géographie", tabName = "acte2", icon = icon("map")),
      menuItem("Acte 3 · Environnement", tabName = "acte3", icon = icon("tree")),
      menuItem("Acte 4 · Facteurs", tabName = "acte4", icon = icon("magnifying-glass-chart")),
      menuItem("Acte 5 · Temporel", tabName = "acte5", icon = icon("clock")),
      tags$div(class = "side-section", tags$span("Analyse avancée")),
      menuItem("Prédiction", tabName = "ml", icon = icon("robot")),
      menuItem("Recommandations", tabName = "reco", icon = icon("lightbulb")),
      selectInput("quartiers_filtre", tagList(icon("magnifying-glass"), " Filtrer par quartier :"),
                  choices = c("Tous" = "Tous", choix_quartiers), selected = "Tous"),
      tags$div(class = "filtre-hint", "S'applique aux graphiques des Actes 1 et 3.")
    )
  ),
  
  dashboardBody(
    tags$div(id = "splash-screen",
             style = "position:fixed; inset:0; z-index:9999; overflow:hidden; display:flex; flex-direction:column;
               align-items:center; justify-content:center; color:white; text-align:center;",
             tags$div(style = paste0("font-size:11px; text-transform:uppercase; letter-spacing:2px; color:", COULEURS$orange,
                                     "; font-weight:700; margin-bottom:12px;"), "Data Science.IA · UFRMI · 2025-2026"),
             tags$h1("Immobilier Ames", style = "font-size:44px; font-weight:800; margin:0;"),
             tags$p("Une enquête sur le prix des maisons, 1 460 transactions, 5 actes",
                    style = "font-size:15px; color:#D8D0BE; margin-top:10px;"),
             tags$div(class = "splash-ring"),
             tags$script(HTML("
            setTimeout(function() {
              var s = document.getElementById('splash-screen');
              if (s) { s.style.transition = 'opacity 0.5s'; s.style.opacity = '0';
                       setTimeout(function(){ s.style.display = 'none'; }, 500); }
            }, 3000);
            $(document).on('click', '#sidebar-hamburger-btn', function() { $('body').toggleClass('sidebar-minimized'); });
            var lastWidth = window.innerWidth;
            function adaptSidebar() {
              if (window.innerWidth < 992) { $('body').addClass('sidebar-minimized'); }
              else { $('body').removeClass('sidebar-minimized'); }
            }
            $(document).ready(adaptSidebar);
            $(window).on('resize', function() {
              if (Math.abs(window.innerWidth - lastWidth) > 50) {
                lastWidth = window.innerWidth;
                adaptSidebar();
              }
            });
          "))
    ),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = paste0("style.css?v=", as.numeric(Sys.time())))),
    
    tabItems(
      
      ## ---------- ACCUEIL ----------
      tabItem(tabName = "accueil",
              uiOutput("hero_carousel"),
              page_header(title = "Une maison peut valoir 21 fois une autre",
                          meta = "Ames, Iowa. 1 460 transactions. 2006-2010. Kaggle House Prices"),
              section_subtitle("En 2007, une maison à Ames valait 755 000 dollars. La même année, une autre valait 35 000 dollars. Même ville, 5 kilomètres d'écart. Notre enquête parcourt 1 460 transactions et 80 variables pour répondre à une seule question. Qu'est-ce qui fait le prix d'une maison ?"),
              tags$div(style = "display:flex; gap:16px; flex-wrap:wrap;",
                       tags$div(style="flex:1;min-width:150px;", kpi_card("Prix médian", "163 000 $", hint="Référence marché", accent=COULEURS$bleu)),
                       tags$div(style="flex:1;min-width:150px;", kpi_card("Quartier le plus cher", "Northridge Heights", hint="315 000 $ médian", accent=COULEURS$vert)),
                       tags$div(style="flex:1;min-width:150px;", kpi_card("Surface médiane", "1 515 ft²", hint="Maison typique", accent=COULEURS$orange)),
                       tags$div(style="flex:1;min-width:150px;", kpi_card("Qualité moyenne", "6.1 / 10", hint="Qualité globale", accent=COULEURS$jaune)),
                       tags$div(style="flex:1;min-width:150px;", kpi_card("Amplitude", "×21", hint="de 35 000 à 755 000 $", accent=COULEURS$rouge))
              ),
              fluidRow(
                column(6, card(title = "Prix médian par segment de marché", withSpinner(highchartOutput("apercu_segments", height = 300), type = 6))),
                column(6, card(title = "Composition du marché par type de bien", withSpinner(plotlyOutput("apercu_donut", height = 300), type = 6)))
              ),
              fluidRow(
                column(6, card(title = "Classement des 5 quartiers les plus chers", uiOutput("mini_classement"))),
                column(6, card(title = "Volume de ventes par mois", withSpinner(highchartOutput("apercu_barres_mois", height = 300), type = 6)))
              ),
              fluidRow(column(12, card(title = "L'enquête, acte par acte",
                                       tags$ul(class = "obj-list",
                                               tags$li(strong("Acte 0."), " Comment le dossier a été préparé"),
                                               tags$li(strong("Acte 1. Quoi ?"), " Ce qui se vend, types de biens, quartiers, qualité"),
                                               tags$li(strong("Acte 2. Où ?"), " La géographie des prix, écart de 3.9 fois entre quartiers"),
                                               tags$li(strong("Acte 3. Environnement."), " Prime parc de 41%, décote route de 28%"),
                                               tags$li(strong("Acte 4. Pourquoi ?"), " Les vrais facteurs, qualité, surface, garage"),
                                               tags$li(strong("Acte 5. Quand ?"), " La crise de 2008 et la saisonnalité des ventes")
                                       )))),
              fluidRow(column(12, tags$h3(class = "section-title", "Parcours guidé"))),
              fluidRow(
                column(4, actionLink("nav_acte0", class="parcours-card", tags$div(tags$h4(icon("magnifying-glass")," Acte 0"), tags$p("Comment les données ont été préparées.")))),
                column(4, actionLink("nav_acte1", class="parcours-card", tags$div(tags$h4(icon("chart-column")," Acte 1 · Quoi ?"), tags$p("Ce qui se vend, types de biens, quartiers et qualité.")))),
                column(4, actionLink("nav_acte2", class="parcours-card", tags$div(tags$h4(icon("map")," Acte 2 · Où ?"), tags$p("La géographie des prix, écart de 3.9 fois entre quartiers."))))
              ),
              fluidRow(
                column(4, actionLink("nav_acte3", class="parcours-card", tags$div(tags$h4(icon("tree")," Acte 3 · Environnement"), tags$p("Prime parc de 41%, décote route de 28%.")))),
                column(4, actionLink("nav_acte4", class="parcours-card", tags$div(tags$h4(icon("magnifying-glass-chart")," Acte 4 · Pourquoi ?"), tags$p("Les vrais facteurs du prix.")))),
                column(4, actionLink("nav_acte5", class="parcours-card", tags$div(tags$h4(icon("clock")," Acte 5 · Quand ?"), tags$p("La crise de 2008 et la saisonnalité."))))
              )
      ),
      
      ## ---------- ACTE 0 ----------
      tabItem(tabName = "acte0",
              page_header(title = "Comment le dossier de l'enquête a été préparé", meta = "Acte 0. Description des données. Diagnostic. Nettoyage"),
              section_subtitle("Avant de raconter ce que révèlent les données, il faut montrer d'où elles viennent."),
              tabsetPanel(id = "acte0_subtabs", type = "tabs",
                          
                          tabPanel("Méthodologie", br(),
                                   banner_img("images/methodologie.png", hauteur = "240px"),
                                   card(title = "Le jeu de données",
                                        p("Ames Housing Dataset, publié par Dean De Cock en 2011, utilisé par la compétition Kaggle House Prices."),
                                        p("1 460 transactions immobilières résidentielles à Ames, Iowa, entre 2006 et 2010. 80 variables explicatives, plus la variable cible, le prix de vente."),
                                        revele_box(tagList(
                                          tags$p("Le jeu de données mélange des variables numériques continues, catégorielles nominales et catégorielles ordinales."),
                                          tags$p("Cette diversité impose des choix de nettoyage différenciés selon le type de variable, plutôt qu'un traitement uniforme appliqué à toutes les colonnes.")
                                        ))
                                   ),
                                   fluidRow(
                                     column(4, kpi_card("Observations", "1 460", hint="Transactions d'origine", accent=COULEURS$bleu)),
                                     column(4, kpi_card("Variables", "83", hint="81 d'origine, 2 créées", accent=COULEURS$vert)),
                                     column(4, kpi_card("Valeurs manquantes", "0", hint="Après traitement complet", accent=COULEURS$orange))
                                   ),
                                   card(title = "Le vide n'est pas toujours un manque",
                                        tags$div(style = "margin-bottom:12px;", tableOutput("table_nettoyage")),
                                        revele_box(tagList(
                                          tags$p("Le codebook officiel précise, variable par variable, que l'absence de valeur signifie l'absence physique de l'élément lui-même, pas une mesure ratée."),
                                          tags$p("Une valeur vide sur la qualité de la piscine, la clôture ou la cheminée n'est donc jamais un trou dans la collecte, c'est une réponse en soi : cette maison n'a simplement pas cet équipement."),
                                          tags$p("Les 5 variables les plus touchées, jusqu'à 99.5% de vide, sont recodées en catégorie « Absent » plutôt que devinées par une moyenne, ce qui aurait fabriqué une fausse donnée."),
                                          tags$p("LotFrontage suit une logique différente : la façade existe bel et bien mais n'a pas été mesurée, la médiane du quartier est donc le bon choix, deux maisons voisines ayant des terrains comparables."),
                                          tags$p("Cette rigueur conditionne la fiabilité du Random Forest de l'Acte 4 : une valeur inventée aurait introduit un signal artificiel entre 'donnée manquante' et prix, faussant l'apprentissage du modèle.")
                                        ))
                                   ),
                                   card(title = "Deux variables créées pour l'enquête",
                                        p(strong("Score environnemental."), " De moins 2 (route artérielle) à plus 2 (parc)."),
                                        p(strong("Ère de construction."), " 5 tranches historiques de l'année de construction."),
                                        revele_box(tagList(
                                          tags$p("Ces deux variables n'existaient pas dans le jeu de données brut."),
                                          tags$p("Le score environnemental recode la condition environnementale, 9 modalités textuelles peu exploitables, en une échelle numérique ordonnée qui rend possible l'Acte 3."),
                                          tags$p("Le découpage en ères regroupe 112 années de construction distinctes en 5 catégories interprétables, ce qui rend possible l'Acte 5.")
                                        ))
                                   ),
                                   card(title = "Deux valeurs retirées avant la modélisation",
                                        p("Deux transactions affichent plus de 4 000 pieds carrés pour un prix inférieur à 300 000 dollars, une anomalie documentée du jeu de données."),
                                        revele_box(tagList(
                                          tags$p("Ces deux lignes sont des ventes atypiques ou des erreurs de saisie, identifiées publiquement par la communauté ayant étudié ce dataset."),
                                          tags$p("Les retirer avant l'entraînement a amélioré la cohérence des prédictions, 1 458 observations restantes sur 1 460.")
                                        ))
                                   )
                          ),
                          
                          tabPanel("Données nettoyées", br(),
                                   card(title = "Aperçu des variables clés après nettoyage",
                                        downloadButton("telecharger_donnees", "Télécharger train_clean.csv (83 variables)", class = "btn-default"),
                                        tags$div(style = "margin-top:16px;", DTOutput("table_donnees_completes")),
                                        revele_box(tagList(
                                          tags$p("Ce tableau affiche 11 colonnes : l'identifiant, le quartier, la variable cible, les deux variables créées, et les 6 facteurs les plus prédictifs identifiés par le Random Forest en Acte 4."),
                                          tags$p("Le bouton télécharge le fichier complet, 1 460 observations et 83 variables, tel qu'utilisé pour l'ensemble des analyses de ce dashboard.")
                                        ))
                                   )
                          )
              )
      ),
      
      ## ---------- ACTE 1 ----------
      tabItem(tabName = "acte1",
              page_header(title = "Quoi se vend sur ce marché", meta = "Acte 1. Types de biens. Quartiers. Qualité"),
              section_subtitle("Le marché d'Ames est dominé à 83% par des maisons individuelles."),
              tabsetPanel(id = "acte1_subtabs", type = "tabs",
                          
                          tabPanel("Distribution", br(),
                                   viz_card(titre = "80% des maisons se vendent entre 80 000 et 300 000 dollars",
                                            type = "Histogramme", role = "Distribution du prix de vente",
                                            output_ui = withSpinner(plotlyOutput("histo_prix", height = 420), type = 6),
                                            revele = tagList(
                                              tags$p("La majorité des maisons à Ames se vendent dans une fourchette serrée allant de 80 000 à 300 000 dollars, formant un bloc massif avant de s'étirer longuement vers les prix élevés."),
                                              tags$p("Cette forme, appelée asymétrie positive, s'explique par la nature d'une ville moyenne : un socle massif de logements standards pour les familles et le personnel universitaire, face à un marché de luxe minoritaire mais très coûteux."),
                                              tags$p("C'est ce petit groupe de maisons très chères qui tire artificiellement la moyenne vers le haut, à 181 000 dollars, loin de ce que vit la majorité des acheteurs."),
                                              tags$p("La médiane, à 163 000 dollars, sépare le marché en deux parts strictement égales et représente donc la réalité du marché de masse sans être faussée par les transactions exceptionnelles."),
                                              tags$p("C'est pourquoi la médiane, pas la moyenne, sert de référence chiffrée dans le reste de cette enquête.")
                                            )
                                   )),
                          
                          tabPanel("Types de biens", br(),
                                   viz_card(titre = "Les maisons individuelles couvrent tous les segments",
                                            type = "Courbe de densité", role = "Distribution du prix par type de bien",
                                            output_ui = withSpinner(plotOutput("density_prix", height = 380), type = 6),
                                            extra_avant = wellPanel(class = "well",
                                                                    checkboxGroupInput("types", "Filtrer par type de bien :",
                                                                                       choiceNames = unname(labels_bldgtype), choiceValues = names(labels_bldgtype),
                                                                                       selected = names(labels_bldgtype), inline = TRUE)),
                                            revele = tagList(
                                              tags$p("Le prix des maisons individuelles est très étalé et couvre tous les budgets, du plus bas au plus haut, tandis que le prix des duplex se concentre et se bloque brusquement autour de 200 000 dollars."),
                                              tags$p("Cet écart tient à l'usage du bien : un duplex est presque toujours acheté dans une logique d'investissement locatif, où le prix d'achat est plafonné par la rentabilité des loyers espérés."),
                                              tags$p("La maison individuelle, elle, répond à un achat de vie familiale où les acheteurs acceptent de payer une prime émotionnelle pour le confort, l'espace ou le standing."),
                                              tags$p("Un duplex se négocie donc avec des arguments comptables de rendement, une maison individuelle avec des critères de qualité de vie et de prestige."),
                                              tags$p("Ces deux logiques commerciales, radicalement différentes, expliquent pourquoi les deux courbes n'ont ni la même forme ni la même étendue.")
                                            )
                                   ),
                                   viz_card(titre = "8 maisons sur 10 vendues à Ames sont individuelles",
                                            type = "Diagramme circulaire", role = "Composition du marché par type de bien",
                                            output_ui = withSpinner(plotlyOutput("donut_types", height = 380), type = 6),
                                            revele = tagList(
                                              tags$p("Le parc immobilier d'Ames est marqué par une domination écrasante des maisons individuelles, qui représentent plus de 83% de toutes les transactions."),
                                              tags$p("Les appartements et maisons mitoyennes restent des marchés très minoritaires en comparaison."),
                                              tags$p("Cette répartition reflète le modèle d'étalement urbain américain typique, où la priorité historique a été donnée aux pavillons avec jardin plutôt qu'à la densification, la ville s'étant développée horizontalement autour de son pôle universitaire."),
                                              tags$p("Le marché immobilier d'Ames manque donc de diversité structurelle : ses tendances globales de prix sont dictées par la seule santé du segment des maisons individuelles."),
                                              tags$p("Les autres types de biens, moins liquides, se vendent moins souvent, ce qui rend leurs prix plus volatils et plus difficiles à estimer avec précision.")
                                            )
                                   )),
                          
                          tabPanel("Quartiers", br(),
                                   viz_card(titre = "Northridge Heights est 4 fois plus cher que Briardale",
                                            type = "Diagramme en sucette", role = "Classement des quartiers par prix médian",
                                            output_ui = withSpinner(plotOutput("lollipop_quartiers", height = 520), type = 6),
                                            revele = tagList(
                                              tags$p("L'écart de prix médian entre quartiers va du simple au quadruple, de moins de 80 000 dollars dans le secteur le plus accessible à plus de 315 000 dollars dans le plus huppé."),
                                              tags$p("Les prix ne progressent pas de façon fluide mais par paliers successifs bien marqués."),
                                              tags$p("Cela indique que le marché d'Ames n'est pas un bloc uniforme où les prix baisseraient régulièrement en s'éloignant du centre, mais une ville découpée en micro-marchés relativement étanches."),
                                              tags$p("Chaque quartier possède sa propre réputation, ses écoles et son niveau de vie, créant de véritables barrières financières à l'entrée."),
                                              tags$p("La localisation est donc l'un des critères les plus puissants : le seul nom du quartier capture à lui seul un ensemble d'informations invisibles sur une maison, comme le prestige d'une rue ou la qualité du voisinage direct.")
                                            )
                                   )),
                          
                          tabPanel("Qualité", br(),
                                   viz_card(titre = "La qualité 10 sur 10 vaut 7 fois plus que la 1 sur 10",
                                            type = "Diagramme en violon", role = "Prix selon le niveau de qualité globale",
                                            output_ui = withSpinner(plotOutput("violin_qualite", height = 520), type = 6),
                                            revele = tagList(
                                              tags$p("Le prix augmente à chaque progression de la note de qualité globale, mais la dispersion des prix reste faible pour les notes basses et s'étire fortement pour les notes élevées."),
                                              tags$p("Lorsque la qualité est médiocre, note de 1 à 4, le prix reste bloqué près d'un plancher : l'acheteur ne paie quasiment que la valeur du terrain et des matériaux de base."),
                                              tags$p("Dès que l'on atteint le haut de gamme, notes de 8 à 10, la qualité agit comme un accélérateur de valeur qui démultiplie l'importance de toutes les autres caractéristiques de la maison."),
                                              tags$p("Une grande surface ou un beau terrain ne prennent leur pleine valeur que si la maison elle-même est d'un standing excellent."),
                                              tags$p("Sur le segment du luxe, la moindre variation de qualité se traduit donc par des dizaines de milliers de dollars d'écart, tant la sensibilité des acheteurs aisés à la finition est forte.")
                                            )
                                   ))
              )
      ),
      
      ## ---------- ACTE 2 ----------
      tabItem(tabName = "acte2",
              page_header(title = "Où sont les maisons les plus chères", meta = "Acte 2. Dispersion. Composition. Profils"),
              section_subtitle("Northridge Heights affiche 315 000 dollars médian, Briardale 80 000 dollars."),
              tabsetPanel(id = "acte2_subtabs", type = "tabs",
                          
                          tabPanel("Dispersion", br(),
                                   viz_card(titre = "Northridge Heights a 3 fois plus de ventes premium",
                                            type = "Boîte à moustaches", role = "Dispersion des prix par quartier",
                                            output_ui = withSpinner(highchartOutput("boxplot_quartiers", height = 500), type = 6),
                                            revele = tagList(
                                              tags$p("Certains quartiers affichent des prix très regroupés, presque toutes les maisons s'y vendant au même niveau, tandis que les quartiers les plus chers montrent des écarts internes considérables."),
                                              tags$p("Ce contraste traduit un tri spatial volontaire : les acheteurs à très haut pouvoir d'achat se regroupent dans des secteurs exclusifs."),
                                              tags$p("À l'intérieur de ces zones haut de gamme, la compétition intense entre acquéreurs fait s'envoler les prix des propriétés dotées d'options personnalisées ou de superficies supérieures."),
                                              tags$p("C'est ce qui crée les écarts spectaculaires observés au sommet de la distribution, les points isolés au-dessus des boîtes représentant des ventes exceptionnelles."),
                                              tags$p("Dans les quartiers riches, le prix moyen n'est donc pas une garantie : une estimation fiable y exige une analyse minutieuse des options intérieures spécifiques, pour ne pas sous-évaluer un bien d'exception.")
                                            )
                                   )),
                          
                          tabPanel("Composition du marché", br(),
                                   viz_card(titre = "College Creek concentre 10% des ventes",
                                            type = "Carte proportionnelle", role = "Volume et prix par quartier",
                                            output_ui = withSpinner(highchartOutput("treemap_quartiers", height = 450), type = 6),
                                            revele = tagList(
                                              tags$p("College Creek et North Ames concentrent à eux seuls plus d'un cinquième de toutes les transactions de la ville, quand d'autres quartiers n'enregistrent que de très rares ventes."),
                                              tags$p("Ces quartiers géants représentent les poumons immobiliers d'Ames : des zones résidentielles abordables, bien situées, proches des axes de transport ou de l'université."),
                                              tags$p("Le renouvellement des familles et des étudiants y est le plus fluide, ce qui leur confère une forte liquidité, les maisons s'y vendant vite et en grand nombre."),
                                              tags$p("Ces gros quartiers servent de baromètres pour le marché local : ce sont eux qui indiquent en premier si le marché est en train de monter ou de descendre."),
                                              tags$p("Leur volume de données stable et représentatif en fait des indicateurs plus fiables que les quartiers à faible rotation.")
                                            )
                                   ),
                                   viz_card(titre = "Northpark Villa est presque entièrement composé de townhouses",
                                            type = "Diagramme en barres empilées", role = "Composition typologique des quartiers",
                                            output_ui = withSpinner(plotlyOutput("stacked_composition", height = 480), type = 6),
                                            revele = tagList(
                                              tags$p("Northpark Villa, tout à gauche, est composé exclusivement de townhouses (Twnhs et TwnhsE, maisons mitoyennes en rangée) : un micro-quartier dessiné autour d'un seul produit immobilier."),
                                              tags$p("Résultat contre-intuitif : les quartiers les plus mixtes ne sont pas les plus anciens mais les plus récents — Somerset et Northridge Heights combinent 1Fam (maisons individuelles) et townhouses, leurs promoteurs ayant planifié plusieurs gammes de biens pour élargir la clientèle."),
                                              tags$p("À l'inverse, les quartiers pavillonnaires des années 1960-1990, comme Northridge ou Gilbert, sont composés quasi exclusivement de 1Fam : c'est l'ère du zonage résidentiel strict."),
                                              tags$p("Old Town garde la trace d'une autre époque : environ une vente sur huit y concerne une 2fmCon (bi-familiale convertie, grande maison divisée en deux logements), héritage du tissu urbain d'avant le zonage."),
                                              tags$p("La typologie d'un quartier renseigne ainsi sur son époque de construction et sur le degré de standardisation de son offre.")
                                            )
                                   )),
                          
                          tabPanel("Profils", br(),
                                   viz_card(titre = "Un profil type par segment de marché",
                                            type = "Graphique en radar", role = "Profil du quartier le plus représentatif de chaque segment",
                                            output_ui = withSpinner(plotOutput("radar_segments", height = 500), type = 6),
                                            revele = tagList(
                                              tags$p("Le segment Premium se distingue nettement des deux autres sur les 5 dimensions simultanément, prix, surface, qualité, garage et année de construction."),
                                              tags$p("Le segment Abordable reste resserré près du centre du graphique, avec un profil beaucoup plus modeste sur chacun de ces axes."),
                                              tags$p("Cette hiérarchie visuelle confirme que le prix premium ne tient pas à un seul facteur isolé, mais à une accumulation d'avantages sur plusieurs dimensions en même temps."),
                                              tags$p("Un quartier peut afficher un prix élevé sans dominer sur tous les axes, ce qui signale que sa prime provient d'un facteur précis plutôt que d'un profil globalement supérieur."),
                                              tags$p("Ce type de lecture aide à distinguer un quartier cher parce que réellement complet, d'un quartier cher pour une seule raison, comme la rareté ou la localisation.")
                                            )
                                   )),
                          
                          tabPanel("Comparateur", br(),
                                   viz_card(titre = "Comparez deux quartiers sur 5 dimensions",
                                            type = "Graphique en radar", role = "Comparateur interactif de profils de quartiers",
                                            output_ui = tagList(
                                              fluidRow(column(6, selectInput("quartier_a","Quartier A", choices=choix_quartiers, selected="NridgHt")),
                                                       column(6, selectInput("quartier_b","Quartier B", choices=choix_quartiers, selected="BrDale"))),
                                              withSpinner(plotOutput("radar_comparaison", height = 450), type = 6)
                                            ),
                                            revele = textOutput("revele_comparaison")
                                   )),
                          
                          tabPanel("Segmentation", br(),
                                   viz_card(titre = "3 segments naturels de quartiers",
                                            type = "Dendrogramme", role = "Clustering hiérarchique des quartiers",
                                            output_ui = withSpinner(plotOutput("dendrogramme_quartiers", height = 480), type = 6),
                                            revele = tagList(
                                              tags$p("Chaque feuille de l'arbre est un quartier ; plus deux feuilles se rejoignent bas, proche de l'axe horizontal, plus leurs profils sur les 5 dimensions se ressemblent."),
                                              tags$p("Couper l'arbre à 3 branches fait apparaître les 3 groupes coloriés, correspondant exactement à la composition du tableau ci-dessous."),
                                              tags$p("Cette partition confirme statistiquement que les quartiers d'Ames ne se répartissent pas au hasard, mais selon une logique de marché cohérente et mesurable."),
                                              tags$p("Un quartier n'appartient qu'à un seul segment, déterminé par la distance de son profil complet à celui des autres, pas seulement par son prix affiché."),
                                              tags$p("Cette méthode objective vient renforcer, par une preuve statistique indépendante, ce que l'observation visuelle des prix suggérait déjà.")
                                            )
                                   ),
                                   card(title = "Composition des 3 segments", uiOutput("table_segments"))
                          ),
                          
                          tabPanel("Carte", br(),
                                   viz_card(titre = "Les quartiers premium se concentrent au nord de la ville",
                                            type = "Carte géographique", role = "Localisation des prix par quartier",
                                            output_ui = withSpinner(leafletOutput("carte_quartiers", height = 500), type = 6),
                                            revele = tagList(
                                              tags$p("La cartographie met en évidence une fracture géographique nette à Ames : les zones de plus grande valeur se regroupent au nord et au nord-ouest, tandis que le centre historique et le sud affichent des valeurs plus modestes."),
                                              tags$p("Northridge et Northridge Heights sont des développements résidentiels planifiés des années 1990 à 2000, construits avec des parcs intégrés dès l'origine et positionnés volontairement loin de l'agitation du campus universitaire."),
                                              tags$p("Cette urbanisation récente et paysagère explique une bonne part de la prime de prix observée au nord de la ville."),
                                              tags$p("D'autres sources évoquent des mécanismes historiques plus anciens de ségrégation résidentielle pour expliquer ce type de fracture dans certaines villes américaines, mais aucune preuve vérifiée ne documente un tel mécanisme spécifiquement à Ames."),
                                              tags$p("L'explication la plus solidement établie ici reste donc le calendrier de développement urbain, plutôt qu'une cause historique non confirmée pour cette ville précise.")
                                            )
                                   ))
              )
      ),
      
      ## ---------- ACTE 3 ----------
      tabItem(tabName = "acte3",
              page_header(title = "L'environnement crée-t-il une prime ou une décote", meta = "Acte 3. Condition environnementale. Prime parc de 41%. Décote route de 28%"),
              section_subtitle("Vivre près d'une route artérielle coûte jusqu'à 68 000 dollars de moins qu'à proximité d'un parc."),
              tabsetPanel(id = "acte3_subtabs", type = "tabs",
                          
                          tabPanel("Impact sur le prix", br(),
                                   viz_card(titre = "La proximité d'un parc coûte jusqu'à 68 000 dollars de plus à l'achat",
                                            type = "Diagramme en barres", role = "Prix médian selon la condition environnementale",
                                            output_ui = withSpinner(highchartOutput("barres_condition", height = 420), type = 6),
                                            revele = tagList(
                                              tags$p("Sur ce graphique, construit sur la variable Condition1, l'écart est déjà net : Adjacent parc (PosA) à 212 500 $ contre Route artérielle (Artery) à 119 550 $, autour de la référence du marché à 163 000 $ (pointillé)."),
                                              tags$p("Le chiffre phare de l'enquête va plus loin : le dataset décrit chaque bien par deux variables de condition (Condition1 et Condition2). En comptant comme adjacente à un parc toute maison signalée par l'une ou l'autre, 9 maisons sont concernées, pour une médiane de 235 000 $ — soit une prime de 41 % sur les ventes normales, quand l'artère inflige une décote de 28 %."),
                                              tags$p("Un œil attentif remarquera la barre la plus haute : Proche voie ferrée N-S (RRNn). Elle ne repose que sur 5 ventes, dont deux maisons neuves de haute qualité à Somerset, quartier premium du nord : ce n'est pas le train qui valorise ces biens, c'est leur quartier."),
                                              tags$p("Sur les catégories robustes, la voie ferrée décote bien moins qu'une grande route : le passage d'un train est intermittent, le flux automobile est une nuisance continue."),
                                              tags$p("Ce n'est donc pas la présence d'une infrastructure qui pénalise le prix, mais la fréquence et l'intensité de la gêne qu'elle génère au quotidien.")
                                            )
                                   ),
                                   viz_card(titre = "À surface égale, la proximité d'un parc coûte plus cher",
                                            type = "Nuage de points", role = "Surface habitable et prix selon la condition environnementale",
                                            output_ui = withSpinner(plotlyOutput("scatter_condition", height = 420), type = 6),
                                            revele = tagList(
                                              tags$p("Chaque point du graphique est une maison, placée selon sa surface (axe horizontal) et son prix (axe vertical), colorée selon son environnement (variable Condition1). Si l'environnement ne comptait pas, les couleurs seraient mélangées partout."),
                                              tags$p("Or, à surface comparable, les points « parc » (en vert) se placent presque toujours au-dessus des points « routes » (en rouge) : le même espace se vend plus cher au calme — environ 106 $ le ft² près d'un parc, contre 96 $ près d'un axe routier."),
                                              tags$p("Autrement dit, l'environnement ne change pas la maison : il change la valeur de chacun de ses pieds carrés. Plus la maison est grande, plus l'avantage du parc pèse lourd en dollars."),
                                              tags$p("C'est pourquoi deux maisons de taille identique peuvent afficher des prix très différents : l'acheteur ne paie pas seulement des murs, il paie aussi ce qui les entoure."),
                                              tags$p("La leçon pour un acheteur est simple : comparer des prix sans comparer les environnements n'a pas de sens.")
                                            )
                                   )),
                          
                          tabPanel("Rareté des espaces verts", br(),
                                   viz_card(titre = "Seulement 2% des maisons bénéficient d'un espace vert",
                                            type = "Diagramme en rose", role = "Fréquence des conditions environnementales",
                                            output_ui = withSpinner(plotOutput("nightingale_condition", height = 480), type = 6),
                                            revele = tagList(
                                              tags$p("Plus de 85% des maisons de la ville sont construites dans un environnement standard, sans aucun aménagement paysager particulier."),
                                              tags$p("Moins de 2% des propriétés bénéficient d'un accès direct à un parc ou un espace vert protégé."),
                                              tags$p("Cette disproportion illustre la loi fondamentale de l'offre et de la demande : la nature en ville est une ressource rare et figée, une municipalité ne pouvant pas créer de nouveaux grands parcs au milieu de zones déjà construites."),
                                              tags$p("Comme le nombre de maisons bénéficiant de cet avantage reste infime par rapport à la demande, les acheteurs les plus fortunés entrent en concurrence directe pour ces biens rares."),
                                              tags$p("Cette compétition fait monter les enchères et sanctuarise durablement la valeur de ces propriétés d'exception.")
                                            )
                                   ),
                                   viz_card(titre = "2 maisons sur 100 bénéficient d'un espace vert",
                                            type = "Diagramme en gaufre", role = "Proportion des maisons par condition environnementale",
                                            output_ui = withSpinner(plotOutput("waffle_condition", height = 420), type = 6),
                                            revele = tagList(
                                              tags$p("Cette même rareté, ramenée à une grille de 100 maisons, rend l'écart d'offre immédiatement lisible : à peine 2 propriétés sur 100 profitent d'un espace vert."),
                                              tags$p("Cette contrainte physique d'offre limite mécaniquement le nombre de transactions premium liées à l'environnement possibles chaque année, quelle que soit l'évolution de la demande."),
                                              tags$p("Un promoteur ou un urbaniste ne peut donc pas répondre à cette demande simplement en construisant davantage : l'espace vert lui-même est la ressource limitante, pas le nombre de maisons."),
                                              tags$p("C'est cette rareté structurelle, plus que la qualité de la maison elle-même, qui explique une part de la prime observée sur ces biens."),
                                              tags$p("Une politique urbaine qui créerait de nouveaux espaces verts modifierait donc directement l'équilibre de cette partie du marché.")
                                            )
                                   ))
              )
      ),
      
      ## ---------- ACTE 4 ----------
      tabItem(tabName = "acte4",
              page_header(title = "Quels facteurs influencent vraiment le prix", meta = "Acte 4. Corrélations. Vue 3D. Réseau. Random Forest"),
              section_subtitle("La qualité globale, la surface habitable et les places de garage sont les trois piliers du prix."),
              tabsetPanel(id = "acte4_subtabs", type = "tabs",
                          
                          tabPanel("Corrélations", br(),
                                   tags$div(class = "mini-nav", tags$a(href="#viz-correlation","Corrélation"), tags$a(href="#viz-reseau","Réseau")),
                                   viz_card(id = "viz-correlation", titre = "Trois variables dominent la corrélation avec le prix",
                                            type = "Carte de chaleur", role = "Matrice de corrélation des variables numériques",
                                            output_ui = withSpinner(plotOutput("heatmap_correlation", height = 450), type = 6),
                                            revele = tagList(
                                              tags$p("Trois forces majeures guident le marché d'Ames : la qualité globale, la surface habitable et la taille du garage."),
                                              tags$p("Un piège technique apparaît toutefois : ces caractéristiques sont très fortement liées entre elles, les grandes maisons ayant presque toujours de grands garages et d'excellentes finitions."),
                                              tags$p("Une maison doit donc être analysée comme un équilibre entre trois piliers interconnectés, l'espace habitable, la logistique du stationnement et la modernité des matériaux."),
                                              tags$p("Pour estimer correctement un bien, on ne peut pas simplement additionner ces critères indépendamment, il faut comprendre comment ils s'articulent ensemble."),
                                              tags$p("C'est cette interdépendance qui explique pourquoi le cercle le plus foncé de la matrice n'est jamais un facteur isolé, mais toujours une combinaison de plusieurs variables corrélées.")
                                            )
                                   ),
                                   viz_card(id = "viz-reseau", titre = "Trois regroupements de variables apparaissent",
                                            type = "Diagramme en réseau", role = "Relations entre variables corrélées",
                                            output_ui = withSpinner(visNetworkOutput("reseau_variables", height = 450), type = 6),
                                            revele = textOutput("revele_reseau")
                                   )),
                          
                          tabPanel("Vue 3D", br(),
                                   viz_card(titre = "Surface et qualité se renforcent mutuellement",
                                            type = "Diagramme à bulles", role = "Surface habitable, qualité et prix combinés",
                                            output_ui = withSpinner(plotlyOutput("bubble_chart", height = 480), type = 6),
                                            revele = tagList(
                                              tags$p("Plus les maisons sont grandes, plus leur prix s'envole, mais uniquement si leur note de qualité augmente en même temps."),
                                              tags$p("C'est la démonstration d'un effet de synergie : la taille seule ne suffit pas à atteindre des sommets financiers."),
                                              tags$p("Pour maximiser le prix, les grands espaces doivent obligatoirement s'accompagner de finitions irréprochables ; les maisons les plus chères de la ville représentent cette alliance entre immensité et grand luxe."),
                                              tags$p("À l'inverse, une grande maison construite avec des matériaux bas de gamme voit sa valeur stagner, les acheteurs du segment supérieur refusant d'y habiter sans engager de lourds travaux."),
                                              tags$p("Surface et qualité fonctionnent donc comme deux leviers complémentaires, jamais substituables l'un à l'autre.")
                                            )
                                   ),
                                   viz_card(titre = "Trois marchés distincts apparaissent en 3D",
                                            type = "Nuage de points en 3 dimensions", role = "Surface habitable, qualité et prix par quartier",
                                            output_ui = withSpinner(plotlyOutput("scatter3d", height = 480), type = 6),
                                            revele = tagList(
                                              tags$p("La rotation du nuage de points révèle des regroupements distincts selon le quartier, plutôt qu'un continuum unique de surface, qualité et prix."),
                                              tags$p("Ames possède donc plusieurs marchés immobiliers parallèles, chacun avec sa propre combinaison de ces trois variables."),
                                              tags$p("Un même niveau de qualité ne produit pas le même prix selon le quartier où se trouve la maison, ce qui confirme que la localisation agit comme un multiplicateur propre à chaque zone."),
                                              tags$p("Cette lecture en trois dimensions illustre mieux qu'un graphique plat pourquoi deux maisons très similaires sur le papier peuvent se vendre à des prix très différents."),
                                              tags$p("Elle renforce l'idée que le prix résulte toujours d'une combinaison de facteurs, jamais d'une seule variable prise isolément.")
                                            )
                                   )),
                          
                          tabPanel("Profils premium", br(),
                                   viz_card(titre = "Qualité et surface sont deux conditions conjointes",
                                            type = "Coordonnées parallèles", role = "Profils des maisons premium",
                                            output_ui = withSpinner(plotOutput("parcoord_premium", height = 450), type = 6),
                                            revele = tagList(
                                              tags$p("Les maisons premium (plus de 300 000 $) révèlent une hiérarchie nette entre les critères : la qualité globale (OverallQual) est le verrou non négociable — aucune ne descend sous 7/10, et neuf sur dix affichent au moins 8/10."),
                                              tags$p("La surface, elle, se négocie : un tiers des maisons premium fait moins de 2 000 ft². À Stone Brook, une maison de 1 419 ft² s'est vendue 392 000 $, portée par sa seule qualité."),
                                              tags$p("Un immense garage ou un sous-sol aménagé ne suffisent jamais à compenser des finitions médiocres ; une qualité exceptionnelle, en revanche, compense une surface contenue."),
                                              tags$p("Cela se lit sur le graphique : toutes les lignes passent haut sur l'axe qualité, alors qu'elles s'étalent largement sur l'axe surface."),
                                              tags$p("Pour viser le segment premium, la montée en gamme prime donc sur l'agrandissement.")
                                            )
                                   ),
                                   viz_card(titre = "Un seul facteur domine le pouvoir prédictif",
                                            type = "Diagramme en barres", role = "Importance des variables, Random Forest",
                                            output_ui = withSpinner(plotOutput("importance_rf", height = 450), type = 6),
                                            revele = tagList(
                                              tags$p("L'algorithme prédictif, qui classe les caractéristiques selon leur capacité réelle à anticiper le prix de vente, place la qualité globale sur la première marche, loin devant la surface habitable et l'année de construction."),
                                              tags$p("Cela enseigne une leçon commerciale essentielle : le standing perçu d'une maison compte davantage aux yeux des acheteurs que la simple taille brute des pièces."),
                                              tags$p("Pour un investisseur qui souhaite rénover avant de revendre, le retour sur investissement sera maximal en modernisant et en montant en gamme l'existant, plutôt qu'en construisant une extension brute."),
                                              tags$p("La qualité globale capture à elle seule une part disproportionnée de ce qui explique le prix, bien au-delà de son poids apparent parmi les 80 variables du dataset."),
                                              tags$p("C'est le facteur à surveiller en priorité pour toute stratégie de valorisation d'un bien à Ames.")
                                            )
                                   ))
              )
      ),
      
      ## ---------- ACTE 5 ----------
      tabItem(tabName = "acte5",
              page_header(title = "Comment les prix évoluent-ils dans le temps", meta = "Acte 5. Crise de 2008. Ères de construction. Saisonnalité"),
              section_subtitle("167 000 dollars en 2007, 155 000 dollars en 2010, 7.2% de moins en 36 mois."),
              tabsetPanel(id = "acte5_subtabs", type = "tabs",
                          
                          tabPanel("Évolution 2006-2010", br(),
                                   viz_card(titre = "La chute post Lehman Brothers a fait baisser les prix",
                                            type = "Courbe temporelle", role = "Évolution du prix médian et du volume de ventes",
                                            output_ui = withSpinner(highchartOutput("line_evolution", height = 450), type = 6),
                                            revele = textOutput("revele_evolution")
                                   )),
                          
                          tabPanel("Ères et saisonnalité", br(),
                                   viz_card(titre = "Les maisons post 1991 valent nettement plus",
                                            type = "Diagramme en crêtes", role = "Distribution des prix par ère de construction",
                                            output_ui = withSpinner(plotOutput("ridgeline_era", height = 450), type = 6),
                                            revele = textOutput("revele_ridgeline")
                                   ),
                                   viz_card(titre = "Mai et juin concentrent le pic de ventes",
                                            type = "Carte de chaleur", role = "Saisonnalité des ventes",
                                            output_ui = withSpinner(plotOutput("heatmap_saison", height = 450), type = 6),
                                            revele = tagList(
                                              tags$p("L'analyse des ventes mois par mois révèle un rythme cyclique d'une régularité spectaculaire : chaque année, l'activité explose sur mai, juin et juillet, tandis que l'automne et l'hiver restent calmes."),
                                              tags$p("Ce rythme n'est pas lié à la météo mais à l'organisation sociologique d'une ville universitaire, où toute la vie locale est calée sur le calendrier académique."),
                                              tags$p("C'est durant la pause estivale que se concentrent les déménagements d'étudiants, les mutations d'enseignants et les changements d'école des familles avant la rentrée de fin août."),
                                              tags$p("Un vendeur qui met sa maison sur le marché en juin profite d'une concurrence maximale entre acheteurs, ce qui augmente ses chances d'obtenir une offre au prix fort et rapidement."),
                                              tags$p("Mettre en vente en décembre, à l'inverse, expose à des délais longs et à des négociations agressives à la baisse.")
                                            )
                                   ))
              )
      ),
      
      ## ---------- PRÉDICTION ----------
      tabItem(tabName = "ml",
              page_header(title = "Prédiction du prix d'une maison", meta = "Régression linéaire. Random Forest. k-NN. Soumission Kaggle"),
              section_subtitle("Cinq modèles comparés pour valider les conclusions de l'enquête."),
              fluidRow(
                column(3, kpi_card("RMSE validation", "28 324 $", hint="Random Forest", accent=COULEURS$bleu)),
                column(3, kpi_card("Variance expliquée", "87.8 %", hint="Sur données jamais vues", accent=COULEURS$vert)),
                column(3, kpi_card("Score Kaggle", "0.12714", hint="Validation externe, XGBoost", accent=COULEURS$orange)),
                column(3, kpi_card("Variables clés", "6", hint="Qualité, surface, quartier", accent=COULEURS$rouge))
              ),
              fluidRow(
                column(6, card(title = "Comparaison des modèles", tableOutput("comparaison_modeles"))),
                column(6, card(title = "Simulateur de prix",
                               numericInput("sim_grlivarea","Surface habitable (pi²)",1500,300,6000),
                               selectInput("sim_overallqual","Qualité globale (1-10)", choices=1:10, selected=7),
                               selectInput("sim_neighborhood","Quartier", choices=choix_quartiers),
                               numericInput("sim_garagecars","Places de garage",2,0,4),
                               numericInput("sim_totalbsmtsf","Surface sous-sol (pi²)",800,0,3000),
                               numericInput("sim_yearbuilt","Année de construction",1980,1870,2010),
                               actionButton("sim_predire","Prédire le prix", class="btn-primary"),
                               br(), br(),
                               tags$div(style = "text-align:center; padding:16px; background:#FFF7ED; border-radius:10px;",
                                        tags$div(style = "font-size:11px; text-transform:uppercase; color:#7A6F60; letter-spacing:0.5px;", "Prix estimé"),
                                        tags$div(style = "font-size:32px; font-weight:800; color:#E8721C; margin-top:4px;", textOutput("sim_resultat"))
                               )
                ))
              ),
              fluidRow(column(12, card(title = "5 biens comparables", uiOutput("knn_cards"))))
      ),
      
      ## ---------- RECOMMANDATIONS ----------
      tabItem(tabName = "reco",
              page_header(title = "5 recommandations stratégiques", meta = "Acheteur. Vendeur. Investisseur. Promoteur. Urbaniste"),
              banner_img("images/reco_banniere.png", hauteur = "240px"),
              section_subtitle("Cinq recommandations différenciées par profil d'acteur."),
              reco_card(1,"Acheteur individuel","Cibler les quartiers sous-évalués", chiffre_cle="-30%", accent=COULEURS$bleu, image="images/acheteur.png",
                        texte="Timberland (228 950 dollars) et Somerset (226 000 dollars) offrent des profils quasi identiques à Northridge Heights (315 000 dollars) pour 30% de moins.",
                        graphiques="Appuyé par le classement des quartiers et le profil radar des segments de marché"),
              reco_card(2,"Vendeur","Rénover cuisine et extérieur, vendre en mai ou juin", chiffre_cle="+20k $", accent=COULEURS$vert, image="images/vendeur.png",
                        texte="La qualité de la cuisine et la qualité extérieure figurent dans le top 5 des facteurs les plus prédictifs. Mai et juin génèrent 5 à 8% de ventes en plus.",
                        graphiques="Appuyé par l'importance des variables du Random Forest et la saisonnalité des ventes"),
              reco_card(3,"Investisseur","Acheter en condition de vente anormale", chiffre_cle="-18%", accent=COULEURS$orange, image="images/investisseur.png",
                        texte="Les ventes en condition anormale affichent 18% de moins que les ventes classiques.",
                        graphiques="Appuyé par la matrice de corrélation et la courbe d'évolution temporelle"),
              reco_card(4,"Promoteur immobilier","Construire en zone résidentielle avec espaces verts", chiffre_cle="+41%", accent=COULEURS$rouge, image="images/promoteur.jpg",
                        texte="Une maison adjacente à un parc affiche 235 000 dollars, une prime de 41% face à une route artérielle.",
                        graphiques="Appuyé par la fréquence des conditions environnementales et les barres de prix"),
              reco_card(5,"Urbaniste et collectivité","Les espaces verts valorisent le patrimoine", chiffre_cle="5-10M $", accent=COULEURS$jaune, image="images/urbaniste.png",
                        texte="Un parc dans un quartier de 100 maisons peut générer 5 à 10 millions de dollars de valorisation du patrimoine.",
                        graphiques="Appuyé par le nuage de points par condition et le graphique en gaufre")
      )
    )
  )
)

## ============================================================
## SERVER
## ============================================================
server <- function(input, output, session) {
  
  observeEvent(input$nav_acte0, updateTabItems(session, "main_tabs", "acte0"))
  observeEvent(input$nav_acte1, updateTabItems(session, "main_tabs", "acte1"))
  observeEvent(input$nav_acte2, updateTabItems(session, "main_tabs", "acte2"))
  observeEvent(input$nav_acte3, updateTabItems(session, "main_tabs", "acte3"))
  observeEvent(input$nav_acte4, updateTabItems(session, "main_tabs", "acte4"))
  observeEvent(input$nav_acte5, updateTabItems(session, "main_tabs", "acte5"))
  observeEvent(input$nav_reco,  updateTabItems(session, "main_tabs", "reco"))
  
  donnees <- reactive({ if (input$quartiers_filtre == "Tous") train else train %>% filter(Neighborhood == input$quartiers_filtre) })
  
  output$hero_carousel <- renderUI({
    segs <- segments_df %>% group_by(label_segment) %>%
      summarise(prix_median = median(prix), exemple = first(nom_complet)) %>%
      arrange(match(label_segment, c("Premium","Moyen","Abordable")))
    tags$div(class = "hero-carousel",
             lapply(seq_len(nrow(segs)), function(i) {
               seg <- segs$label_segment[i]
               img_src <- get_safe(images_segment, seg, "")
               tags$div(class = "hero-slide", style = if (nzchar(img_src)) paste0("background-image:url('", img_src, "');") else "",
                        tags$div(class = "hero-caption",
                                 tags$div(style = paste0("font-size:11px; text-transform:uppercase; letter-spacing:0.5px; color:", get_safe(couleurs_par_label, seg, COULEURS$orange), "; font-weight:700;"), paste("Segment", seg)),
                                 tags$div(style = "font-size:20px; font-weight:700; margin-top:2px;", paste0(scales::dollar(segs$prix_median[i]), " médian")),
                                 tags$div(style = "font-size:12px; color:#D8D0BE; margin-top:2px;", paste("Exemple :", segs$exemple[i]))
                        )
               )
             })
    )
  })
  
  output$apercu_segments <- renderHighchart({
    apercu <- segments_df %>% group_by(label_segment) %>% summarise(prix_median = median(prix)) %>%
      arrange(match(label_segment, c("Premium","Moyen","Abordable")))
    couleurs_ordre <- unname(sapply(apercu$label_segment, function(s) get_safe(couleurs_par_label, s, COULEURS$orange)))
    hchart(apercu, "column", hcaes(x = label_segment, y = prix_median, color = label_segment)) %>%
      hc_colors(couleurs_ordre) %>% hc_xAxis(categories = apercu$label_segment, title = list(text="Segment de marché")) %>%
      hc_yAxis(title=list(text="Prix médian ($)")) %>% hc_legend(enabled=FALSE)
  })
  
  output$apercu_donut <- renderPlotly({
    bd <- train %>% count(BldgType_label) %>% arrange(desc(n))
    plot_ly(bd, labels = ~BldgType_label, values = ~n, type = "pie", hole = 0.5, textinfo = "percent",
            marker = list(colors = unname(palette_bldgtype[bd$BldgType_label]))) %>% layout(showlegend = TRUE)
  })
  
  output$mini_classement <- renderUI({
    top5 <- train %>% mutate(nom_complet = recode(Neighborhood, !!!noms_quartiers)) %>%
      group_by(nom_complet) %>% summarise(prix = median(SalePrice)) %>% arrange(desc(prix)) %>% head(5)
    tagList(lapply(seq_len(nrow(top5)), function(i) {
      tags$div(class = "ranking-row",
               tags$span(tags$span(class="ranking-rank", i), top5$nom_complet[i]),
               tags$span(style = "font-weight:700; color:var(--orange);", scales::dollar(top5$prix[i]))
      )
    }))
  })
  
  output$apercu_barres_mois <- renderHighchart({
    md <- train %>% mutate(MoSold = as.integer(as.character(MoSold))) %>% count(MoSold) %>% arrange(MoSold)
    hchart(md, "column", hcaes(x = MoSold, y = n)) %>% hc_colors(COULEURS$orange) %>%
      hc_xAxis(title = list(text = "Mois de vente")) %>% hc_yAxis(title = list(text = "Nombre de ventes"))
  })
  
  output$table_nettoyage <- renderTable({ audit_nettoyage })
  
  output$table_donnees_completes <- renderDT({
    apercu <- train %>%
      select(all_of(colonnes_pertinentes)) %>%
      rename(!!!setNames(colonnes_pertinentes,
                         recode(colonnes_pertinentes, !!!labels_variables, Id = "Identifiant", Neighborhood = "Quartier")))
    datatable(apercu, options = list(pageLength = 10, scrollX = TRUE, dom = "ltip"), rownames = FALSE)
  })
  
  output$telecharger_donnees <- downloadHandler(
    filename = function() "train_clean.csv",
    content = function(file) {
      write_csv(train %>% select(-Condition1_label, -BldgType_label, -ScoreEnv_label), file)
    }
  )
  
  output$histo_prix <- renderPlotly({
    p <- ggplot(donnees(), aes(x = SalePrice)) + geom_histogram(bins = 50, fill = COULEURS$orange, color = "white") +
      geom_vline(xintercept = median_global, color = COULEURS$bleu, linetype = "dashed") +
      scale_x_continuous(labels = scales::dollar) + labs(x = "Prix de vente", y = "Nombre de maisons") + theme_ames()
    ggplotly(p)
  })
  
  output$density_prix <- renderPlot({
    donnees() %>% filter(BldgType %in% input$types) %>%
      ggplot(aes(x = SalePrice, fill = BldgType_label)) + geom_density(alpha = 0.6) +
      scale_fill_manual(values = palette_bldgtype) +
      scale_x_continuous(labels = label_dollar(scale = 1/1000, suffix = "k")) +
      labs(x = "Prix de vente", y = NULL, fill = "Type de bâtiment") +
      theme_ames() + theme(axis.text.y = element_blank())
  })
  
  output$donut_types <- renderPlotly({
    bldg_df <- donnees() %>% filter(BldgType %in% input$types) %>% count(BldgType_label) %>% arrange(desc(n))
    plot_ly(bldg_df, labels = ~BldgType_label, values = ~n, type = "pie", hole = 0.55, textinfo = "label+percent",
            marker = list(colors = unname(palette_bldgtype[bldg_df$BldgType_label]))) %>% layout(showlegend = FALSE)
  })
  
  output$lollipop_quartiers <- renderPlot({
    train %>% mutate(nom_complet = recode(Neighborhood, !!!noms_quartiers)) %>%
      group_by(nom_complet) %>% summarise(prix_median = median(SalePrice)) %>%
      ggplot(aes(x = prix_median, y = reorder(nom_complet, prix_median))) +
      geom_segment(aes(xend = 0, yend = nom_complet), color = "#d8d4cc") +
      geom_point(aes(color = prix_median), size = 3) +
      scale_color_gradient(low = "#FBDFC0", high = COULEURS$orange) +
      geom_vline(xintercept = median_global, color = COULEURS$bleu, linetype = "dashed") +
      scale_x_continuous(labels = label_dollar(scale = 1/1000, suffix = "k")) +
      labs(x = "Prix médian", y = "Quartier") + theme_ames() + theme(legend.position = "none")
  })
  
  output$violin_qualite <- renderPlot({
    ggplot(donnees(), aes(x = OverallQual, y = SalePrice, fill = OverallQual)) +
      geom_violin(trim = FALSE, alpha = 0.8) + geom_boxplot(width = 0.1, fill = "white", alpha = 0.7) +
      scale_fill_manual(values = setNames(colorRampPalette(c("#FBDFC0", COULEURS$orange))(10), levels(train$OverallQual)), drop = TRUE) +
      scale_x_discrete(drop = TRUE) + scale_y_continuous(labels = label_dollar(scale = 1/1000, suffix = "k")) +
      labs(x = "Qualité globale", y = "Prix de vente") + theme_ames() + theme(legend.position = "none")
  })
  
  output$boxplot_quartiers <- renderHighchart({
    df_recode <- train %>% mutate(nom_complet = recode(Neighborhood, !!!noms_quartiers))
    box_data <- data_to_boxplot(df_recode, SalePrice, nom_complet, add_outliers = TRUE, name = "Prix")
    highchart() %>% hc_xAxis(type = "category", title = list(text = "Quartier")) %>%
      hc_yAxis(title = list(text = "Prix de vente ($)")) %>% hc_add_series_list(box_data) %>%
      hc_legend(enabled = FALSE) %>% hc_colors(COULEURS$orange)
  })
  
  output$treemap_quartiers <- renderHighchart({
    neigh_df <- train %>% mutate(nom_complet = recode(Neighborhood, !!!noms_quartiers)) %>%
      group_by(nom_complet) %>% summarise(n = n(), med = median(SalePrice))
    hchart(neigh_df, "treemap", hcaes(x = nom_complet, value = n, color = med)) %>%
      hc_colorAxis(stops = color_stops(10, c(COULEURS$rouge, "#FFFFFF", COULEURS$vert)))
  })
  
  output$stacked_composition <- renderPlotly({
    quartiers_aff <- union(train %>% count(Neighborhood, sort = TRUE) %>% head(14) %>% pull(Neighborhood), "NPkVill")
    comp_df <- train %>% filter(Neighborhood %in% quartiers_aff) %>%
      mutate(nom_complet = recode(Neighborhood, !!!noms_quartiers)) %>%
      count(nom_complet, BldgType_label) %>% group_by(nom_complet) %>% mutate(pct = n / sum(n)) %>% ungroup()
    part_indiv <- comp_df %>% filter(BldgType_label == "Maison individuelle") %>% arrange(pct)
    ordre <- c(setdiff(unique(comp_df$nom_complet), part_indiv$nom_complet), part_indiv$nom_complet)
    comp_df <- comp_df %>% mutate(nom_complet = factor(nom_complet, levels = ordre))
    plot_ly(comp_df, x = ~nom_complet, y = ~pct, color = ~BldgType_label, type = "bar", colors = palette_bldgtype) %>%
      layout(barmode = "stack", yaxis = list(title = "Proportion", tickformat = ".0%"),
             xaxis = list(title = "Quartier", tickangle = -35),
             legend = list(orientation = "h", y = -0.3, x = 0.1), margin = list(b = 120))
  })
  
  output$radar_segments <- renderPlot({
    med_q <- train %>% group_by(Neighborhood) %>%
      summarise(prix=median(SalePrice), surface=median(GrLivArea), qualite=median(as.numeric(OverallQual)),
                garage=median(GarageCars), recence=median(YearBuilt))
    rv <- representants_segments %>%
      transmute(prix    = (prix    - min(med_q$prix))    / (max(med_q$prix)    - min(med_q$prix)),
                surface = (surface - min(med_q$surface)) / (max(med_q$surface) - min(med_q$surface)),
                qualite = (qualite - min(med_q$qualite)) / (max(med_q$qualite) - min(med_q$qualite)),
                garage  = (garage  - min(med_q$garage))  / (max(med_q$garage)  - min(med_q$garage)),
                recence = (recence - min(med_q$recence)) / (max(med_q$recence) - min(med_q$recence))) %>%
      as.data.frame()
    rownames(rv) <- representants_segments$label_segment
    rv <- rv[c("Premium","Moyen","Abordable"), , drop = FALSE]
    rd <- rbind(rep(1,5), rep(0,5), rv)
    fmsb::radarchart(rd, pcol = palette_radar_fixe, pfcol = paste0(palette_radar_fixe, "44"), plwd = 2,
                     cglcol = "grey", axislabcol = "grey", vlabels = c("Prix","Surface","Qualité","Garage","Récence"))
    legend("topright", legend = rownames(rv), col = palette_radar_fixe, lty = 1, lwd = 2, bty = "n", cex = 0.9)
  })
  
  output$carte_quartiers <- renderLeaflet({
    pal <- colorNumeric(palette = colorRampPalette(c("#FBDFC0", COULEURS$orange, COULEURS$rouge))(10), domain = carte_data$prix_median)
    leaflet(carte_data) %>% addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~sqrt(n) * 2, color = ~pal(prix_median), fillOpacity = 0.8, stroke = TRUE, weight = 1,
                       label = ~nom_complet, labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE,
                                                                         style = list("font-weight"="600","font-size"="11px","color"="#18181E")),
                       popup = ~paste0("<b>", nom_complet, "</b><br>Prix médian : ", scales::dollar(prix_median), "<br>", n, " ventes")) %>%
      addLegend("bottomright", pal = pal, values = ~prix_median, title = "Prix médian ($)", labFormat = labelFormat(prefix = "$"))
  })
  
  output$radar_comparaison <- renderPlot({
    med_q <- train %>% group_by(Neighborhood) %>%
      summarise(prix=median(SalePrice), surface=median(GrLivArea), qualite=median(as.numeric(OverallQual)),
                garage=median(GarageCars), recence=median(YearBuilt))
    bornes <- med_q %>% summarise(prix_min=min(prix), prix_max=max(prix), surf_min=min(surface), surf_max=max(surface),
                                  qual_min=min(qualite), qual_max=max(qualite), gar_min=min(garage), gar_max=max(garage),
                                  rec_min=min(recence), rec_max=max(recence))
    deux <- train %>% filter(Neighborhood %in% c(input$quartier_a, input$quartier_b)) %>% group_by(Neighborhood) %>%
      summarise(prix=median(SalePrice), surface=median(GrLivArea), qualite=median(as.numeric(OverallQual)), garage=median(GarageCars), recence=median(YearBuilt)) %>%
      arrange(match(Neighborhood, c(input$quartier_a, input$quartier_b)))
    rv <- deux %>% mutate(
      prix=(prix-bornes$prix_min)/(bornes$prix_max-bornes$prix_min), surface=(surface-bornes$surf_min)/(bornes$surf_max-bornes$surf_min),
      qualite=(qualite-bornes$qual_min)/(bornes$qual_max-bornes$qual_min), garage=(garage-bornes$gar_min)/(bornes$gar_max-bornes$gar_min),
      recence=(recence-bornes$rec_min)/(bornes$rec_max-bornes$rec_min)) %>% select(-Neighborhood)
    rd <- rbind(rep(1,5), rep(0,5), rv)
    couleurs2 <- c(COULEURS$orange, COULEURS$bleu)
    fmsb::radarchart(rd, pcol = couleurs2, plwd = 2, cglcol = "grey", axislabcol = "grey", vlabels = c("Prix","Surface","Qualité","Garage","Récence"))
    legend("topright", legend = noms_quartiers[c(input$quartier_a, input$quartier_b)], col = couleurs2, lty = 1, lwd = 2, bty = "n")
  })
  
  output$revele_comparaison <- renderText({
    req(input$quartier_a, input$quartier_b)
    da <- train %>% filter(Neighborhood == input$quartier_a) %>% summarise(p=median(SalePrice)) %>% pull(p)
    db <- train %>% filter(Neighborhood == input$quartier_b) %>% summarise(p=median(SalePrice)) %>% pull(p)
    sa <- train %>% filter(Neighborhood == input$quartier_a) %>% summarise(s=median(GrLivArea)) %>% pull(s)
    sb <- train %>% filter(Neighborhood == input$quartier_b) %>% summarise(s=median(GrLivArea)) %>% pull(s)
    na_ <- noms_quartiers[[input$quartier_a]]; nb_ <- noms_quartiers[[input$quartier_b]]
    fr <- function(x) paste0(format(round(x), big.mark = " "), " $")
    plus_cher <- if (da > db) na_ else nb_
    plus_grand <- if (sa > sb) na_ else nb_
    intro <- paste0(na_, " affiche un prix médian de ", fr(da), " contre ", fr(db), " pour ", nb_, ". ")
    if (plus_cher == plus_grand) {
      paste0(intro, plus_cher, " domine à la fois par le prix et par la surface : la taille explique une partie de l'écart, mais le radar montre que la qualité et la récence font le reste.")
    } else {
      paste0(intro, plus_cher, " est le plus cher sans être le plus grand : son prix s'explique surtout par la qualité, le garage ou la localisation, visibles sur les autres axes.")
    }
  })
  
  output$dendrogramme_quartiers <- renderPlot({
    hc_affichage <- hc
    hc_affichage$labels <- recode(hc_affichage$labels, !!!noms_quartiers)
    plot(hc_affichage, main = "", xlab = "", sub = "", ylab = "Distance", col = "#18181E", cex = 0.75)
    rect.hclust(hc, k = 3, border = couleurs_cluster_dessin)
  })
  
  output$table_segments <- renderUI({
    data <- segments_df %>% group_by(label_segment) %>%
      summarise(n_quartiers = n(), prix_median = median(prix), quartiers = paste(nom_complet, collapse = ", ")) %>%
      arrange(match(label_segment, c("Premium","Moyen","Abordable")))
    tagList(lapply(seq_len(nrow(data)), function(i) {
      seg <- data$label_segment[i]; coul <- get_safe(couleurs_par_label, seg, COULEURS$orange)
      tags$div(style = paste0("background:white; border-left:5px solid ", coul, "; border-radius:8px; padding:14px 16px; margin-bottom:10px;"),
               tags$div(style = "display:flex; justify-content:space-between; align-items:center;",
                        tags$strong(style = "font-size:15px; color:#18181E;", seg),
                        tags$span(style = paste0("color:", coul, "; font-weight:700;"), scales::dollar(data$prix_median[i]))),
               tags$div(style = "color:#7A6F60; font-size:12px; margin-top:4px;", paste(data$n_quartiers[i], "quartiers")),
               tags$div(style = "color:#3A342A; font-size:12.5px; margin-top:6px;", data$quartiers[i])
      )
    }))
  })
  
  output$barres_condition <- renderHighchart({
    hchart(cond_df, "bar", hcaes(x = Condition1_label, y = med, color = impact_col)) %>%
      hc_yAxis(plotLines = list(list(value = median_global, color = COULEURS$bleu, dashStyle = "dash", width = 2)), title = list(text = "Prix médian ($)")) %>%
      hc_xAxis(title = list(text = "Condition environnementale"))
  })
  
  output$scatter_condition <- renderPlotly({
    plot_ly(donnees(), x = ~GrLivArea, y = ~SalePrice, color = ~ScoreEnv_label,
            colors = c("Positif (parc)"=COULEURS$vert, "Normal"="#9C8F78", "Route secondaire"=COULEURS$orange, "Route/rail"=COULEURS$rouge),
            type = "scatter", mode = "markers", marker = list(size = 5, opacity = 0.6)) %>%
      layout(xaxis = list(title = "Surface habitable (pi²)"), yaxis = list(title = "Prix de vente ($)"))
  })
  
  output$nightingale_condition <- renderPlot({
    cf <- train %>% count(Condition1_label) %>% mutate(n_sqrt = sqrt(n))
    ggplot(cf, aes(x = Condition1_label, y = n_sqrt, fill = Condition1_label)) +
      geom_col(width = 0.85, alpha = 0.9) + coord_polar() +
      scale_fill_manual(values = palette_condition9) +
      geom_text(aes(label = n, y = n_sqrt + 1.5), size = 3, color = "#18181E") +
      labs(x = NULL, y = NULL, fill = "Condition environnementale") + theme_ames() +
      theme(legend.position = "bottom", axis.text.x = element_text(size = 7), axis.text.y = element_blank())
  })
  
  output$waffle_condition <- renderPlot({
    wd <- train %>% mutate(groupe = case_when(Condition1=="Norm"~"Normale", Condition1 %in% c("PosA","PosN")~"Parc", TRUE~"Route/rail")) %>%
      count(groupe) %>% mutate(pct = round(100*n/sum(n)))
    grille <- expand.grid(x = 1:10, y = 1:10) %>% mutate(id = row_number())
    seuils <- cumsum(wd$pct); grille$groupe <- cut(grille$id, breaks = c(0, seuils), labels = wd$groupe)
    ggplot(grille, aes(x, y, fill = groupe)) + geom_tile(color = "white", size = 1.5) +
      scale_fill_manual(values = c("Normale"=COULEURS$bleu, "Parc"=COULEURS$vert, "Route/rail"=COULEURS$rouge)) +
      coord_equal() + theme_void() + theme(legend.position = "bottom")
  })
  
  output$heatmap_correlation <- renderPlot({
    cor_m_fr <- cor_m
    colnames(cor_m_fr) <- recode(colnames(cor_m_fr), !!!labels_variables)
    rownames(cor_m_fr) <- recode(rownames(cor_m_fr), !!!labels_variables)
    corrplot(cor_m_fr, method = "circle", type = "upper", col = colorRampPalette(c(COULEURS$rouge,"#FFFFFF",COULEURS$vert))(200), tl.cex = 0.65, tl.col = "#18181E")
  })
  
  output$reseau_variables <- renderVisNetwork({
    cl <- cor_m %>% as.data.frame() %>% rownames_to_column("var1") %>% pivot_longer(-var1, names_to="var2", values_to="r") %>%
      filter(var1 != var2, abs(r) > 0.5) %>% rowwise() %>% mutate(paire = paste(sort(c(var1,var2)), collapse="_")) %>% ungroup() %>% distinct(paire, .keep_all=TRUE)
    noeuds <- unique(c(cl$var1, cl$var2))
    groupes <- cutree(hclust(as.dist(1 - abs(cor_m[noeuds, noeuds])), method="average"), k=3)
    nodes_df <- data.frame(id=noeuds, label=recode(noeuds, !!!labels_variables), group=paste0("Cluster ", groupes[noeuds]), value=abs(cor_m["SalePrice", noeuds]))
    edges_df <- data.frame(from=cl$var1, to=cl$var2, width=abs(cl$r)*5)
    visNetwork(nodes_df, edges_df) %>% visGroups(groupname="Cluster 1", color=COULEURS$orange) %>%
      visGroups(groupname="Cluster 2", color=COULEURS$bleu) %>% visGroups(groupname="Cluster 3", color=COULEURS$vert) %>%
      visOptions(highlightNearest = TRUE, selectedBy = "group") %>% visLayout(randomSeed = 42) %>% visLegend()
  })
  
  output$revele_reseau <- renderText({
    cl <- cor_m %>% as.data.frame() %>% rownames_to_column("var1") %>% pivot_longer(-var1, names_to="var2", values_to="r") %>%
      filter(var1 != var2, abs(r) > 0.5) %>% distinct(var1, var2, .keep_all=TRUE)
    noeuds <- unique(c(cl$var1, cl$var2))
    groupes <- cutree(hclust(as.dist(1 - abs(cor_m[noeuds, noeuds])), method="average"), k=3)
    contenu <- sapply(1:3, function(g) paste0("Cluster ", g, " (", paste(recode(names(groupes[groupes==g]), !!!labels_variables), collapse=", "), ")"))
    paste0("Trois regroupements de variables apparaissent naturellement : ", paste(contenu, collapse=". "),
           ". Deux variables du même cluster racontent souvent la même information sous une forme différente, comme la surface du sous-sol et la surface totale, liées mécaniquement. Un modèle prédictif n'a donc pas besoin de conserver les deux variables d'un même groupe pour bien fonctionner, l'une suffit à capturer l'essentiel de l'information. Cette redondance explique aussi pourquoi la matrice de corrélation montre des blocs entiers de cercles foncés plutôt que des liens isolés. Comprendre ces regroupements aide à simplifier un modèle sans perdre en pouvoir prédictif.")
  })
  
  output$bubble_chart <- renderPlotly({
    bd <- bubble_data %>% mutate(taille = scales::rescale(as.numeric(OverallQual), to = c(10, 45)))
    plot_ly(bd, x = ~GrLivArea, y = ~SalePrice, size = ~taille, color = ~nom_complet, colors = palette_quartiers,
            type = "scatter", mode = "markers", marker = list(sizemode = "diameter", opacity = 0.65, line = list(width = 0.5, color = "white"))) %>%
      layout(xaxis = list(title = "Surface habitable (pi²)"), yaxis = list(title = "Prix de vente ($)"))
  })
  
  output$scatter3d <- renderPlotly({
    plot_ly(bubble_data, x = ~GrLivArea, y = ~as.numeric(OverallQual), z = ~SalePrice, color = ~nom_complet, colors = palette_quartiers,
            type = "scatter3d", mode = "markers", marker = list(size = 3, opacity = 0.7)) %>%
      layout(scene = list(xaxis = list(title = "Surface habitable (pi²)"), yaxis = list(title = "Qualité globale"), zaxis = list(title = "Prix de vente ($)")))
  })
  
  output$parcoord_premium <- renderPlot({
    pc <- train %>% mutate(premium = ifelse(SalePrice > 300000, "Premium", "Ordinaire")) %>%
      select(GrLivArea, OverallQual, GarageCars, TotalBsmtSF, SalePrice, premium) %>% mutate(OverallQual = as.numeric(OverallQual))
    ggparcoord(pc, columns = 1:5, groupColumn = "premium", scale = "uniminmax", alphaLines = 0.4) +
      scale_color_manual(values = c("Ordinaire"="#D1C7B0", "Premium"=COULEURS$orange)) +
      scale_x_discrete(labels = c("Surface habitable","Qualité globale","Places de garage","Surface du sous-sol","Prix de vente")) +
      theme_ames() + theme(legend.position = "bottom")
  })
  
  output$importance_rf <- renderPlot({
    ggplot(imp_df, aes(x = IncNodePurity, y = reorder(Variable, IncNodePurity), fill = IncNodePurity)) +
      geom_col() + scale_fill_gradient(low = "#FBDFC0", high = COULEURS$orange) +
      labs(x = "Importance prédictive", y = NULL) + theme_ames() + theme(legend.position = "none")
  })
  
  output$line_evolution <- renderHighchart({
    td <- train %>% mutate(YrSold = as.integer(as.character(YrSold))) %>% group_by(YrSold) %>% summarise(med_price = median(SalePrice), n = n())
    highchart() %>%
      hc_add_series(td, "column", hcaes(x = YrSold, y = n), yAxis = 1, name = "Volume", color = COULEURS$bleu, zIndex = 1) %>%
      hc_add_series(td, "line", hcaes(x = YrSold, y = med_price), name = "Prix médian", color = COULEURS$orange, lineWidth = 3, zIndex = 2,
                    marker = list(enabled = TRUE, radius = 5)) %>%
      hc_yAxis_multiples(list(title = list(text = "Prix médian")), list(title = list(text = "Volume"), opposite = TRUE))
  })
  
  output$revele_evolution <- renderText({
    td <- train %>% mutate(YrSold = as.integer(as.character(YrSold))) %>% group_by(YrSold) %>% summarise(m = median(SalePrice), n = n())
    pic <- td %>% filter(m == max(m))
    paste0("Le prix médian atteint son sommet en ", pic$YrSold[1], ", à ", scales::dollar(pic$m[1]),
           ", avant d'entamer une chute nette de 7.2% jusqu'en 2010, en pleine crise des subprimes de 2008. ",
           "Le volume de ventes reste stable entre 2006 et 2009, autour de 300 à 340 transactions par an. ",
           "La chute apparente du volume en 2010 (175 ventes) n'est pas un signe d'effondrement du marché : le fichier de données s'arrête en juillet 2010, soit seulement 7 mois enregistrés contre 12 pour les autres années. ",
           "Ramené à un rythme annuel comparable, le volume 2010 reste proche des années précédentes, ce qui confirme que le marché d'Ames a encaissé la crise par un ajustement des prix, pas par un arrêt des transactions. ",
           "Cette résistance du volume réel s'explique par la présence de l'Iowa State University, qui maintient un flux constant d'étudiants et de personnel ayant besoin de se loger, indépendamment du climat économique général.")
  })
  
  output$ridgeline_era <- renderPlot({
    ggplot(train, aes(x = SalePrice, y = Era, fill = Era)) + geom_density_ridges(alpha = 0.8, scale = 1.2) +
      scale_fill_manual(values = palette_era) + geom_vline(xintercept = median_global, color = COULEURS$bleu, linetype = "dashed") +
      labs(x = "Prix de vente", y = "Ère de construction") + theme_ridges() +
      theme(legend.position = "none", plot.background = element_rect(fill = "white", color = NA))
  })
  
  output$revele_ridgeline <- renderText({
    par_era <- train %>% group_by(Era) %>% summarise(med = median(SalePrice))
    plus_recent <- par_era %>% filter(Era == "1991-2010") %>% pull(med)
    plus_ancien <- par_era %>% filter(Era == "Pré-1920") %>% pull(med)
    ecart_pct <- round((plus_recent / plus_ancien - 1) * 100)
    paste0("Les maisons construites après 1991 affichent un prix médian de ", scales::dollar(plus_recent),
           ", contre ", scales::dollar(plus_ancien), " pour celles d'avant 1920, soit ", ecart_pct, "% de plus. ",
           "Cette différence dépasse largement la simple usure naturelle des vieux bâtiments : elle matérialise une rupture dans la façon même de concevoir l'architecture et l'habitat. ",
           "Les maisons modernes intègrent des standards qui n'existaient pas autrefois, des plans ouverts entre cuisine et salon, une isolation thermique performante, des suites parentales privatives et des garages doubles ou triples. ",
           "Ce sont ces standards de confort récents et très recherchés qui expliquent la forte prime de prix sur cette vague de construction, bien plus que l'âge du bâtiment en lui-même. ",
           "La courbe la plus récente est aussi la plus décalée vers la droite du graphique, sans chevauchement important avec la courbe pré-1920 : une vraie séparation entre deux générations de logements.")
  })
  
  output$heatmap_saison <- renderPlot({
    train %>% mutate(MoSold = as.integer(as.character(MoSold)), YrSold = as.integer(as.character(YrSold))) %>%
      count(MoSold, YrSold) %>% ggplot(aes(x = MoSold, y = YrSold, fill = n)) + geom_tile(color = "white") +
      geom_text(aes(label = n), color = "#18181E", size = 3) + scale_fill_gradient(low = "#FBDFC0", high = COULEURS$orange) +
      scale_x_continuous(breaks = 1:12) + labs(x = "Mois de vente", y = "Année de vente") + theme_ames()
  })
  
  output$comparaison_modeles <- renderTable({ comparaison_modeles_df })
  
  prediction <- eventReactive(input$sim_predire, {
    nouvelle <- train_rf[1, ]
    nouvelle$GrLivArea <- input$sim_grlivarea
    nouvelle$OverallQual <- factor(input$sim_overallqual, levels = 1:10, ordered = TRUE)
    nouvelle$Neighborhood <- factor(input$sim_neighborhood, levels = levels(train_rf$Neighborhood))
    nouvelle$GarageCars <- input$sim_garagecars
    nouvelle$TotalBsmtSF <- input$sim_totalbsmtsf
    nouvelle$YearBuilt <- input$sim_yearbuilt
    predict(rf_model, newdata = nouvelle)
  })
  
  output$sim_resultat <- renderText({ req(prediction()); scales::dollar(round(prediction())) })
  
  output$knn_cards <- renderUI({
    req(input$sim_predire)
    knn_data <- train %>% mutate(nom_complet = recode(Neighborhood, !!!noms_quartiers)) %>%
      select(GrLivArea, SalePrice, OverallQual, GarageCars, TotalBsmtSF, YearBuilt, nom_complet)
    
    vars_num <- knn_data %>% select(GrLivArea, SalePrice, OverallQual) %>%
      mutate(OverallQual = as.numeric(OverallQual)) %>% scale()
    cible <- c(input$sim_grlivarea, prediction(), as.numeric(input$sim_overallqual))
    centre <- attr(vars_num, "scaled:center"); echelle <- attr(vars_num, "scaled:scale")
    cible_scaled <- (cible - centre) / echelle
    distances <- sqrt(rowSums((as.matrix(vars_num) - matrix(cible_scaled, nrow(vars_num), 3, byrow = TRUE))^2))
    top5 <- knn_data %>% mutate(distance = distances) %>% arrange(distance) %>% head(5)
    
    top5 <- top5 %>% mutate(
      ecart_surface = abs(GrLivArea - input$sim_grlivarea) / input$sim_grlivarea,
      ecart_qualite = abs(as.numeric(OverallQual) - as.numeric(input$sim_overallqual)) / 10,
      ecart_garage  = abs(GarageCars - input$sim_garagecars) / max(1, input$sim_garagecars),
      ecart_soussol = abs(TotalBsmtSF - input$sim_totalbsmtsf) / max(1, input$sim_totalbsmtsf),
      ecart_annee   = abs(YearBuilt - input$sim_yearbuilt) / 50
    )
    
    labels_criteres <- c("la surface habitable", "la qualité globale", "le nombre de places de garage",
                         "la surface du sous-sol", "l'année de construction")
    
    top5$critere_fort <- apply(top5[, c("ecart_surface","ecart_qualite","ecart_garage","ecart_soussol","ecart_annee")],
                               1, function(v) labels_criteres[which.min(v)])
    
    tags$div(style = "display:flex; gap:12px; flex-wrap:wrap;",
             lapply(seq_len(nrow(top5)), function(i) {
               tags$div(style = "flex:1; min-width:160px;",
                        bien_card(top5$nom_complet[i], top5$GrLivArea[i], top5$SalePrice[i], top5$OverallQual[i],
                                  accents_biens[((i-1) %% length(accents_biens)) + 1],
                                  paste0("proche de votre recherche sur ", top5$critere_fort[i]))
               )
             })
    )
  })
}

shinyApp(ui = ui, server = server)