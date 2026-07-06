ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$div(
      class = "brand",
      tags$span(class = "brand-dot"),
      tags$span(class = "brand-text",
                tags$span(class = "brand-main", "Immobilier Ames"),
                tags$span(class = "brand-sub", "Enquête sur le prix des maisons"))
    ),
    titleWidth = 280
  ),

  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "main_tabs",
      
      tags$div(class = "side-section", "L'enquête"),
      menuItem("Accueil", tabName = "accueil", icon = icon("house")),
      menuItem("Acte 1 · Marché", tabName = "acte1", icon = icon("chart-column")),
      menuItem("Acte 2 · Géographie", tabName = "acte2", icon = icon("map")),
      menuItem("Acte 3 · Environnement", tabName = "acte3", icon = icon("tree")),
      menuItem("Acte 4 · Facteurs", tabName = "acte4", icon = icon("magnifying-glass-chart")),
      menuItem("Acte 5 · Temporel", tabName = "acte5", icon = icon("clock")),
      
      tags$div(class = "side-section", "Analyse avancée"),
      menuItem("Machine Learning", tabName = "ml", icon = icon("robot")),
      menuItem("Recommandations", tabName = "reco", icon = icon("lightbulb")),
      
      selectInput("quartiers_filtre", "Filtrer par quartier :",
                  choices = c("Tous", sort(unique(train$Neighborhood))),
                  selected = "Tous")
    ),
    
    tags$div(class = "side-footer",
             tags$div("CAMARA Massaram · LOGBO Axelle"),
             tags$div("Visualisation de Données · DATA SCIENCE.IA UFRMI · 2025-2026")
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css",
                href = paste0("style.css?v=", as.numeric(Sys.time())))
    ),
    tabItems(
      tabItem(tabName = "accueil",
              page_header(
                title = "Une maison peut en valoir 21 fois une autre",
                meta  = "Ames, Iowa · 1 460 transactions · 2006-2010 · Kaggle House Prices"
              ),
              section_subtitle(
                "En 2007, une maison à Ames valait 755 000 $. La même année, une autre
     valait 35 000 $. Même ville, 5 kilomètres d'écart. Notre enquête
     parcourt 1 460 transactions et 80 variables pour répondre à une seule
     question : qu'est-ce qui fait le prix d'une maison ?"
              ),
              tags$div(style = "display:flex; gap:16px; flex-wrap:wrap;",
                       tags$div(style = "flex:1; min-width:150px;",
                                kpi_card("Prix médian", "163 000 $", hint = "Référence marché", accent = COULEURS$bleu)),
                       tags$div(style = "flex:1; min-width:150px;",
                                kpi_card("Quartier le + cher", "NridgHt", hint = "315 000 $ médian", accent = COULEURS$vert)),
                       tags$div(style = "flex:1; min-width:150px;",
                                kpi_card("Surface médiane", "1 515 ft²", hint = "Maison typique", accent = COULEURS$orange)),
                       tags$div(style = "flex:1; min-width:150px;",
                                kpi_card("Qualité moyenne", "6.1 / 10", hint = "OverallQual", accent = COULEURS$jaune)),
                       tags$div(style = "flex:1; min-width:150px;",
                                kpi_card("Amplitude", "×21", hint = "de 35 000 $ à 755 000 $", accent = COULEURS$rouge))
              ),
              fluidRow(
                column(12,
                       card(title = "L'enquête en 5 actes",
                            tags$ul(class = "obj-list",
                                    tags$li(strong("Acte 1 · Quoi ?"), " — Ce qui se vend : types de biens, quartiers, qualité"),
                                    tags$li(strong("Acte 2 · Où ?"), " — La géographie des prix : NridgHt vs BrDale, écart ×3.9"),
                                    tags$li(strong("Acte 3 · Environnement"), " — Prime parc +41%, décote route -28%"),
                                    tags$li(strong("Acte 4 · Pourquoi ?"), " — Les vrais facteurs : qualité, surface, garage"),
                                    tags$li(strong("Acte 5 · Quand ?"), " — La crise de 2008 et la saisonnalité des ventes")
                            )
                       )
                )
              ),
              fluidRow(
                column(12, tags$h3(class = "section-title", "Parcours guidé"))
              ),
              fluidRow(
                column(4,
                       actionLink("nav_acte1", class = "parcours-card",
                                  tags$div(
                                    tags$h4("📊  Acte 1 · Quoi ?"),
                                    tags$p("Ce qui se vend : types de biens, quartiers et qualité.")
                                  )
                       )
                ),
                column(4,
                       actionLink("nav_acte2", class = "parcours-card",
                                  tags$div(
                                    tags$h4("🗺️  Acte 2 · Où ?"),
                                    tags$p("La géographie des prix : NridgHt vs BrDale, écart ×3.9.")
                                  )
                       )
                ),
                column(4,
                       actionLink("nav_acte3", class = "parcours-card",
                                  tags$div(
                                    tags$h4("🌳  Acte 3 · Environnement"),
                                    tags$p("Prime parc +41 %, décote route artérielle −28 %.")
                                  )
                       )
                )
              ),
              fluidRow(
                column(4,
                       actionLink("nav_acte4", class = "parcours-card",
                                  tags$div(
                                    tags$h4("🔍  Acte 4 · Pourquoi ?"),
                                    tags$p("Les vrais facteurs du prix : qualité, surface, garage.")
                                  )
                       )
                ),
                column(4,
                       actionLink("nav_acte5", class = "parcours-card",
                                  tags$div(
                                    tags$h4("📉  Acte 5 · Quand ?"),
                                    tags$p("La crise de 2008 et la saisonnalité des ventes.")
                                  )
                       )
                ),
                column(4,
                       actionLink("nav_reco", class = "parcours-card",
                                  tags$div(
                                    tags$h4("💡  Recommandations"),
                                    tags$p("5 stratégies par profil : acheteur, vendeur, investisseur...")
                                  )
                       )
                )
              )
      ),

      tabItem(tabName = "acte1",
              page_header(
                title = "Quoi se vend sur ce marché ?",
                meta  = "Acte 1 · Types de biens · Quartiers · Qualité"
              ),
              section_subtitle(
                "Le marché d'Ames est dominé à 83 % par des maisons individuelles,
     avec des prix allant de 35 000 $ à 755 000 $. Trois angles pour
     comprendre ce qui se vend : le type de bien, le quartier, la qualité."
              ),
              tabsetPanel(
                id = "acte1_subtabs",
                type = "tabs",
                tabPanel("Distribution",
                         br(),
                         fluidRow(
                           column(8,
                                  card(title = "80 % des maisons se vendent entre 80 000 $ et 300 000 $ — mais des villas atteignent 755 000 $",
                                       withSpinner(plotlyOutput("histo_prix", height = 420), type = 6),
                                       note_box("La ligne rouge marque la médiane (163 000 $). La distribution
                  est asymétrique à droite : quelques maisons très chères tirent
                  la moyenne (181 000 $) au-dessus de la médiane.")
                                  )
                           )
                         )
                ),
                
                tabPanel("Types de biens",
                         br(),
                         fluidRow(
                           column(3,
                                  wellPanel(
                                    checkboxGroupInput("types", "Types de bien :",
                                                       choices = unique(train$BldgType),
                                                       selected = unique(train$BldgType))
                                  )
                           ),
                           column(5,
                                  card(title = "Les maisons individuelles couvrent tous les segments — les duplex restent sous 200 000 $",
                                       withSpinner(plotOutput("density_prix", height = 380), type = 6),
                                       note_box("Chaque courbe montre la répartition des prix d'un type de bien.
                                                 Plus la courbe s'étale à droite, plus le type atteint des prix élevés.")
                                  )
                           ),
                           column(4,
                                  card(title = "8 maisons sur 10 vendues à Ames sont des maisons individuelles",
                                       withSpinner(plotlyOutput("donut_types", height = 380), type = 6),
                                       note_box("Les townhouses (TwnhsE + Twnhs) pèsent 11 % du marché ;
                                                 duplex et bi-familles se partagent les 6 % restants.")
                                  )
                           )
                         )
                ),
                
                tabPanel("Quartiers",
                         br(),
                         fluidRow(
                           column(8,
                                  card(title = "NridgHt est 4 fois plus cher que BrDale — même ville, 5 km de distance",
                                       withSpinner(plotOutput("lollipop_quartiers", height = 520), type = 6),
                                       note_box("La ligne rouge marque la médiane globale (163 000 $).
                                                 Les 25 quartiers se répartissent en 5 paliers de prix distincts.")
                                  )
                           )
                         )
                ),
                
                tabPanel("Qualité",
                         br(),
                         fluidRow(
                           column(8,
                                  card(title = "La qualité 10/10 vaut 7 fois plus que la 1/10 — et la dispersion s'élargit avec la qualité",
                                       withSpinner(plotOutput("violin_qualite", height = 520), type = 6),
                                       note_box("Chaque violon montre la distribution des prix pour un niveau de qualité.
                                                En moyenne, chaque point de qualité supplémentaire vaut ~25 000 $.")
                                  )
                           )
                         )
                )
              )
      ),

      tabItem(tabName = "acte2",
              page_header(
                title = "Où sont les maisons les plus chères ?",
                meta  = "Acte 2 · Dispersion · Composition · Profils de quartiers"
              ),
              section_subtitle(
                "NridgHt affiche 315 000 $ de prix médian, BrDale 80 000 $ — un écart
     de ×3.9 dans la même ville, à 5 kilomètres de distance. La géographie
     des prix se lit en trois temps : dispersion, composition, profils."
              ),
              tabsetPanel(
                id = "acte2_subtabs",
                type = "tabs",
                
                tabPanel("Dispersion",
                         br(),
                         fluidRow(
                           column(12,
                                  card(title = "Northridge Heights a 3 fois plus de ventes premium que la moyenne d'Ames",
                                       withSpinner(highchartOutput("boxplot_quartiers", height = 500), type = 6),
                                       note_box("Survolez chaque boîte pour la médiane, les quartiles et les valeurs extrêmes.
                                                 Les points isolés sont les ventes atypiques (outliers).")
                                  )
                           )
                         )
                ),
                
                tabPanel("Composition du marché",
                         br(),
                         fluidRow(
                           column(6,
                                  card(title = "CollgCr concentre 10 % des ventes — le quartier le plus actif du marché",
                                       withSpinner(highchartOutput("treemap_quartiers", height = 450), type = 6),
                                       note_box("Surface du rectangle = volume de ventes · couleur = prix médian.
                                                 Avec NAmes, ils totalisent 21 % des transactions, dans la fourchette médiane.")
                                  )
                           ),
                           column(6,
                                  card(title = "NPkVill est 100 % Townhouses · OldTown est le plus diversifié",
                                       withSpinner(plotlyOutput("stacked_composition", height = 450), type = 6),
                                       note_box("Chaque barre montre la répartition des types de biens d'un quartier,
                                                ramenée à 100 %. Cliquez sur la légende pour isoler un type.")
                                  )
                           )
                         )
                ),
                
                tabPanel("Profils",
                         br(),
                         fluidRow(
                           column(8,
                                  card(title = "StoneBr est le quartier le plus équilibré — NridgHt domine le prix mais pas la surface",
                                       withSpinner(plotOutput("radar_top5", height = 500), type = 6),
                                       note_box("Cinq dimensions normalisées de 0 à 1 : prix, surface, qualité,
                                                garage, récence. Plus le polygone est large, plus le profil est complet.")
                                  )
                           )
                         )
                )
              )
      ),

      tabItem(tabName = "acte3",
              page_header(
                title = "L'environnement crée-t-il une prime ou une décote ?",
                meta  = "Acte 3 · Condition1 · Prime parc +41 % · Décote route −28 %"
              ),
              section_subtitle(
                "Vivre près d'un parc rapporte 68 000 $ de plus que vivre près d'une
     route artérielle. Cet acte quantifie l'impact de l'environnement
     immédiat sur le prix de vente."
              ),
              tabsetPanel(
                id = "acte3_subtabs",
                type = "tabs",
                tabPanel("Impact sur le prix",
                         br(),
                         tags$div(class = "empty-state", "V11 Barres Condition1 · V12 Scatter coloré — en construction (Axelle)")
                ),
                tabPanel("Rareté des espaces verts",
                         br(),
                         tags$div(class = "empty-state", "V13 Nightingale Rose · V14 Waffle Chart — en construction (Axelle)")
                )
              )
      ),

      tabItem(tabName = "acte4",
              page_header(
                title = "Quels facteurs influencent vraiment le prix ?",
                meta  = "Acte 4 · Corrélations · 3D · Réseau · Random Forest"
              ),
              section_subtitle(
                "OverallQual (r = 0.79), GrLivArea (r = 0.71) et GarageCars (r = 0.64) :
     les trois piliers du prix, confirmés par le Random Forest."
              ),
              tabsetPanel(
                id = "acte4_subtabs",
                type = "tabs",
                tabPanel("Corrélations",
                         br(),
                         tags$div(class = "empty-state", "V15 Heatmap · V16 Scatter + lm() · V20 Réseau — en construction (Axelle)")
                ),
                tabPanel("Vue 3D",
                         br(),
                         tags$div(class = "empty-state", "V17 Bubble Chart · V18 Scatter 3D ★ — en construction (Axelle)")
                ),
                tabPanel("Profils premium",
                         br(),
                         tags$div(class = "empty-state", "V19 Parallel Coordinates · V21 Importance RF — en construction (Axelle)")
                )
              )
      ),
      
      tabItem(tabName = "acte5",
              page_header(
                title = "Comment les prix évoluent-ils dans le temps ?",
                meta  = "Acte 5 · Crise 2008 · Ères de construction · Saisonnalité"
              ),
              section_subtitle(
                "167 000 $ en 2007, 155 000 $ en 2010 : −7,2 % en 36 mois. La crise a
     réduit les prix mais n'a pas changé les règles du marché."
              ),
              tabsetPanel(
                id = "acte5_subtabs",
                type = "tabs",
                tabPanel("Évolution 2006-2010",
                         br(),
                         tags$div(class = "empty-state", "V22 Line Graph interactif — en construction (Axelle)")
                ),
                tabPanel("Ères et saisonnalité",
                         br(),
                         tags$div(class = "empty-state", "V23 Ridgeline · V24 Heatmap saisonnalité — en construction (Axelle)")
                )
              )
      ),
      
      tabItem(tabName = "ml",
              page_header(
                title = "Modélisation prédictive",
                meta  = "Régression linéaire · Random Forest · k-NN · Soumission Kaggle"
              ),
              section_subtitle(
                "Trois modèles pour valider les conclusions de l'enquête et prédire
     le prix sur les 1 459 maisons du fichier test Kaggle."
              ),
              tags$div(class = "empty-state", "Coefficient plot · Importance RF · k-NN · Score Kaggle — en construction (Axelle)")
      ),
      
      tabItem(tabName = "reco",
              page_header(
                title = "5 recommandations stratégiques",
                meta  = "Acheteur · Vendeur · Investisseur · Promoteur · Urbaniste"
              ),
              section_subtitle(
                "Cinq recommandations différenciées par profil d'acteur, ancrées
     dans les résultats des cinq actes de l'enquête."
              ),
              tags$div(class = "empty-state", "Tableau des recommandations R1 à R5 — en construction")
      ), 

      tabItem(tabName = "acte5",
              h3("Acte 5 · Comment les prix évoluent-ils dans le temps ?"),
              p("En construction — V22 à V24")
      ),

      tabItem(tabName = "ml",
              h3("Modélisation prédictive"),
              p("En construction — Régression · Random Forest · k-NN · Kaggle")
      ),

      tabItem(tabName = "reco",
              h3("5 recommandations stratégiques"),
              p("En construction — par profil d'acteur")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$nav_acte1, updateTabItems(session, "main_tabs", "acte1"))
  observeEvent(input$nav_acte2, updateTabItems(session, "main_tabs", "acte2"))
  observeEvent(input$nav_acte3, updateTabItems(session, "main_tabs", "acte3"))
  observeEvent(input$nav_acte4, updateTabItems(session, "main_tabs", "acte4"))
  observeEvent(input$nav_acte5, updateTabItems(session, "main_tabs", "acte5"))
  observeEvent(input$nav_reco,  updateTabItems(session, "main_tabs", "reco"))
  donnees <- reactive({
    if (input$quartiers_filtre == "Tous") {
      train
    } else {
      train %>% filter(Neighborhood == input$quartiers_filtre)
    }
  })
  
  output$density_prix <- renderPlot({
    donnees() %>%
      filter(BldgType %in% input$types) %>%
      ggplot(aes(x = SalePrice, fill = BldgType)) +
      geom_density(alpha = 0.5) +
      scale_fill_brewer(palette = "Set2") +
      scale_x_continuous(labels = label_dollar(scale = 1/1000, suffix = "k")) +
      labs(title = "Distribution des prix par type de bien",
           x = "Prix de vente", y = NULL, fill = "Type") +
      theme_minimal() +
      theme(axis.text.y = element_blank())
  })
  output$lollipop_quartiers <- renderPlot({
    train %>%
      group_by(Neighborhood) %>%
      summarise(prix_median = median(SalePrice)) %>%
      ggplot(aes(x = prix_median, y = reorder(Neighborhood, prix_median))) +
      geom_segment(aes(xend = 0, yend = Neighborhood), color = "#d8d4cc") +
      geom_point(aes(color = prix_median), size = 3) +
      scale_color_distiller(palette = "Blues", direction = 1) +
      geom_vline(xintercept = median_global, color = "red", linetype = "dashed") +
      scale_x_continuous(labels = label_dollar(scale = 1/1000, suffix = "k")) +
      labs(x = "Prix médian", y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$violin_qualite <- renderPlot({
    ggplot(donnees(), aes(x = OverallQual, y = SalePrice, fill = OverallQual)) +
      geom_violin(trim = FALSE, alpha = 0.8) +
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.7) +
      scale_fill_manual(
        values = setNames(colorRampPalette(c("#deebf7", "#2d5fa8"))(10), levels(train$OverallQual)),
        drop = TRUE
      ) +
      scale_x_discrete(drop = TRUE) +
      scale_y_continuous(labels = label_dollar(scale = 1/1000, suffix = "k")) +
      labs(x = "Qualité générale", y = "Prix") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  output$donut_types <- renderPlotly({
    bldg_df <- donnees() %>%
      filter(BldgType %in% input$types) %>%
      count(BldgType) %>%
      arrange(desc(n))
    
    plot_ly(bldg_df, labels = ~BldgType, values = ~n,
            type = "pie", hole = 0.55, textinfo = "label+percent") %>%
      layout(showlegend = FALSE)
  })
  
  output$boxplot_quartiers <- renderHighchart({
    box_data <- data_to_boxplot(train, SalePrice, Neighborhood,
                                add_outliers = TRUE, name = "Prix")
    highchart() %>%
      hc_xAxis(type = "category", title = list(text = "Quartier")) %>%
      hc_yAxis(title = list(text = "Prix de vente ($)")) %>%
      hc_add_series_list(box_data) %>%
      hc_legend(enabled = FALSE)
  })
  
  output$treemap_quartiers <- renderHighchart({
    neigh_df <- train %>%
      group_by(Neighborhood) %>%
      summarise(n = n(), med = median(SalePrice))
    
    hchart(neigh_df, "treemap",
           hcaes(x = Neighborhood, value = n, color = med)) %>%
      hc_colorAxis(stops = color_stops(10, c("#c8392b", "#f5f5f5", "#2d7a55")))
  })
  
  output$stacked_composition <- renderPlotly({
    top15 <- train %>% count(Neighborhood, sort = TRUE) %>% head(15) %>% pull(Neighborhood)
    
    comp_df <- train %>%
      filter(Neighborhood %in% top15) %>%
      count(Neighborhood, BldgType) %>%
      group_by(Neighborhood) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup()
    
    plot_ly(comp_df, x = ~Neighborhood, y = ~pct, color = ~BldgType, type = "bar") %>%
      layout(barmode = "stack", yaxis = list(title = "", tickformat = ".0%"),
             xaxis = list(title = ""), legend = list(orientation = "h", y = -0.15))
  })
  output$radar_top5 <- renderPlot({
    top5 <- train %>%
      group_by(Neighborhood) %>%
      summarise(
        prix = median(SalePrice),
        surface = median(GrLivArea),
        qualite = median(as.numeric(OverallQual)),
        garage = median(GarageCars),
        recence = median(YearBuilt)
      ) %>%
      arrange(desc(prix)) %>%
      head(5)
    
    normalise <- function(x) (x - min(x)) / (max(x) - min(x))
    radar_vals <- as.data.frame(lapply(top5[, -1], normalise))
    rownames(radar_vals) <- top5$Neighborhood
    radar_df <- rbind(rep(1, 5), rep(0, 5), radar_vals)
    
    couleurs <- c("#c8392b", "#2d5fa8", "#d4890a", "#2d7a55", "#1e7a7a")
    
    fmsb::radarchart(radar_df,
                     pcol = couleurs,
                     pfcol = paste0(couleurs, "44"),
                     plwd = 2, cglcol = "grey", axislabcol = "grey",
                     vlabels = c("Prix", "Surface", "Qualité", "Garage", "Récence"))
    legend("topright", legend = rownames(radar_vals),
           col = couleurs, lty = 1, lwd = 2, bty = "n", cex = 0.9)
  })
  
  output$histo_prix <- renderPlotly({
    p <- ggplot(donnees(), aes(x = SalePrice)) +
      geom_histogram(bins = 50, fill = "#2d5fa8", color = "white") +
      geom_vline(xintercept = median_global, color = "red", linetype = "dashed") +
      scale_x_continuous(labels = scales::dollar) +
      labs(x = "Prix de vente", y = "Nombre de maisons") +
      theme_minimal()
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)