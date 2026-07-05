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
              fluidRow(
                column(3, kpi_card("Prix médian", "163 000 $",
                                   hint = "Référence marché", accent = COULEURS$bleu)),
                column(3, kpi_card("Quartier le + cher", "NridgHt",
                                   hint = "315 000 $ médian", accent = COULEURS$vert)),
                column(3, kpi_card("Qualité moyenne", "6.1 / 10",
                                   hint = "OverallQual", accent = COULEURS$orange)),
                column(3, kpi_card("Amplitude", "×21",
                                   hint = "de 35 000 $ à 755 000 $", accent = COULEURS$rouge))
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
                                  card(title = "Distribution des prix par type",
                                       withSpinner(plotOutput("density_prix", height = 380), type = 6),
                                       note_box("Les maisons individuelles couvrent tous les segments de prix ;
                      les duplex restent concentrés sous 200 000 $.")
                                  )
                           ),
                           column(4,
                                  card(title = "Composition du marché",
                                       withSpinner(plotlyOutput("donut_types", height = 380), type = 6),
                                       note_box("8 maisons sur 10 vendues à Ames sont des maisons individuelles.")
                                  )
                           )
                         )
                ),
                
                tabPanel("Quartiers",
                         br(),
                         fluidRow(
                           column(8,
                                  card(title = "Prix médian par quartier",
                                       withSpinner(plotOutput("lollipop_quartiers", height = 520), type = 6),
                                       note_box("Les 25 quartiers forment 5 niveaux de prix distincts.
                      La ligne rouge marque la médiane globale : 163 000 $.")
                                  )
                           )
                         )
                ),
                
                tabPanel("Qualité",
                         br(),
                         fluidRow(
                           column(8,
                                  card(title = "Prix par niveau de qualité",
                                       withSpinner(plotOutput("violin_qualite", height = 520), type = 6),
                                       note_box("Chaque point de qualité supplémentaire génère environ
                      25 000 $ de prime — et la dispersion s'élargit avec la qualité.")
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
                                  card(title = "Prix par quartier — médiane, quartiles et outliers",
                                       withSpinner(highchartOutput("boxplot_quartiers", height = 500), type = 6),
                                       note_box("Survolez chaque boîte pour les statistiques exactes.
                      Northridge Heights concentre 3 fois plus de ventes premium
                      que la moyenne des quartiers.")
                                  )
                           )
                         )
                ),
                
                tabPanel("Composition du marché",
                         br(),
                         fluidRow(
                           column(6,
                                  card(title = "Volume et prix par quartier",
                                       withSpinner(highchartOutput("treemap_quartiers", height = 450), type = 6),
                                       note_box("Surface = volume de ventes, couleur = prix médian.
                      CollgCr et NAmes concentrent 21 % des ventes dans la
                      fourchette médiane.")
                                  )
                           ),
                           column(6,
                                  card(title = "Composition typologique par quartier",
                                       withSpinner(plotlyOutput("stacked_composition", height = 450), type = 6),
                                       note_box("NPkVill est 100 % Townhouses ; OldTown est le quartier
                      le plus diversifié en types de biens.")
                                  )
                           )
                         )
                ),
                
                tabPanel("Profils",
                         br(),
                         fluidRow(
                           column(8,
                                  card(title = "Profil multi-dimensionnel des 5 quartiers les plus chers",
                                       withSpinner(plotOutput("radar_top5", height = 500), type = 6),
                                       note_box("Cinq dimensions normalisées de 0 à 1. StoneBr est le plus
                      équilibré ; NridgHt domine le prix mais pas la surface.")
                                  )
                           )
                         )
                )
              )
      ),

      tabItem(tabName = "acte3",
              h3("Acte 3 · L'environnement crée-t-il une prime ou une décote ?"),
              p("En construction — V11 à V14")
      ),

      tabItem(tabName = "acte4",
              h3("Acte 4 · Quels facteurs influencent vraiment le prix ?"),
              p("En construction — V15 à V21")
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
}

shinyApp(ui = ui, server = server)