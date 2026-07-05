ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Marché Immobilier d'Ames"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("house")),
      menuItem("Acte 1 · Marché", tabName = "acte1", icon = icon("chart-column")),
      menuItem("Acte 2 · Géographie", tabName = "acte2", icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
    .skin-blue .main-sidebar { background-color: #1e3a5f; }
    .skin-blue .main-header .navbar { background-color: #2d5fa8; }
    .skin-blue .main-header .logo { background-color: #24487e; }
    .skin-blue .sidebar-menu > li.active > a { border-left-color: #c8392b; }
  "))),
    tabItems(
      tabItem(tabName = "accueil",
              h2("L'enquête"),
              fluidRow(
                valueBox("163 000 $", "Prix médian global", icon = icon("house"), color = "blue", width = 3),
                valueBox("NridgHt", "Quartier le + cher · 315 000 $", icon = icon("star"), color = "green", width = 3),
                valueBox("6.1 / 10", "Qualité moyenne", icon = icon("medal"), color = "yellow", width = 3),
                valueBox("×21", "Amplitude · de 35k$ à 755k$", icon = icon("arrows-up-down"), color = "red", width = 3)
              ),
              fluidRow(
                box(width = 12, title = "Le point de départ", status = "primary",
                    p("En 2007, une maison à Ames valait 755 000 $. La même année, une autre
               valait 35 000 $. Même ville, 5 kilomètres d'écart."),
                    p(strong("1 460 transactions, 80 variables, une seule question :
               qu'est-ce qui fait le prix d'une maison ?"))
                )
              ),
              fluidRow(
                box(width = 12, title = "L'enquête en 5 actes", status = "primary",
                    tags$ul(
                      tags$li(strong("Acte 1 · Quoi ?"), " — Ce qui se vend : types de biens, quartiers, qualité"),
                      tags$li(strong("Acte 2 · Où ?"), " — La géographie des prix : NridgHt vs BrDale, écart ×3.9"),
                      tags$li(strong("Acte 3 · Environnement"), " — Prime parc +41%, décote route -28%"),
                      tags$li(strong("Acte 4 · Pourquoi ?"), " — Les vrais facteurs : qualité, surface, garage"),
                      tags$li(strong("Acte 5 · Quand ?"), " — La crise de 2008 et la saisonnalité des ventes")
                    )
                )
              )
      ),
      
      tabItem(tabName = "acte1",
              fluidRow(
                box(width = 3,
                    checkboxGroupInput("types", "Types de bien :",
                                       choices = unique(train$BldgType),
                                       selected = unique(train$BldgType))
                ),
                box(width = 5, title = "Distribution des prix par type de bien",
                    plotOutput("density_prix")),
                box(width = 4, title = "8 maisons sur 10 sont des maisons individuelles",
                    plotlyOutput("donut_types"))
              ),
              
              fluidRow(
                box(width = 6, title = "Prix médian par quartier", plotOutput("lollipop_quartiers", height = 500)),
                box(width = 6, title = "Prix par niveau de qualité", plotOutput("violin_qualite", height = 500))
              )
      ),
      
      tabItem(tabName = "acte2",
              fluidRow(
                box(width = 12, title = "Northridge Heights a 3 fois plus de ventes premium que la moyenne",
                    highchartOutput("boxplot_quartiers", height = 500))
              ),
              fluidRow(
                box(width = 6, title = "CollgCr concentre 10% des ventes — le quartier le plus actif",
                    highchartOutput("treemap_quartiers", height = 450)),
                box(width = 6, title = "NPkVill est 100% Townhouses · OldTown le plus diversifié",
                    plotlyOutput("stacked_composition", height = 450))
              ),
              fluidRow(
                box(width = 8, title = "StoneBr est le quartier le plus équilibré — NridgHt domine le prix mais pas la surface",
                    plotOutput("radar_top5", height = 500))
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$density_prix <- renderPlot({
    train %>%
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
    ggplot(train, aes(x = OverallQual, y = SalePrice, fill = OverallQual)) +
      geom_violin(trim = FALSE, alpha = 0.8) +
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.7) +
      scale_fill_manual(values = colorRampPalette(c("#deebf7", "#2d5fa8"))(10)) +
      scale_y_continuous(labels = label_dollar(scale = 1/1000, suffix = "k")) +
      labs(x = "Qualité générale", y = "Prix") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  output$donut_types <- renderPlotly({
    bldg_df <- train %>%
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