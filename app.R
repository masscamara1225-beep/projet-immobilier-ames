ui <- dashboardPage(
  dashboardHeader(title = "Marché Immobilier d'Ames"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("house")),
      menuItem("Acte 1 · Marché", tabName = "acte1", icon = icon("chart-column")),
      menuItem("Acte 2 · Géographie", tabName = "acte2", icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "accueil",
              h2("L'enquête"),
              fluidRow(
                valueBox("163 000 $", "Prix médian global", icon = icon("house"), color = "blue"),
                valueBox("NridgHt", "Quartier le + cher", icon = icon("star"), color = "green"),
                valueBox("×21", "De 35k$ à 755k$", icon = icon("arrows-up-down"), color = "red")
              )
      ),
      
      tabItem(tabName = "acte1",
              fluidRow(
                box(width = 3,
                    checkboxGroupInput("types", "Types de bien :",
                                       choices = unique(train$BldgType),
                                       selected = unique(train$BldgType))
                ),
                box(width = 9, plotOutput("density_prix"))
              )
      ),
      
      tabItem(tabName = "acte2",
              h4("En construction — V7 à V10")
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
}

shinyApp(ui = ui, server = server)