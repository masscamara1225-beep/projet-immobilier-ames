library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)

train <- read.csv("data/train_clean.csv", stringsAsFactors = FALSE)
train$OverallQual <- factor(train$OverallQual, levels = 1:10, ordered = TRUE)

ui <- navbarPage("Marché Immobilier d'Ames · 2006-2010",
                 
                 tabPanel("Accueil", icon = icon("house"),
                          h2("L'enquête", align = "center"),
                          br(),
                          fluidRow(
                            column(2, offset = 1, align = "center",
                                   icon("house", "fa-2x"), h3("163 000 $"), p("Prix médian global")),
                            column(2, align = "center",
                                   icon("star", "fa-2x"), h3("NridgHt"), p("Quartier le + cher")),
                            column(2, align = "center",
                                   icon("ruler-combined", "fa-2x"), h3("1 515 ft²"), p("Surface médiane")),
                            column(2, align = "center",
                                   icon("medal", "fa-2x"), h3("6.1 / 10"), p("Qualité moyenne")),
                            column(2, align = "center",
                                   icon("arrows-up-down", "fa-2x"), h3("×21"), p("De 35k$ à 755k$"))
                          ),
                          hr(),
                          p("En 2007, une maison à Ames valait 755 000 $. La même année, une autre
     valait 35 000 $. Même ville, 5 km d'écart.", align = "center"),
                          p(strong("1 460 transactions, 80 variables, une question :
     qu'est-ce qui fait le prix d'une maison ?"), align = "center")
                 ),
                 
                 tabPanel("Acte 1 · Marché",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("types", "Types de bien :",
                                                 choices = unique(train$BldgType),
                                                 selected = unique(train$BldgType)),
                              width = 3
                            ),
                            mainPanel(
                              plotOutput("density_prix"),
                              width = 9
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