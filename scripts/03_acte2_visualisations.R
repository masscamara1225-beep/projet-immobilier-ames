# V7 : boxplot interactif des prix par quartier
box_data <- data_to_boxplot(train, SalePrice, Neighborhood,
                            add_outliers = TRUE, name = "Prix")

v7 <- highchart() %>%
  hc_xAxis(type = "category", title = list(text = "Quartier")) %>%
  hc_yAxis(title = list(text = "Prix de vente ($)")) %>%
  hc_add_series_list(box_data) %>%
  hc_title(text = "Northridge Heights a 3 fois plus de ventes premium que la moyenne") %>%
  hc_legend(enabled = FALSE)
v7


# V8 : treemap volume et prix par quartier
neigh_df <- train %>%
  group_by(Neighborhood) %>%
  summarise(n = n(), med = median(SalePrice))

v8 <- hchart(neigh_df, "treemap",
             hcaes(x = Neighborhood, value = n, color = med)) %>%
  hc_colorAxis(stops = color_stops(10, c("#c8392b", "#f5f5f5", "#2d7a55")),
               title = list(text = "Prix médian")) %>%
  hc_title(text = "CollgCr concentre 10% des ventes avec des prix dans la moyenne")

v8