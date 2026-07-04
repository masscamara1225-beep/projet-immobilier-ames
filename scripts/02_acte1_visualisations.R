# Acte 1 - Visualisations V3 a V6
# Projet Ames - Camara Massaram

library(ggplot2)
library(dplyr)
library(scales)

train <- read.csv("data/train_clean.csv", stringsAsFactors = FALSE)

# V3 : density plot des prix par type de bien

v3 <- ggplot(train, aes(x = SalePrice, fill = BldgType)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(labels = label_dollar(scale = 1/1000, suffix = "k")) +
  labs(
    title = "Les maisons individuelles couvrent tous les segments de prix",
    subtitle = "Les duplex restent sous 200 000 $",
    x = "Prix de vente", y = "Densité", fill = "Type de bien"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())

v3
ggsave("outputs/V3_density_bldgtype.png", v3, width = 10, height = 6, dpi = 300)

# V5 : lollipop chart du prix median par quartier
quartiers <- train %>%
  group_by(Neighborhood) %>%
  summarise(prix_median = median(SalePrice))

v5 <- ggplot(quartiers, aes(x = prix_median, y = reorder(Neighborhood, prix_median))) +
  geom_segment(aes(xend = 0, yend = Neighborhood), color = "#d8d4cc") +
  geom_point(aes(color = prix_median), size = 3) +
  scale_color_distiller(palette = "Blues", direction = 1) +
  geom_vline(xintercept = 163000, color = "red", linetype = "dashed") +
  scale_x_continuous(labels = label_dollar(scale = 1/1000, suffix = "k")) +
  labs(
    title = "NridgHt est 4 fois plus cher que BrDale",
    subtitle = "Ligne rouge : médiane globale (163 000 $)",
    x = "Prix médian", y = NULL, color = "Prix"
  ) +
  theme_minimal()

v5
ggsave("outputs/V5_lollipop_quartiers.png", v5, width = 10, height = 8, dpi = 300)


# V6 : violin plot des prix par niveau de qualite

train$OverallQual <- factor(train$OverallQual, levels = 1:10, ordered = TRUE)

v6 <- ggplot(train, aes(x = OverallQual, y = SalePrice, fill = OverallQual)) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.7) +
  scale_fill_manual(values = colorRampPalette(c("#deebf7", "#2d5fa8"))(10)) +
  scale_y_continuous(labels = label_dollar(scale = 1/1000, suffix = "k")) +
  labs(
    title = "La qualité 10/10 vaut 7 fois plus que la qualité 1/10",
    subtitle = "La dispersion des prix s'élargit avec la qualité",
    x = "Qualité générale (OverallQual)", y = "Prix de vente"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

v6
ggsave("outputs/V6_violin_qualite.png", v6, width = 10, height = 6, dpi = 300)

