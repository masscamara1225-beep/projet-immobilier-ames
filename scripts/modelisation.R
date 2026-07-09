## ============================================================
## 05_modelisation.R
## Comparaison de 5 approches de régression — VERSION FINALE
## ============================================================

library(tidyverse)
library(glmnet)
library(randomForest)
library(Metrics)

train <- read_csv("data/train_clean.csv", show_col_types = FALSE)

# ------------------------------------------------------------
# Utilities : quasi-constante (1459 AllPub vs 1 NoSeWa sur 1460).
# Aucune valeur prédictive réelle, et casse les contrastes glmnet
# dès qu'un split isole la valeur rare d'un seul côté. On la retire.
# ------------------------------------------------------------
train <- train %>% select(-Utilities)

# ------------------------------------------------------------
# Nettoyage + typage sur la TOTALITÉ des données AVANT de splitter
# (garantit que toutes les modalités de facteurs sont représentées
# une seule fois, cohérentes pour train ET validation)
# ------------------------------------------------------------
full_data <- train %>%
  select(-Id) %>%
  mutate(across(where(is.character), as.factor)) %>%
  drop_na()

cat("Observations disponibles :", nrow(full_data), "\n")

# ------------------------------------------------------------
# Split — sur les LIGNES d'un dataset déjà cohérent
# ------------------------------------------------------------
set.seed(42)
idx <- sample(nrow(full_data), 0.8 * nrow(full_data))

df_train <- full_data[idx, ]
df_valid <- full_data[-idx, ]

cat("Train :", nrow(df_train), "| Validation :", nrow(df_valid), "\n")


## ------------------------------------------------------------
## MODÈLE 1 — Régression linéaire simple (variables métier)
## ------------------------------------------------------------
m1 <- lm(SalePrice ~ OverallQual + GrLivArea + Neighborhood + GarageCars + TotalBsmtSF + YearBuilt,
         data = df_train)
pred1 <- predict(m1, newdata = df_valid)
rmse1 <- rmse(df_valid$SalePrice, pred1)
cat("Modèle 1 (lm simple)      — RMSE :", round(rmse1, 0), "| AIC :", round(AIC(m1), 0), "\n")


## ------------------------------------------------------------
## MODÈLE 2 — Régression stepwise (sélection AIC automatique)
## ------------------------------------------------------------
m2_full <- lm(SalePrice ~ OverallQual + GrLivArea + Neighborhood + GarageCars + TotalBsmtSF +
                YearBuilt + FullBath + LotArea + ExterQual + KitchenQual + BsmtQual,
              data = df_train)
m2 <- step(m2_full, direction = "both", trace = 0)
pred2 <- predict(m2, newdata = df_valid)
rmse2 <- rmse(df_valid$SalePrice, pred2)
cat("Modèle 2 (lm stepwise)    — RMSE :", round(rmse2, 0), "| AIC :", round(AIC(m2), 0), "\n")


## ------------------------------------------------------------
## MODÈLE 3 & 4 — Ridge et Lasso
## Matrice construite UNE SEULE FOIS sur full_data, puis découpée
## après coup avec les mêmes indices idx que df_train/df_valid
## ------------------------------------------------------------
X_full <- model.matrix(SalePrice ~ . - 1, data = full_data)
y_full <- full_data$SalePrice

X_train <- X_full[idx, ]
X_valid <- X_full[-idx, ]
y_train <- y_full[idx]
y_valid <- y_full[-idx]

cat("Colonnes X_train :", ncol(X_train), "| Colonnes X_valid :", ncol(X_valid), "\n")  # doivent être identiques

m3 <- cv.glmnet(X_train, y_train, family = "gaussian", alpha = 0)  # Ridge
pred3 <- predict(m3, newx = X_valid, s = "lambda.min")
rmse3 <- rmse(y_valid, pred3)
cat("Modèle 3 (Ridge)          — RMSE :", round(rmse3, 0), "\n")

m4 <- cv.glmnet(X_train, y_train, family = "gaussian", alpha = 1)  # Lasso
pred4 <- predict(m4, newx = X_valid, s = "lambda.min")
rmse4 <- rmse(y_valid, pred4)
n_vars_gardees <- sum(coef(m4, s = "lambda.min") != 0) - 1
cat("Modèle 4 (Lasso)          — RMSE :", round(rmse4, 0), "|", n_vars_gardees, "variables retenues sur", ncol(X_train), "\n")


## ------------------------------------------------------------
## MODÈLE 5 — Random Forest
## ------------------------------------------------------------
set.seed(42)
m5 <- randomForest(SalePrice ~ ., data = df_train, ntree = 500, importance = TRUE)
pred5 <- predict(m5, newdata = df_valid)
rmse5 <- rmse(y_valid, pred5)
cat("Modèle 5 (Random Forest)  — RMSE :", round(rmse5, 0), "\n")


## ------------------------------------------------------------
## TABLEAU RÉCAPITULATIF — pour le rapport et l'oral
## ------------------------------------------------------------
comparaison <- data.frame(
  Modele = c("Régression linéaire simple", "Régression stepwise (AIC)", "Ridge", "Lasso", "Random Forest"),
  RMSE   = c(rmse1, rmse2, rmse3, rmse4, rmse5)
) %>%
  mutate(RMSE_pct_prix_median = round(100 * RMSE / median(train$SalePrice), 1)) %>%
  arrange(RMSE)

print(comparaison)

dir.create("outputs", showWarnings = FALSE)
write_csv(comparaison, "outputs/comparaison_modeles.csv")