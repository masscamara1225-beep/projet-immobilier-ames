## DESCRIPTION ET DIAGOSTIC DU DATASET

library(dplyr)
library(tidyverse)

train <- read_csv("./data/train.csv")
cat("Dimensions initiales :", dim(train), "\n")

# ------------------------------------------------------------
# 1. Bornes métier — valeurs quantitatives impossibles -> NA
# ------------------------------------------------------------

# YearBuilt / YearRemodAdd / GarageYrBlt : aucune maison à Ames avant 1800,
# ni après l'année de vente. GarageYrBlt = 2207 est un typo connu du dataset (-> 2007).
train$YearBuilt    <- ifelse(train$YearBuilt < 1800 | train$YearBuilt > as.numeric(train$YrSold), NA, train$YearBuilt)
train$YearRemodAdd <- ifelse(train$YearRemodAdd < 1800 | train$YearRemodAdd > as.numeric(train$YrSold), NA, train$YearRemodAdd)
train$GarageYrBlt  <- ifelse(train$GarageYrBlt < 1800 | train$GarageYrBlt > as.numeric(train$YrSold) + 1, NA, train$GarageYrBlt)

# Surfaces et comptages : négatif = impossible physiquement
surface_vars <- c("LotArea","LotFrontage","GrLivArea","TotalBsmtSF","1stFlrSF","2ndFlrSF",
                  "GarageArea","WoodDeckSF","OpenPorchSF","MasVnrArea")
train[surface_vars] <- lapply(train[surface_vars], function(x) ifelse(x < 0, NA, x))

# GrLivArea : 2 outliers connus et documentés par le créateur du dataset
# (>4000 pi² vendus <300k$, probablement des ventes atypiques/erreurs) -> à signaler, pas forcément supprimer
cat("Maisons GrLivArea > 4000 pi2 :", sum(train$GrLivArea > 4000, na.rm = TRUE), "\n")

# ------------------------------------------------------------
# 2. Sentinelles isolées — valeurs hors codebook officiel -> NA
# ------------------------------------------------------------

cat("\n=== Distribution complète de OverallQual ===\n")
print(table(train$OverallQual))   # doit être strictement 1 à 10



cat("\n=== Distribution complète de MoSold ===\n")
print(table(train$MoSold))        # doit être strictement 1 à 12

train$OverallQual <- ifelse(!train$OverallQual %in% 1:10, NA, train$OverallQual)
train$OverallCond <- ifelse(!train$OverallCond %in% 1:10, NA, train$OverallCond)
train$MoSold       <- ifelse(!train$MoSold %in% 1:12, NA, train$MoSold)
train$YrSold       <- ifelse(!train$YrSold %in% 2006:2010, NA, train$YrSold)

# MSSubClass : codes fermés selon le codebook (data_description.txt)
codes_valides <- c(20,30,40,45,50,60,70,75,80,85,90,120,150,160,180,190)
train$MSSubClass <- ifelse(!train$MSSubClass %in% codes_valides, NA, train$MSSubClass)

# ------------------------------------------------------------
# 3. Normalisation des catégorielles (espaces, casse)
# ------------------------------------------------------------

char_cols <- names(train)[sapply(train, is.character)]
train[char_cols] <- lapply(train[char_cols], function(x) trimws(x))
# Attention : ne PAS mettre en majuscules ici, les codes du codebook (ex. "RL", "GdPrv")
# sont sensibles à la casse et doivent matcher exactement data_description.txt

# ------------------------------------------------------------
# 4. Doublons (après normalisation)
# ------------------------------------------------------------

n_avant <- nrow(train)
train <- train %>% distinct()
cat("\nDoublons supprimés :", n_avant - nrow(train), "\n")
cat("Id dupliqués restants :", sum(duplicated(train$Id)), "\n")

# ------------------------------------------------------------
# 5. Bilan
# ------------------------------------------------------------

cat("\nDimensions finales :", dim(train), "\n")
cat("NA totaux après passe de validation :\n")
print(colSums(is.na(train))[colSums(is.na(train)) > 0])


summary(train$GarageYrBlt)
sort(unique(train$GarageYrBlt), decreasing = TRUE)[1:5]