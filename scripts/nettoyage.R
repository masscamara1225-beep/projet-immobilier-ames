library(tidyverse)

train <- read_csv("data/train.csv", show_col_types = FALSE)

train$MSSubClass  <- as.factor(train$MSSubClass)
train$MoSold      <- as.factor(train$MoSold)
train$YrSold      <- as.factor(train$YrSold)
train$OverallQual <- factor(train$OverallQual, levels = 1:10, ordered = TRUE)
train$OverallCond <- factor(train$OverallCond, levels = 1:10, ordered = TRUE)

train <- train %>%
  group_by(Neighborhood) %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage), median(LotFrontage, na.rm = TRUE), LotFrontage)) %>%
  ungroup()

train$GarageYrBlt <- ifelse(is.na(train$GarageYrBlt), train$YearBuilt, train$GarageYrBlt)
train$MasVnrArea  <- ifelse(is.na(train$MasVnrArea), 0, train$MasVnrArea)
train$Electrical[is.na(train$Electrical)] <- "SBrkr"

# AJOUT : MasVnrType manquait ici — sans lui, 8 NA résiduels subsistent
na_cols <- c("PoolQC","Fence","Alley","MiscFeature","FireplaceQu",
             "GarageType","GarageFinish","GarageQual","GarageCond",
             "BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2",
             "MasVnrType")
train[na_cols] <- lapply(train[na_cols], function(x) ifelse(is.na(x), "Absent", x))

cat("NA restants :", sum(is.na(train)), "\n")   # doit être 0

train$ScoreEnv <- case_when(
  train$Condition1 %in% c("PosA","PosN") ~ 2,
  train$Condition1 == "Norm"             ~ 0,
  train$Condition1 == "Feedr"            ~ -1,
  TRUE                                   ~ -2
)

train$Era <- cut(train$YearBuilt, breaks = c(1871,1919,1945,1970,1990,2010),
                 labels = c("Pré-1920","1920-1945","1946-1970","1971-1990","1991-2010"))

#
train <- train %>%
  rename(FirstFlrSF = `1stFlrSF`, SecondFlrSF = `2ndFlrSF`, ThreeSsnPorch = `3SsnPorch`)

train <- train %>% mutate(across(where(is.character), as.factor))
write_csv(train, "data/train_clean.csv")
cat("train_clean.csv exporté —", dim(train)[1], "obs,", dim(train)[2], "variables\n")


ames_geo_complet <- ames_raw %>%
  select(PID, Neighborhood) %>%
  inner_join(geo_coords, by = "PID")

cat("Lignes après jointure PID :", nrow(ames_geo_complet), "sur", nrow(ames_raw), "\n")

centroides_quartiers <- ames_geo_complet %>%
  group_by(Neighborhood) %>%
  summarise(lon = mean(Longitude, na.rm = TRUE), lat = mean(Latitude, na.rm = TRUE))

cat("Nombre de quartiers avec coordonnées :", nrow(centroides_quartiers), "\n")
print(sort(unique(centroides_quartiers$Neighborhood)))


carte_data <- train %>%
  group_by(Neighborhood) %>%
  summarise(prix_median = median(SalePrice), n = n()) %>%
  inner_join(centroides_quartiers, by = "Neighborhood")

cat("Quartiers de train matchés avec coordonnées :", nrow(carte_data), "/",
    n_distinct(train$Neighborhood), "\n")