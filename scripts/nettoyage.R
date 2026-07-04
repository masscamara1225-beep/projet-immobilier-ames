library(tidyverse)

# ------------------------------------------------------------
train <- read_csv("data/train.csv", show_col_types = FALSE)

# ------------------------------------------------------------
# 2. Variables faussement numériques (Cours 1)
# ------------------------------------------------------------
train$MSSubClass  <- as.factor(train$MSSubClass)
train$MoSold      <- as.factor(train$MoSold)
train$YrSold      <- as.factor(train$YrSold)
train$OverallQual <- factor(train$OverallQual, levels = 1:10, ordered = TRUE)
train$OverallCond <- factor(train$OverallCond, levels = 1:10, ordered = TRUE)

# ------------------------------------------------------------
# 3. Imputation LotFrontage par médiane du quartier
# ------------------------------------------------------------
train <- train %>%
  group_by(Neighborhood) %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage),
                              median(LotFrontage, na.rm = TRUE),
                              LotFrontage)) %>%
  ungroup()

# ------------------------------------------------------------
# 4. GarageYrBlt : NA -> année de construction 
# ------------------------------------------------------------
train$GarageYrBlt <- ifelse(is.na(train$GarageYrBlt), train$YearBuilt, train$GarageYrBlt)

# ------------------------------------------------------------
# 5. MasVnrArea : NA -> 0 ; Electrical : NA -> mode
# ------------------------------------------------------------
train$MasVnrArea <- ifelse(is.na(train$MasVnrArea), 0, train$MasVnrArea)
train$Electrical[is.na(train$Electrical)] <- "SBrkr"

# ------------------------------------------------------------
# 6. NA structurels -> "Absent" (inclut FireplaceQu, oublié dans la V1 mais présent dans ton diagnostic : 690 NA)
# ------------------------------------------------------------
na_cols <- c("PoolQC","Fence","Alley","MiscFeature","FireplaceQu",
             "GarageType","GarageFinish","GarageQual","GarageCond",
             "BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2")
train[na_cols] <- lapply(train[na_cols], function(x) ifelse(is.na(x), "Absent", x))

cat("NA restants :", sum(is.na(train)), "\n")   # doit être proche de 0

# ------------------------------------------------------------
# 7. Variables synthétiques
# ------------------------------------------------------------
train$ScoreEnv <- case_when(
  train$Condition1 %in% c("PosA","PosN") ~ 2,
  train$Condition1 == "Norm"             ~ 0,
  train$Condition1 == "Feedr"            ~ -1,
  TRUE                                   ~ -2
)

train$Era <- cut(train$YearBuilt,
                 breaks = c(1871,1919,1945,1970,1990,2010),
                 labels = c("Pré-1920","1920-1945","1946-1970","1971-1990","1991-2010"))

# ------------------------------------------------------------
# 8. Export
# ------------------------------------------------------------
train <- train %>% mutate(across(where(is.character), as.factor))
write_csv(train, "data/train_clean.csv")
cat("train_clean.csv exporté —", dim(train)[1], "obs,", dim(train)[2], "variables\n")


