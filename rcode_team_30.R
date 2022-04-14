
# Libraries ---------------------------------------------------------------
library("tidyverse")
library("tidymodels")
library("vip")
library("doParallel") 
library("ggplot2")
library("gt")
library("MASS")
library("glmnet")
doParallel::registerDoParallel()



# Load data ---------------------------------------------------------------
# https://www.kaggle.com/competitions/data-prices-advanced-regression-techniques/data
# Assumes that data is stored locally in a sub-folder ~/data

data_load <- read.csv('data/train.csv', stringsAsFactors = F)
data_withheld <- read.csv('data/test.csv', stringsAsFactors = F) # use for final evaluation


# EDA ---------------------------------------------------------------------
data_load %>% glimpse() # review sample data

summary(data_load) # summary statistics

sum(sapply(data_load[,1:81], typeof) == "character") # number of character columns
sum(sapply(data_load[,1:81], typeof) == "integer") # number of character columns

sum(is.na(data_load)) / (nrow(data_load) *ncol(data_load)) # % of data missing in train data
sum(is.na(data_load)) / (nrow(data_load) *ncol(data_load)) # % of data missing in test data


# VISUALIZATIONS
# https://www.kaggle.com/code/pradeeptripathi/predicting-data-prices-using-r
# very useful for creating visualizations!


data_load <- data_load[data_load$GrLivArea<=4000,] # remove outliers from GrLivArea

# look at missing data
missing_indices <- sapply(data_load,function(x) sum(is.na(x)))
missing_summary <- data.frame(index = names(data_load),missing_values=missing_indices) 
missing_summary[missing_summary$missing_values > 0,] %>% arrange(-missing_values) %>%
  gt() %>% 
  tab_header(
    title = md("Missing Values"),
    subtitle = md("Training data set")
)



## Combining data sets -------------------------------------------------------
data_withheld$SalePrice <- NA
data_withheld$isTrain <- 0 # so we can separate it later
data_load$isTrain <- 1

data <- rbind(data_load,data_withheld) # combined data, can be split on "isTrain"

## Prepare Variables --------------------------------------------------------

# Ally
# Change NA ally to none
data$Alley <- as.character(data$Alley)
data$Alley[which(is.na(data$Alley))] <- "None"
table(data$Alley)


# MasVnrType
# Changing NA in MasVnrType to None
data$MasVnrType1 <- as.character(data$MasVnrType)
data$MasVnrType1[which(is.na(data$MasVnrType))] <- "None"
data$MasVnrType <- as.factor(data$MasVnrType1)
data <- subset(data,select = -MasVnrType1)
table(data$MasVnrType)


# LotFrontage
# Imputing missing Lot Frontage by the median
data$LotFrontage[which(is.na(data$LotFrontage))] <- median(data$LotFrontage,na.rm = T)

# FireplaceQu
# Changing NA in FireplaceQu to None
data$FireplaceQu1 <- as.character(data$FireplaceQu)
data$FireplaceQu1[which(is.na(data$FireplaceQu))] <- "None"
data$FireplaceQu <- as.factor(data$FireplaceQu1)
data <- subset(data, select = -FireplaceQu1)

# PoolQC
# Changing NA in PoolQC to None
data$PoolQC1 <- as.character(data$PoolQC)
data$PoolQC1[which(is.na(data$PoolQC))] <- "None"
data$PoolQC <- as.factor(data$PoolQC1)
data <- subset(data,select = -PoolQC1)

# Fence
# Changing NA in Fence to None
data$Fence1 <- as.character(data$Fence)
data$Fence1[which(is.na(data$Fence))] <- "None"
data$Fence <- as.factor(data$Fence1)
data <- subset(data,select = -Fence1)

# MiscFeature
# Changing NA in MiscFeature to None
data$MiscFeature1 <- as.character(data$MiscFeature)
data$MiscFeature1[which(is.na(data$MiscFeature))] <- "None"
data$MiscFeature <- as.factor(data$MiscFeature1)
data <- subset(data,select = -MiscFeature1)

# GarageType
# Changing NA in GarageType to None
data$GarageType1 <- as.character(data$GarageType)
data$GarageType1[which(is.na(data$GarageType))] <- "None"
data$GarageType <- as.factor(data$GarageType1)
data <- subset(data,select = -GarageType1)

# GarageYrBlt
# Changing NA in GarageYrBlt to None
data$GarageYrBlt[which(is.na(data$GarageYrBlt))] <- 0 

# GarageFinish
# Changing NA in GarageFinish to None
data$GarageFinish1 <- as.character(data$GarageFinish)
data$GarageFinish1[which(is.na(data$GarageFinish))] <- "None"
data$GarageFinish <- as.factor(data$GarageFinish1)
data <- subset(data,select = -GarageFinish1)

# GarageQual
# Changing NA in GarageQual to None
data$GarageQual1 <- as.character(data$GarageQual)
data$GarageQual1[which(is.na(data$GarageQual))] <- "None"
data$GarageQual <- as.factor(data$GarageQual1)
data <- subset(data,select = -GarageQual1)

# GarageCond
# Changing NA in GarageCond to None
data$GarageCond1 <- as.character(data$GarageCond)
data$GarageCond1[which(is.na(data$GarageCond))] <- "None"
data$GarageCond <- as.factor(data$GarageCond1)
data <- subset(data,select = -GarageCond1)

# BsmtQual
# Changing NA in BsmtQual to None
data$BsmtQual1 <- as.character(data$BsmtQual)
data$BsmtQual1[which(is.na(data$BsmtQual))] <- "None"
data$BsmtQual <- as.factor(data$BsmtQual1)
data <- subset(data,select = -BsmtQual1)


# BsmtCond
# Changing NA in BsmtCond to None
data$BsmtCond1 <- as.character(data$BsmtCond)
data$BsmtCond1[which(is.na(data$BsmtCond))] <- "None"
data$BsmtCond <- as.factor(data$BsmtCond1)
data <- subset(data,select = -BsmtCond1)

# BsmtExposure
# Changing NA in BsmtExposure to None
data$BsmtExposure1 <- as.character(data$BsmtExposure)
data$BsmtExposure1[which(is.na(data$BsmtExposure))] <- "None"
data$BsmtExposure <- as.factor(data$BsmtExposure1)
data <- subset(data,select = -BsmtExposure1)

# BsmtFinType1
# Changing NA in BsmtFinType1 to None
data$BsmtFinType11 <- as.character(data$BsmtFinType1)
data$BsmtFinType11[which(is.na(data$BsmtFinType1))] <- "None"
data$BsmtFinType1 <- as.factor(data$BsmtFinType11)
data <- subset(data,select = -BsmtFinType11)


# BsmtFinType2
# Changing NA in BsmtFinType2 to None
data$BsmtFinType21 <- as.character(data$BsmtFinType2)
data$BsmtFinType21[which(is.na(data$BsmtFinType2))] <- "None"
data$BsmtFinType2 <- as.factor(data$BsmtFinType21)
data <- subset(data,select = -BsmtFinType21)

# Electrical
# Changing NA in Electrical to None
data$Electrical1 <- as.character(data$Electrical)
data$Electrical1[which(is.na(data$Electrical))] <- "None"
data$Electrical <- as.factor(data$Electrical1)
data <- subset(data,select = -Electrical1)




# Misc NA values to fix
# use 0, unknown, or most common value
# data$Utilities %>% summary() # check tool
data$MasVnrArea[which(is.na(data$MasVnrArea))] <- 0
data$MSZoning[which(is.na(data$MSZoning))] <- "RL" # most common
data$BsmtFullBath[which(is.na(data$BsmtFullBath))] <- 0
data$BsmtHalfBath[which(is.na(data$BsmtHalfBath))] <- 0
data$Functional[which(is.na(data$Functional))] <- "Typ"
data$Utilities[which(is.na(data$Utilities))] <- "AllPub"
data$Exterior1st[which(is.na(data$Exterior1st))] <- "unknown"
data$Exterior2nd[which(is.na(data$Exterior2nd))] <- "unknown"
data$BsmtFinSF1[which(is.na(data$BsmtFinSF1))] <- 0
data$BsmtFinSF2[which(is.na(data$BsmtFinSF2))] <- 0
data$BsmtUnfSF[which(is.na(data$BsmtUnfSF))] <- 0
data$TotalBsmtSF	[which(is.na(data$TotalBsmtSF	))] <- 0
data$KitchenQual[which(is.na(data$KitchenQual))] <- "TA"
data$GarageCars[which(is.na(data$GarageCars))] <- 0
data$GarageArea[which(is.na(data$GarageArea))] <- 0
data$SaleType[which(is.na(data$SaleType))] <- "WD"



# Factorizing
data$MSZoning<- factor(data$MSZoning)
data$Street <- factor(data$Street)
data$LotShape <-factor(data$LotShape )
data$LandContour<-factor(data$LandContour)
data$Utilities<-factor(data$Utilities)
data$LotConfig<-factor(data$LotConfig)
data$LandSlope<-factor(data$LandSlope)
data$Neighborhood<-factor(data$Neighborhood)
data$Condition1<-factor(data$Condition1)
data$Condition2<-factor(data$Condition2)
data$BldgType<-factor(data$BldgType)
data$dataStyle<-factor(data$dataStyle)
data$RoofStyle<-factor(data$RoofStyle)
data$RoofMatl<-factor(data$RoofMatl)
data$Exterior1st<-factor(data$Exterior1st)
data$Exterior2nd<-factor(data$Exterior2nd)
data$ExterQual<-factor(data$ExterQual)
data$ExterCond<-factor(data$ExterCond)
data$Foundation<-factor(data$Foundation)
data$Heating<-factor(data$Heating)
data$HeatingQC<-factor(data$HeatingQC)
data$CentralAir<-factor(data$CentralAir)
data$KitchenQual<-factor(data$KitchenQual)
data$Functional<-factor(data$Functional)
data$PavedDrive<-factor(data$PavedDrive)
data$SaleType<-factor(data$SaleType)
data$SaleCondition<-factor(data$SaleCondition)

# check
data %>% glimpse()


# Other notes
  # normalization is done in recipe
  # log of prices is taken in the recipe

# good discussion here, should follow this example
#https://www.kaggle.com/code/limyenwee/stacked-ensemble-models-top-3-on-leaderboard/notebook


# Variable Selection ------------------------------------------------------
# not performed

data_var_selection <- data %>% 
  dplyr::select(-Id) %>% 
  mutate(SalePrice=log(SalePrice))


lm_var_sel_full <-lm(SalePrice~.,data=data_var_selection)
lm_var_sel_full %>% summary()



## AIC Stepwise ------------------------------------------------------------

# AIC stepwsie (both directions, takes a little to run)
lm_step <- stepAIC(lm_var_sel_full, direction = "both", trace = FALSE)
summary(lm_step)


## LASSO -------------------------------------------------------------------

x_vars <- model.matrix(SalePrice~. , data_var_selection)[,-1]
y_var <- data_var_selection$SalePrice
lambda_seq <- 10^seq(2, -2, by = -.1)

# Splitting the data into test and train
set.seed(13)
train = sample(1:nrow(x_vars), nrow(x_vars)/2)
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(x_vars[train,], y_var[train],
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = x_vars[x_test,])

# Summary
summary(lasso_best)

# Inspecting beta coefficients
coef(lasso_best)

# Plot
plot(glmnet(
  x_vars[train,], y_var[train],alpha = 1), 
  "lambda", label = TRUE
  ) 


## Final selection for model -----------------------------------------------

data_final_vars <- data %>% dplyr::select(
  'Id',
  'SalePrice',
  'isTrain',
  'MSZoning',
  'LotFrontage',
  'LotArea',
  'Street',
  'LotConfig',
  'Neighborhood',
  'BldgType',
  'OverallQual',
  'OverallCond',
  'YearBuilt',
  'YearRemodAdd',
  'Exterior1st',
  'MasVnrType',
  'Foundation',
  'BsmtExposure',
  'BsmtFinType1',
  'BsmtFinSF1',
  'TotalBsmtSF',
  'Heating',
  'CentralAir',
  'GrLivArea',
  'BsmtFullBath',
  'FullBath',
  'HalfBath',
  'KitchenQual',
  'Functional',
  'Fireplaces',
  'FireplaceQu',
  'GarageType',
  'GarageCars',
  'GarageArea',
  'PavedDrive',
  'WoodDeckSF',
  'OpenPorchSF',
  'ScreenPorch',
  'PoolQC',
  'SaleType',
  'SaleCondition'
  )

lm(SalePrice~.,data=data_final_vars %>% dplyr::select(-isTrain,-Id)) %>% summary()
data_final_vars %>% glimpse()

data_final_vars %>% names()

# Modeling ----------------------------------------------------------------


## Split data --------------------------------------------------------------
# separate data from prior join
data_for_training <- data_final_vars %>% filter(isTrain==1) %>% dplyr::select(-isTrain)
data_for_testing <- data_final_vars %>% filter(isTrain==0) %>% dplyr::select(-isTrain)


# look at missing data
missing_indices <- sapply(data_for_training,function(x) sum(is.na(x)))
missing_summary <- data.frame(index = names(data_for_training),missing_values=missing_indices) 
missing_summary[missing_summary$missing_values > 0,] %>% arrange(-missing_values) %>%
  gt() %>% 
  tab_header(
    title = md("Missing Values"),
    subtitle = md("data_for_training")
  )

missing_indices <- sapply(data_for_testing,function(x) sum(is.na(x)))
missing_summary <- data.frame(index = names(data_for_testing),missing_values=missing_indices) 
missing_summary[missing_summary$missing_values > 0,] %>% arrange(-missing_values) %>%
  gt() %>% 
  tab_header(
    title = md("Missing Values"),
    subtitle = md("data_for_testing")
  )


# split training data into validation data sets
set.seed(30)
data_split <- initial_split(data_for_training, prop = 0.8) # Can include strata = ...
data_train <- training(data_split)
data_test  <- testing(data_split)


## Recipe ------------------------------------------------------------------

# for xgb
# prep data: impute, dummy step, normalize
data_rec <- recipe(SalePrice ~ ., data = data_train) %>%
  update_role(Id, new_role = "id variable") %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors(), -all_outcomes()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_log(all_outcomes())

data_prep <- prep(data_rec)
juiced <- juice(data_prep)

# for rf and knn
# prep data: impute, dummy step, normalize
data_rec2 <- recipe(SalePrice ~ ., data = data_train) %>%
  update_role(Id, new_role = "id variable") %>%
  #step_dummy(all_nominal(), -all_outcomes()) %>%
  #step_zv(all_nominal(), -all_outcomes()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_log(all_outcomes())


data_prep2 <- prep(data_rec2)
juiced2 <- juice(data_prep2)

## Models --------------------------------------------------

### Random Forest -----------------

# Set tuning spec
rf_tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

# Set workflow
rf_tune_wf <- workflow() %>%
  add_recipe(data_rec2) %>%
  add_model(rf_tune_spec)

# Train hyperparameters
set.seed(123)
data_folds <- vfold_cv(data_train,v = 5)

set.seed(456)
rf_tune_res <- tune_grid(
  rf_tune_wf,
  resamples = data_folds,
  grid = 20
)

rf_tune_res

rf_tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  dplyr::select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse", title = "Random Forest Tuning Grid")


# adjust grid (based on prior chart)
rf_grid <- grid_regular(
  mtry(range = c(5, 25)),
  min_n(range = c(2, 10)),
  levels = 5
)

rf_grid

# re-tune
set.seed(456)
regular_res_rf <- tune_grid(
  rf_tune_wf,
  resamples = data_folds,
  grid = rf_grid
)

regular_res_rf

regular_res_rf %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(
    title = "Random Forest Tuning Grid",
    y = "rmse"
    )
  


# select best model
best_rf <- select_best(regular_res_rf, metric = "rmse")

final_rf <- finalize_model(
  rf_tune_spec,
  best_rf
)

final_rf

# explore VIP
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(SalePrice~ .,
      data = juice(data_prep2)
  ) %>%
  vip(geom = "point") + 
  labs(title="Random Forest VIP", subtitle = "permutation")

# final workflow
rf_final_wf <- workflow() %>%
  add_recipe(data_rec2) %>%
  add_model(final_rf)

final_res_rf <- rf_final_wf %>% last_fit(data_split)
final_res_rf %>% collect_metrics()


### XGBoost -----------------
# This one may take a bit of time to run on the CV grid tuning!

# Tuning spec
xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

xgb_spec


# Tuning grid
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), data_train),
  learn_rate(),
  size = 20
)

xgb_grid


# xgboost workflow
xgb_wf <- workflow() %>%
  add_recipe(data_rec) %>%
  add_model(xgb_spec)

xgb_wf


# Train hyperparameters
set.seed(123)
xgb_folds <- vfold_cv(data_train,v = 5)

set.seed(456)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = xgb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_res
collect_metrics(xgb_res)


# Visualize results
xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  dplyr::select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse", title = "XGBoost Tuning Grid")


# show best
show_best(xgb_res, "rmse")

# select best
best_xgb <- select_best(xgb_res, "rmse")
best_xgb

# finalize model
final_xgb <- finalize_workflow(
  xgb_wf,
  best_xgb
)

final_xgb

# view vip
final_xgb %>%
  fit(data = data_train) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point") +
  labs(title = "XGBoost Variable Importance")

# metrics
final_res_xgb <- last_fit(final_xgb, data_split)
collect_metrics(final_res_xgb)

### KNN -----------------


# Set tuning spec
knn_spec <- nearest_neighbor(
  neighbors = tune(),
  weight_func = NULL,
  dist_power = NULL
) %>%
  set_mode("regression") %>%
  set_engine("kknn")

# Set workflow
tune_knn <- workflow() %>%
  add_recipe(data_rec2) %>%
  add_model(knn_spec)

# Train hyperparameters
set.seed(123)
data_folds <- vfold_cv(data_train,v = 5)

set.seed(456)
tune_res <- tune_grid(
  tune_knn,
  resamples = data_folds,
  grid = 50
)

tune_res

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  dplyr::select(mean, neighbors) %>%
  pivot_longer(neighbors,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")


# adjust grid
knn_grid <- grid_regular(
  neighbors(range = c(4, 30)),
  levels = 20
)

knn_grid

# re-tune
set.seed(456)
regular_res <- tune_grid(
  tune_knn,
  resamples = data_folds,
  grid = knn_grid
)

regular_res

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(neighbors, mean)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(
    title = "KNN Tuning Grid",
       y = "rmse"
    )


# select best model
best_knn <- select_best(regular_res, "rmse")

final_knn <- finalize_model(
  knn_spec,
  best_knn
)

final_knn

# final workflow
final_wf <- workflow() %>%
  add_recipe(data_rec2) %>%
  add_model(final_knn)

final_res_knn <- final_wf %>% last_fit(data_split)

final_res_knn %>% collect_metrics()




## Linear Regression -------------------------------------------------------

lm_spec <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')

lm_workflow <- workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(data_rec)

final_res_lm <- lm_workflow %>% last_fit(data_split)

final_res_lm %>% collect_metrics()


