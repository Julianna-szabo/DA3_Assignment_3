#####################
#                   #  
#     Best deals    #
#    for hotels in  #
#       Vienna      #
#                   #
#####################

# Clear memory

rm(list = ls())

# Libraries

library(tidyverse)
library(ggplot2)
library(Hmisc)
library(GGally)
library(caret)
library(rpart.plot)

# Import data
data_url <- "https://raw.githubusercontent.com/Julianna-szabo/DA3_Assignment_3/main/data/raw/hotels-vienna.csv"
df <- read_csv(data_url)

# Take a quick look at the data
glimpse(df)


# Creating sample ---------------------------------------------------------

# DOUBLE CHECK THIS!!!!!!

# Filter to one day

# Let's see what the distribution of day and month looks like

df %>% 
  group_by(month) %>% 
  ggplot(aes(x = month)) +
  geom_histogram()

# So all of them are in November in 2017.

df %>% 
  filter(weekend == 0) %>% 
  filter(holiday == 0)

# Looks like we only have the days we need


# Label engeniering -------------------------------------------------------

# My y variable will be price since that is what I'm trying to predict

# Let's look at what the distribution of the price is

df %>% 
  group_by(price) %>% 
  ggplot(aes(x = price)) +
  geom_histogram()

describe(df$price)

# From domain knowledge, it looks like the mean is pretty good but some of the highest values are too high.
# I will restrict the sample to only include prices below 500 based on the graph
# Also there are no missing values so those do not need to be dropped

df <- df %>% 
  filter(price < 500)

df %>% 
  group_by(price) %>% 
  ggplot(aes(x = price)) +
  geom_histogram()

# Now the distribution looks much better

# Since the goal is price prediction, I will not look into log transformation.
# Since you loose accuracy when converting back, you can loose accuracy in the prediction.

# Feature engeniering -----------------------------------------------------

# Let's take a look at all the different variables that we have in this dataframe

glimpse(df)

# Looks like we have a few groups of variables
# One about location, one about stars and ratings, offers, distance from center
# These will be the different cateogires used for analysis

# Most of the variables look pretty clean but I will create a few additional flags and convert into factors

# Location city center

df %>% 
  group_by(center1label) %>% 
  ggplot(aes(x = center1label)) +
  geom_histogram(stat="count")

# These are all the same no not very useful

df %>% 
  group_by(center2label) %>% 
  ggplot(aes(x = center2label)) +
  geom_histogram(stat="count")

# There is also no change in this one

# Location neighborhood

df %>% 
  group_by(neighbourhood) %>% 
  ggplot(aes(x = neighbourhood)) +
  geom_histogram(stat="count")

View(df %>% 
  group_by(neighbourhood) %>% 
  summarise(
    n = n()
  )
)

# This one has some variation, so we will convert it into a factor
# Some only have one or two values though, so we make it "other" for ease of use
to_filter <- df %>% 
  group_by(neighbourhood) %>% 
  summarise(
    n = n()
  )

to_filter <- to_filter %>% 
  filter(n < 5)

df <- df %>% 
  mutate(
    neighbourhood = ifelse(df$neighbourhood %in% to_filter$neighbourhood, "Other", df$neighbourhood)
  )

df$neighbourhood_f <- as.factor(df$neighbourhood)

rm(to_filter)

# Distance from the City center

df %>% 
  group_by(distance) %>% 
  ggplot(aes(x = distance)) +
  geom_histogram()

df %>% 
  group_by(distance_alter) %>% 
  ggplot(aes(x = distance_alter)) +
  geom_histogram()

# I'm going to create a flag called close to cc1 or cc2 and it will include anything closer than
# a certain value found by using 

# Distance

df %>% 
  ggplot(aes(x = distance, y = price)) +
  geom_point() +
  geom_smooth(method="loess")

df <- df %>% 
  mutate(
    close_to_cc1_flag = ifelse(df$distance < 1, 1, 0),
    close_to_cc2_flag = ifelse(df$distance_alter < 1, 1, 0),
  )

# Offer variable

df %>% 
  group_by(offer_cat) %>% 
  ggplot(aes(x = offer_cat)) +
  geom_histogram(stat = "count")

df %>% 
  group_by(offer_cat) %>% 
  summarise(
    n = n()
  )

# Since there is only one value with 75%, I will rename the 50% to 50%+ and add it there

to_filter <- df %>% 
  group_by(offer_cat) %>% 
  summarise(
    n = n()
  )

to_filter <- to_filter %>% 
  filter(n < 50)


df <- df %>% 
  mutate(
    offer_cat = ifelse(df$offer_cat %in% to_filter$offer_cat, "50%+ offer", df$offer_cat)
  )

# Now we convert to a factor
df$offer_cat_f <- as.factor(df$offer_cat)

# Accommodation type variable

df %>% 
  group_by(accommodation_type) %>% 
  ggplot(aes(x = accommodation_type)) +
  geom_histogram(stat = "count")

df %>% 
  group_by(accommodation_type) %>% 
  summarise(
    n = n()
  )

# Looks like the two main types are Apartments and Hotels, so I'm going to make everything else "Other"

to_filter <- df %>% 
  group_by(accommodation_type) %>% 
  summarise(
    n = n()
  )

to_filter <- to_filter %>% 
  filter(n < 50)

df <- df %>% 
  mutate(
    accommodation_type = ifelse(df$accommodation_type %in% to_filter$accommodation_type, "Other", df$accommodation_type)
  )

# Now we convert to a factor
df$accommodation_type_f <- as.factor(df$accommodation_type)

# Let's take a look at what our data looks like now

skimr::skim(df)

# The rating columns have some missing values which I will keep though since there are already few observations
# If missing I will fill with 0 and add a flag

df <- df %>% 
  mutate(
    ratingta = ifelse(is.na(df$ratingta) == TRUE, 0, df$ratingta),
    ratingta_flag = ifelse(is.na(df$ratingta) == TRUE, 1, 0),
    ratingta_count = ifelse(is.na(df$ratingta_count) == TRUE, 0, df$ratingta_count),
    ratingta_count_flag = ifelse(is.na(df$ratingta_count) == TRUE, 1, 0),
    rating_count = ifelse(is.na(df$rating_count) == TRUE, 0, df$rating_count),
    rating_count_flag = ifelse(is.na(df$rating_count) == TRUE, 1, 0),
    rating = ifelse(is.na(df$rating) == TRUE, 0, df$rating),
    rating_flag = ifelse(is.na(df$rating) == TRUE, 1, 0),
  )

# Save cleaned file

write_csv(df, "data/clean/hotels_vienna_clean")

# X variable selection ----------------------------------------------------

# correlation table of all the variables

ggcorr(df)

# Looking at the correlation matrix the only ones strongly correlated are the variables
# built on each other, so we can keep them.

# looking at the main structure of some of the variables

# rating

df %>% 
  ggplot(aes(x = rating, y = price)) +
  geom_point() +
  geom_smooth(method="loess")

# Looks okay, no need for transformation
# This will be the same for all the rating fields

df %>% 
  ggplot(aes(x = stars, y = price)) +
  geom_point() +
  geom_smooth(method="loess")

# stars also looks fine

# Model building ----------------------------------------------------------

# Variable groupings

yvar <- "price"

basics <- c("accommodation_type_f", "neighbourhood_f", "distance", "rating", "stars")

distance <- c("distance_alter", "close_to_cc1_flag", "close_to_cc2_flag")

rating <- c("rating_count", "ratingta", "ratingta_count")

offer <- c("offer_cat_f", "offer")

hotel_related <- c("scarce_room", "nnights")

interactions_non_num <- c("accommodation_type_f*close_to_cc1_flag", "accommodation_type_f*close_to_cc2_flag",
                  "accommodation_type_f*neighbourhood_f", "accommodation_type_f*scarce_room",
                  "accommodation_type_f*offer", "accommodation_type_f*offer_cat")

interactions_num <- c("distance*rating", "distance*stars", "distance*distance_alter", "distance*rating_count",
                      "distance*ratingta", "distance*ratingta_count", "distance*nnights")

# Model building

X1 <- c(basics)
X2 <- c(basics, distance)
X3 <- c(basics, distance, rating)
X4 <- c(basics, distance, rating, offer)
X5 <- c(basics, distance, rating, offer, hotel_related)
# Think about using vs not using interactions
LASSO <- c(basics, distance, rating, offer, hotel_related, interactions_num, interactions_non_num)


# The actual models -------------------------------------------------------

model1 <- paste0(" ~ ",paste(X1,collapse = " + "))
model2 <- paste0(" ~ ",paste(X2,collapse = " + "))
model3 <- paste0(" ~ ",paste(X3,collapse = " + "))
model4 <- paste0(" ~ ",paste(X4,collapse = " + "))
model5 <- paste0(" ~ ",paste(X5,collapse = " + "))
LASSO <- paste0(" ~ ",paste(LASSO,collapse = " + "))


# Create holdout set ------------------------------------------------------

# The decision was made to create a holdout/test set to be able to compare all the models to each other,
# since some do and some no not have cross validation options while others don't.

set.seed(19920828)

train_indices <- as.integer(createDataPartition(df$price, p = 0.8, list = FALSE))
df_train <- df[train_indices, ]
df_holdout <- df[-train_indices, ]

dim(df_train)
dim(df_holdout)

# Linear regression -------------------------------------------------------

# Mean squared loss function
mse_lev <- function(pred, y) {
  # Mean Squared Error for log models
  (mean((pred - y)^2, na.rm=T))
}

## N = 5
n_folds=5
# Create the folds
set.seed(19920828)

folds_i <- sample(rep(1:n_folds, length.out = nrow(df_train) ))
# Create results
model_results_cv <- list()

for (i in (1:5)){
  model_name <-  paste0("model",i)
  model_pretty_name <- paste0("(",i,")")
  
  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))
  
  # Initialize values
  rmse_train <- c()
  rmse_test <- c()
  
  model_work_data <- lm(formula,data = df_train)
  BIC <- BIC(model_work_data)
  nvars <- model_work_data$rank -1
  r2 <- summary(model_work_data)$r.squared
  
  # Do the k-fold estimation
  for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
    # Train sample: all except test_i
    data_train <- df_train[-test_i, ]
    # Test sample
    data_test <- df_train
    # Estimation and prediction
    model <- lm(formula,data = data_train)
    prediction_train <- predict(model, newdata = data_train)
    prediction_test <- predict(model, newdata = data_test)
    
    # Criteria evaluation
    rmse_train[k] <- mse_lev(prediction_train, data_train$price)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_train$price)**(1/2)
    
  }
  
  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}


t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    dplyr::summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1

# Model 3 best in terms of BIC and model 1 best in terms of RMSE.
# I will use model 3 since model 1 is probably too simplistic and we are getting this
# result due to the small sample size

# LASSO -------------------------------------------------------------------

# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = n_folds)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# Formula
formula <- formula(paste0(yvar,LASSO))

set.seed(19920828)
lasso_model <- caret::train(formula,
                            data = df_train,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)

lasso_final_model <- lasso_model$finalMode

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)

lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse[1, 1])




# CART model --------------------------------------------------------------


# CART model with two slipts
# Formula
model_cart_3 <- formula(formula(paste0(yvar,model3)))
model_cart_comp <- formula(formula(paste0(yvar,LASSO)))

# Simple tree
cart1 <- train(
  model_cart_3, data=df_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth = 5),
  na.action = na.pass)

summary(cart1)
pred_cart1 <- predict(cart1, df_train, na.action = na.pass)
rmse_cart1 <- sqrt(mean((pred_cart1 - df_train$price)^2))
# Tree graph
rpart.plot(cart1$finalModel, tweak=1.2, digits=-1, extra=1)


# Complex tree
cart2 <- train(
  model_cart_comp, 
  data=df_train,
  method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth = 5),
  na.action = na.pass)

summary(cart2)
pred_cart2 <- predict(cart2, df_train, na.action = na.pass)
rmse_cart2 <- sqrt(mean((pred_cart2 - df_train$price)^2))
# Tree graph
rpart.plot(cart2$finalModel, tweak=1.2, digits=-1, extra=1)

# Compare the two models

cart_compare <- data.frame(
  "Model" = c("CART Model 1", "CART Model 2"),
  "RMSE in Train" = c(rmse_cart1, rmse_cart2)
)

# Looks like the RMSE of CART2 is lower, so that one will be used for further reference

# Random Forest -----------------------------------------------------------

# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)

# Tuning parameters
tune_grid <- expand.grid(
  .mtry = c(3, 5, 7), # Since 5 is about the square root, I have picked it plus or minus 2
  .splitrule = "variance",
  .min.node.size = c(5, 10) # While we do not have many variables going smaller than this, seems too niche
)

# Simpler random forest
set.seed(19920828)
system.time({
  rf_model_1 <- train(
    model_cart_3,
    data = df_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity",
    na.action = na.omit
  )
})

rf1_RMSE <- summary(rf_model_1$resample$RMSE)
rf1_R_squared <- summary(rf_model_1$resample$Rsquared)

# Complex random forest
set.seed(19920828)
system.time({
  rf_model_2 <- train(
    model_cart_comp,
    data = df_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity",
    na.action = na.omit
  )
})

rf2_RMSE <- summary(rf_model_2$resample$RMSE)
rf2_R_squared <- summary(rf_model_2$resample$Rsquared)

results_rf <- rbind(rf1_RMSE, rf2_RMSE, rf1_R_squared, rf2_R_squared)


# RF model 1 has the lower RSME so that will be used for further reference

# Compare on holdout set --------------------------------------------------

# The linear model
model3_level <- model_results_cv[["model3"]][["model_work_data"]]

# look at holdout RMSE
model3_level_train_rmse <- mse_lev(predict(model3_level, newdata = df_train), df_train$price)**(1/2)
model3_level_holdout_rmse <- mse_lev(predict(model3_level, newdata = df_holdout), df_holdout$price)**(1/2)
model3_level_holdout_rmse


# Lasso model

lasso_predictions_train_rmse <-  mse_lev(predict(lasso_model, s = lasso_model$bestTune$lambda, 
                                   newx = df_train, 
                                   type = "raw"), df_train$price)**(1/2)

lasso_predictions_holdout_rmse <- mse_lev(predict(lasso_model, s = lasso_model$bestTune$lambda, 
                                   newx = df_holdout, 
                                   type = "raw"), df_holdout$price)**(1/2)
lasso_predictions_holdout_rmse

# CART model

# Training set
cart2_pred_train <- predict(cart2, df_train, na.action = na.pass)
cart2_rmse_train <- sqrt(mean((pred_cart2 - df_train$price)^2))

# Holdout set
cart2_pred_holdout <- predict(cart2, df_holdout, na.action = na.pass)
cart2_rmse_holdout <- sqrt(mean((pred_cart2 - df_holdout$price)^2))


# Random Forest

# Training set

rf_predicted_probabilities_train <- predict(rf_model_1, newdata = df_train, type = "raw")
df_train$rf_prediction <- rf_predicted_probabilities_train
rf_rmse_train <- RMSE(df_train$rf_prediction, df_train$price)

rf_predicted_probabilities_holdout <- predict(rf_model_1, newdata = df_holdout, type = "raw")
df_holdout$rf_prediction <- rf_predicted_probabilities_holdout
rf_rmse_holdout <- RMSE(df_holdout$rf_prediction, df_holdout$price)

# Summary

rmse_compare <- data.frame(
  "Model" = c("Linear Model", "LASSO Model","CART Tree", "Random Forest"),
  "RMSE in Train" = c(model3_level_train_rmse, lasso_predictions_train_rmse, cart2_rmse_train, rf_rmse_train),
  "RMSE in Holdout" = c(model3_level_holdout_rmse, lasso_predictions_holdout_rmse, cart2_rmse_holdout, rf_rmse_holdout)
)

rmse_compare


# Predictions -------------------------------------------------------------

# Run the prdiction on the whole dataset

rf_predicted_probabilities <- predict(rf_model_1, newdata = df, type = "raw")
df$rf_prediction <- rf_predicted_probabilities

# Calculate residuals

df$rf_prediction_res <- df$price - df$rf_prediction 

# Check the ones with the smallest (negative) residuals 

hotels$bestdeals <- ifelse(hotels$lnprice_resid %in% tail(sort(hotels$lnprice_resid, decreasing=TRUE),5),TRUE,FALSE)

best_deals <- df %>% top_n( -5 , rf_prediction_res ) %>% 
  select( hotel_id , price , rf_prediction , rf_prediction_res )

df$bestdeals <- ifelse(df$rf_prediction_res %in% tail(sort(df$rf_prediction_res, decreasing=TRUE),5),TRUE,FALSE)


# Graph the predictions vs the actual numbers

df %>% 
  ggplot() +
  geom_point(aes(x = rf_prediction, y = price)) +
  geom_line( aes( x = price , y = price ) , color = "darkgreen", size = 1.2) +
  geom_point(aes(x = rf_prediction, y = price, color=bestdeals, shape=bestdeals), size = 1.2, fill= "deeppink", alpha = 0.8, show.legend=F, na.rm = TRUE) +
  scale_shape_manual(name='',values=c(16,21)) +
  scale_colour_manual(name='',values=c("azure3", "black")) +
  labs( x = "Predicted prices", y = "Actual prices")

# TRY USING LOG SINCE BEST DEAL WOULD BE LOW

df <- df %>% 
  mutate(
    log_price = log(price),
    log_price_prediction = log(predict(rf_model_1, newdata = df, type = "raw"))
  )

# Calculate residuals

df$log_prediction_res <- df$log_price - df$log_price_prediction 

# Check the ones with the smallest (negative) residuals 

df %>% top_n( -5 , log_prediction_res ) %>% 
  select( hotel_id , price, log_price , log_price_prediction , log_prediction_res )
