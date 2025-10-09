

##############################################################
#### F23_MDSSB-MET-01-A_Data Science Tools in R
####
#### Session 5: Factors, and Logistic Regression / Classification
####
#### Recommended Reading: R for Data Science
#### URL: https://r4ds.had.co.nz/
#### Chapter 15: Factors
#### https://www.tmwr.org/models
#### https://www.tidymodels.org/ (esp.: https://www.tidymodels.org/start/recipes/)
####
#### An Introduction to Statistical Learning with R
#### URL: https://www.statlearning.com/
#### Chapter on logistic regression
####
#### Armin Müller, Constructor University
##############################################################



# 0. packages
library(tidyverse)
library(lubridate)
library(nycflights13)
library(tidymodels)



# 1. Factors (brief introduction)

# - categorical variable (binary or multivariate)
# - stores values as an integer (more efficient than character)
# - assigns name to every integer
# - can be ordered

# Can serve as response / dependent variable in ...
## - logistic regression (binary)
## - ordinal & multinomial regression (multivariate, ordered or not)
## - support vector machines, random forest and other ML algorithms (classification problems)


# 1.1 disadvantages of a string
# (1) not ordered
x1 <- c("Dec", "Apr", "Jan", "Mar")
?sort()
sort(x1)

# (2) typos
x2 <- c("Dec", "Apr", "Jam", "Mar")
# especially with hand-typed data


# 1.2 Create factors

# a. the simplest way
?as_factor()
as_factor(x1)
as.factor(x2)
# not ordered
# typos

# b. using levels
# create a list of valid levels
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)


# . create a factor from a string

# Option 1: with factor()
?factor()
y1 <- factor(x = x1, 
             levels = month_levels, 
             ordered = is.ordered(month_levels))
y1
sort(y1) # factor is sorted

# Option 2: parse_factor() - recommended
y1a <- parse_factor(x1, 
                    levels = month_levels, 
                    ordered = is.ordered(month_levels))
y1a
sort(y1a) # factor is sorted



# typos automatically converted to NA
y2 <- factor(x2, levels = month_levels, ordered = is.ordered(month_levels))
y2 # error becomes NA
y2a <- parse_factor(x2, levels = month_levels, ordered = is.ordered(month_levels))
y2a # error becomes NA, plus there is additional information about the error !
# this is useful in larger data sets

# have levels assume the order in which they appear in the data
?unique() # get an overview of the different observations in a 
f1 <- parse_factor(x1, levels = unique(x1), ordered = is.ordered(month_levels))
f1
sort(f1)

# More functions 
#fct_inorder(): by the order in which they first appear.
#fct_infreq(): by number of observations with each level (largest first)
#fct_inseq(): by numeric value of level.

?fct_inorder()

f1 <- x1 %>% 
  factor() %>% 
  fct_inorder() # we do not set the predefined months as levels here
head(sort(f1))


f2 <- flights$carrier %>% 
  factor() %>% 
  fct_infreq()
head(sort(f2))



# c. access the set of valid levels
levels(f1) # this factor has only the 4 levels present in the data
levels(f2)




# 2. Data preparation

# Last week we used Ordinary Least Squares (OLS) Regression to predict a numerical outcome.
# Logistic Regression is a common approach to analyzing and predicting binary categorical outcomes.
# In Machine Learning, predicting categorical outcomes is called classification.


# 2.1 Prepare a dependent variable

library(lubridate)

# Let's assume 6 minutes is a threshold for meaningful arrival delays (like DB), 
# where passengers shift from tolerance to being annoyed.
# We take the flights data set and try to predict if planes arrive more than 30 minutes late.


flight_data <- flights %>% 
  mutate(
    arr_delay_dummy = ifelse(arr_delay >= 6, "late", "on_time"), # Convert the arrival delay to a dummy
    arr_delay_ord = ordered(arr_delay_dummy, levels = c("on_time", "late")), # ordered factor for logistic regression in base-R
    arr_delay = factor(arr_delay_dummy), # regular factor for Tidymodels logistic regression
    date = lubridate::as_date(time_hour) # We will use the date (not date-time) for pre-processing
  ) %>% 
  # Include the weather data
  #inner_join(weather, by = c("origin", "time_hour")) %>% 
  # Only retain the specific columns we will use
  select(dep_time, origin, dest, air_time, distance, 
         carrier, date, arr_delay, arr_delay_ord, time_hour) %>% 
  na.omit() %>% # Exclude missing data
  # For calculations it is better to have character variables converted to factors.
  # Be careful with qualitative variables that have many different values, like destination. They slow down calculations.
  mutate_if(is.character, as.factor) 



# Check the share of flights that are delayed 6 minutes or more on arrival
flight_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))


# take a look at the data
glimpse(flight_data) # Variables, types and first observations

summary(flight_data) # Distributions



library(skimr)
skim(flight_data) # more detailed summary



# 2.2 Split training and test data

## split the data
set.seed(0421) # This enables the analysis to be reproducible when random numbers are used 
# Put 75% of the data into the training set 
data_split <- initial_split(flight_data, prop = 0.60)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)




# 3. Logistic Regression in statistical analysis

# 3.1 Calculate the model
# take the ordered factor for logistic regression in base-R
LogReg1 <- glm(arr_delay_ord ~ origin + air_time, # formula
               data = train_data, # data
               family = "binomial") # probability distribution for logistic regression
summary(LogReg1)
# origin is automatically dis-aggregated into dummy variables
table(train_data$origin)
# one airport is selected as the reference category (Intercept);


# 3.2 Assess the model
# Goodness of fit measures are relative, rather than absolute:
## the lower log-likelihood, the better
# see: https://en.wikipedia.org/wiki/Likelihood_function
?logLik()
logLik(LogReg1)



# AIC and BIC are criteria similar to adjusted R2 in OLS regression, but based on Log-Likelihood
# Interpretation: the lower the value, the better the performance.
# They do not show you the overall explained variation.
# But: they show you how well the model performs given the number of predictors used.
## both punish complexity to avoid over-fitting.
## BIC punishes more strictly than AIC, so it can lead to smaller models with fewer predictors
# For more info, see:
# https://en.wikipedia.org/wiki/Akaike_information_criterion
# https://en.wikipedia.org/wiki/Bayesian_information_criterion

AIC(LogReg1)
BIC(LogReg1)

## avoid McFadden's R2, it lacks theoretical foundation


# 3.3 Interpreting coefficients

# a. in Log-odds (additive interpretation like in OLS)

## Dummy Variable
LogReg1$coefficients[2]
# Departing from JFK decreases the log-odds of a 6 minutes delay by 0.15876 points compared to Newark.

## metric variable
LogReg1$coefficients[4]
# With every minute of air-time the log-odds of a 6 minutes delay decreases by 0.00021866.

# Convert to odds ratio (multiplicative interpretation)
exp(LogReg1$coefficients[2])
# Departing from JFK decreases the odds of a 6 minutes delay by the factor 0.85 compared to Newark.
## Odds of 1 = 50/50
## Odds of 2 = 2/1
## Odds of 0.5 = 1/2

# To convert log-odds to probability, follow these 3 steps:
# a. get the case you want to calculate 
LogReg1$coefficients
## say a plane departing from JFK with 100 minutes airtime
CASE_log_odds <- LogReg1$coefficients[1] + # Intercept
                  LogReg1$coefficients[2] +  # JFK departure
                  100*LogReg1$coefficients[4] # 100 minutes airtime
CASE_log_odds
# The log-odds of a plane departing from JFK with 100 minutes airtime being delayed by 6 minutes

# b. get the odds by exponentiating
CASE_odds <- exp(CASE_log_odds)
CASE_odds
# The odds of a plane departing from JFK with 100 minutes airtime being delayed by 6 minutes

# c. get the probability 
# If odds = 2, then probability is 
2/(1+2)
# If odds = 0.5, then probability is 
0.5/(1+0.5)

CASE_prob <- CASE_odds/(1+CASE_odds)
CASE_prob
# The predicted probability of a plane departing from JFK with 100 minutes airtime 
# being delayed by 6 minutes or more is 31.73%.







# 4. Logistic Regression in Machine Learning (Tidy Models)

## Tidymodel the easy way:
## let's recreate the example of LogReg1
# Easy way in Tidymodels
LogReg1tm <-   logistic_reg() |> set_engine("glm") |> # Algorithm
  fit(arr_delay ~ # use a regular factor as dependent variable
        origin + air_time, data = train_data, family = "binomial") # Model
glance(LogReg1tm)

# 4.1 Define the algorithm

# Logistic Regression for classification with Maximum Likelihood
# Loss function: Log-Likelihood (logLik)
?logistic_reg()

Classification_ML <- logistic_reg() |>
  set_mode("classification") |> # Machine learning: regression or classification
  set_engine("glm")

Classification_ML|> translate()

# Compare: regression with "generalized linear models" (glm)
linear_reg() |>
  set_mode("regression") |> # Machine learning: regression or classification
  set_engine("glm") |>
  translate()



# 4.2 Build a model with pre-processing steps

# There are always 2 options
## Option 1: pre-processing before calculation
## Option 2: pre-processing in Tidymodels (recommended if possible)


# 4.2.1 Define a model including all predictors (default) with recipe()

flights_model <- recipe(arr_delay ~ ., data = train_data, family = "binomial") 

# Model formula: arr_delay ~ .
# Model data: data = train_data
# Probability distribution: family = "binomial"

# try at home maybe
flights_fit3 <- 
  workflow() %>% 
  add_model(Classification_ML) %>% # the algorithm
  add_recipe(recipe(arr_delay ~ ., data = train_data, family = "binomial")) %>% 
  fit(data = train_data)



# 4.2.2 Excluding variables from calculation

# In the previous session, we defined model formulas to clarify which variables go into a model.
# Here, we try a different approach: specifying roles for the variables.

summary(flights_model)
# Variables can have any arbitrary role, but there are 
# two special standard roles, "predictor" and "outcome". 
# These two roles are typically required when fitting a model, and implicit in the formula

# If variables are not to be used as predictors (like IDs or time-hour), 
## we can or change their role to exclude them from calculation.

?update_role()

flights_model <- recipe(arr_delay ~ ., data = train_data, family = "binomial") %>% # full model recipe
  update_role(time_hour, new_role = "ID") %>% # change role & exclude from calculation
  step_rm(arr_delay_ord)  # remove alternative dependent variable

# check the variables and their roles in the data:
summary(flights_model)



# 4.2.3 pre-processing with dates

train_data$date
# here we extract more meaningful features from the date variable: weekdays, months and holidays

# Option 1: use mutate() (and remove original variables afterwards) in combination with:
?lubridate::wday()
wday(train_data$date, label = TRUE)

?lubridate::month()
month(train_data$date, label = TRUE)

?timeDate::listHolidays()
timeDate::listHolidays("US") # 18 results
timeDate::listHolidays("DE") #  5 results
# better cross-check (https://publicholidays.de/2023-dates/)
## there are 9 national holidays ....
## Corpus Christi is not one of them
## Result: model for Germany misspecified

## Recommendation:
## More general recommendation: do not blindly trust convenience functions.
## Pre-processing option 1 is often easier and gives you more control.



# go with Option 2: Tidymodels  
flights_model <- 
  recipe(arr_delay ~ ., data = train_data, family = "binomial") %>% 
  step_rm(arr_delay_ord) %>%  # remove alternative dependent variable
  update_role(time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%  # weekday and month
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), # I trust this is right for the US, it's from the official Tidymodels tutorial
               keep_original_cols = FALSE) # remove the date variable, because it is no longer needed
summary(flights_model)



# 4.2.4 pre-processing with factors

# Base-R: factors are dis-aggregated into dummy variables automatically
## see logistic regression example above (origin)

# In Tidymodels we need to do this by hand.

# Option 1: use mutate() and if_else()

# Remember: 1 category is always the reference category and does not get a dummy
table(flight_data$origin)
flight_data %>% 
  transmute(JFK = if_else(origin == "JFK", 1, 0),
            LGA = if_else(origin == "LGA", 1, 0))
# use mutate() in the actual preprocessing


# Option 2: add a step to convert factors to dummies
flights_model <- 
  recipe(arr_delay ~ ., data = train_data, family = "binomial") %>% 
  step_rm(arr_delay_ord) %>%  # remove alternative dependent variable
  update_role(time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE) %>% 
  step_dummy(all_nominal_predictors()) # convert all factors to dummies


# Some factors have a large number of categories.
# They may slow down calculations or cause errors.

# There are several options to handle this
## you can exclude dummies with only 1 observation
?step_zv()
flights_model_do_not_use <- 
  recipe(arr_delay ~ ., data = train_data, family = "binomial") %>% 
  step_rm(arr_delay_ord) %>%  # remove alternative dependent variable
  update_role(time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE) %>% 
  step_dummy(all_nominal_predictors()) %>% # convert all factors to dummies
  step_zv(all_predictors()) # exclude dummies with only 1 observation

## or you can just remove variables with too many groups (carrier & destination)
flights_model <- 
  recipe(arr_delay ~ ., data = train_data, family = "binomial") %>% 
  step_rm(carrier, dest, arr_delay_ord)  %>% # remove factors with too many categories & alternative y
  update_role(time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE) %>% 
  step_dummy(all_nominal_predictors()) # convert all factors to dummies
  
# If you use Option 1, remove carrier and dest before the calculations.



# 4.3 Define workflow and train the model

# define the workflow
flights_wflow <- 
  workflow() %>% 
  add_model(Classification_ML) %>% # the algorithm
  add_recipe(flights_model) # the model


# fit the model on the training data
flights_fit <- 
  flights_wflow %>% 
  fit(data = train_data)

# or save the worflow step ...

flights_fit2 <- 
  workflow() %>% 
  add_model(Classification_ML) %>% # the algorithm
  add_recipe(flights_model) %>%  # the model
  fit(data = train_data)




# 4.6 Inspect the model

# 4.6.1 Goodness of fit

glance(flights_fit)
# here we get log-Likelihood directly
glance(flights_fit)[3]
# log-Likelihood is a relative value and not directly interpretable.
# The lower, the better.

glance(LogReg1tm)



# 4.6.2 Coefficients

# look at the coefficients the coefficients
flights_fit %>% 
  extract_fit_parsnip() %>% 
  tidy() %>% 
  View()


# get a model table in markdown format
library(modelsummary)
?modelsummary()
modelsummary(models = list(LogReg1tm, flights_fit),
             estimate = "{estimate}{stars}",
             gof_omit = c("RMSE"),
             output = "default")
# There is a bug with the Log-Likelihood of the first model

# Log.Lik is Log-Likelihood, the loss function that needs to be minimized.
# When you compare models, the lower it is, the better.



# 4.6.3 Confusion Matrix

# We can calculate the confusion matrix on the training data.

confusion_matrix_train <- predict(flights_fit, train_data) %>% # get predicted classes
  bind_cols(train_data %>% select(arr_delay)) %>% # get test data observations
  mutate(.pred_class = if_else(.pred_class =="late", "Predicted  late", "Predicted on time")) %>% # adjust names to avoid confusion
  count(.pred_class, arr_delay) %>% # sum ob the value combinations
  pivot_wider(names_from = .pred_class, values_from = n) %>% # turn it into proper format
  column_to_rownames(var="arr_delay") # set the row-names
confusion_matrix_train

# On that basis, we can calculate a number of metrics, like accuracy or specificity.
# When you are building a model or have multiple ones to compare, 
# this can tell you how the model is performing and what to expect.

# The confusion matrix and metrics are more interesting for the test data.
# Therefore we discuss them in detail below.




# 4.7 Prediction and model performance

# 4.7.1 get the predictions 

# get the predicted class
predict(flights_fit, test_data)

# get the predicted probabilities
predict(flights_fit, test_data, type = "prob")

# get the predicted log-odds
predict(flights_fit, test_data, type = "raw")



# 4.7.2 get the confusion matrix

# 4.7.2.1 Simple version

confusion_matrix <- predict(flights_fit, test_data) %>% # get predicted classes
  bind_cols(test_data %>% select(arr_delay)) %>% # get test data observations
  mutate(.pred_class = if_else(.pred_class =="late", "Predicted  late", "Predicted on time")) %>% # adjust names to avoid confusion
  count(.pred_class, arr_delay) %>% # sum ob the value combinations
  pivot_wider(names_from = .pred_class, values_from = n) %>% # turn it into proper format
  column_to_rownames(var="arr_delay") # set the rownames
confusion_matrix


# 4.7.2.2 Sophisticated version 
## with option to change the cut-off
cutoff_prob <- 0.3 # default is 0.5
confusion_matrix2 <- predict(flights_fit, test_data, type = "prob") %>% 
  select(delay_prob = .pred_late)  %>% 
  bind_cols(test_data %>% select(arr_delay)) %>%
  mutate(delay      = if_else(arr_delay == "late", "Observed late", "Observed on time"),
         delay_pred = if_else(delay_prob > cutoff_prob, "Predicted  late", "Predicted on time")) %>%
  count(delay_pred, delay) %>%
  pivot_wider(names_from = delay_pred, values_from = n) %>% 
  column_to_rownames(var="delay") 
confusion_matrix2

confusion_matrix

# 4.7.2.3 Tidymodels conf_mat() function

delay_pred <- 
  predict(flights_fit, test_data, type = "prob") |> # get predicted probabilities
  select(delay_prob = .pred_late) |>  # retain probabilities of delay
  bind_cols(predict(flights_fit, test_data)) |> # add predicted classes
  bind_cols(test_data |> select(arr_delay)) # Append observed delays

delay_pred %>% 
  conf_mat(truth = arr_delay, estimate = .pred_class)
# columns and lines are switched ... not optimal ...


# 4.7.3 Metrics and evaluation

# Evaluate the model
# https://en.wikipedia.org/wiki/Confusion_matrix
# https://en.wikipedia.org/wiki/Sensitivity_and_specificity
# https://yardstick.tidymodels.org/

# Those metrics also serve the comparison with other models

## quick overview ... 
delay_pred %>% 
  conf_mat(truth = arr_delay, estimate = .pred_class) |> 
  summary()


# Accuracy

# ... refers to the proportion of correct predictions 
# (both true positives and true negatives) among the total number of cases examined.
confusion_matrix
(confusion_matrix[1,1] + confusion_matrix[2,2])/sum(confusion_matrix)
# 71.78% of flights were correctly predicted as either leaving at least 6 minutes late or not.


# Sensitivity (true positive rate)
# ... refers to the probability of a positive test, conditioned on truly being positive.
# Here: Positive = delay
confusion_matrix
(confusion_matrix[1,1])/(confusion_matrix[1,1] + confusion_matrix[1,2])
# 36.34% of the delayed flights were correctly predicted to to be late.
# This is not so good.


# Specificity (true negative rate)
# ... refers to the probability of a negative test, conditioned on truly being negative.
confusion_matrix
(confusion_matrix[2,2])/(confusion_matrix[2,2] + confusion_matrix[2,1])
# 89.87% of flights that were not delayed were correctly predicted not to be late.



## False Positive Rate = 1-specificity
# ... refers to the probability of a positive test, conditioned on truly being negative.
# Like specificity, it is based on observations that are truely negative,
# therefore False Positive Rate + True Negative Rate = 1
1 - (confusion_matrix[2,2])/(confusion_matrix[2,2] + confusion_matrix[2,1])
# 0.966% of flights that were were not delayed were falsely predicted to be late.






# 4.7.4 ROC curve

# 4.7.4.1 ROC plot for a single model
# join predictions in a data frame
delay_pred <- 
  predict(flights_fit, test_data, type = "prob") |> # get predicted probabilities
  select(delay_prob = .pred_late) |>  # retain probabilities of delay
  bind_cols(test_data |> select(arr_delay)) # Append observed delays


## ROC plot
delay_pred %>% 
  roc_curve(truth = arr_delay, delay_prob
            #, event_level = "second"
            ) %>% 
  autoplot()

delay_pred %>% 
  roc_auc(truth = arr_delay, delay_prob)


# do the same for the other model
delay_pred2 <- 
  predict(LogReg1tm, test_data, type = "prob") |> # get predicted probabilities
  select(delay_prob = .pred_late) |>  # retain probabilities of delay
  bind_cols(test_data |> select(arr_delay)) # Append observed delays

## ROC plot
delay_pred2 %>% 
  roc_curve(truth = arr_delay, delay_prob
            #, event_level = "second"
            ) %>% 
  autoplot()

delay_pred2 %>% 
  roc_auc(truth = arr_delay, delay_prob)



# 4.7.4.2 Model comparison with ROC plots

#compare the models in 1 graph

library(pROC)
table(as.numeric(delay_pred2$arr_delay))

# ROC curve for model1
?roc()
roc_model1 <- roc(response = (as.numeric(delay_pred$arr_delay) ), 
                  predictor = as.numeric(delay_pred$delay_prob))

# ROC curve for model2
roc_model2 <- roc(response = (as.numeric(delay_pred2$arr_delay) ),
                  predictor = as.numeric(delay_pred2$delay_prob))

# Plot ROC curves for both models
plot(roc_model1, col = "blue", main = "ROC Curve Comparison")
lines(roc_model2, col = "red")

# Add AUC values to the plot
text(0.2, 0.4, paste("Model 1 AUC =", round(auc(roc_model1), 2)), col = "blue")
text(0.2, 0.3, paste("Model 2 AUC =", round(auc(roc_model2), 2)), col = "red")

# Add a legend
legend("bottomright", legend = c("Model 1", "Model 2"), fill = c("blue", "red"))


# Interpretation:
# The model the line of which is closest to the top left corner is the best.
# It has the best trade-off between true positives and false positives.






