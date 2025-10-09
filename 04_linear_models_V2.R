##############################################
#### F23_MDSSB-MET-01-A_Data Science Tools in R
####
#### Session 4: Linear Models & Principal Component Analysis
####
####
#### Tidymodels
#### https://www.tmwr.org/
#### https://www.tidymodels.org/
####
#### (Theory & Methods)
#### An Introduction to Statistical Learning with R
#### URL: https://www.statlearning.com/
#### Chapters 2, 3 and 12
#### For basics of machine learning
####
#### Introductory Econometrics (Woolridge)
#### in the library
#### I assunme you had a class on regression before 
#### and know all the Gauss-Markov Assumptions by heart.
#### If not: do a warm-up.
####
#### Armin Müller, Constructor University
##############################################

+


# 1. Preliminaries

# 1.1 packages
# Get new dataset
library(ISLR2) # data
library(tidyverse) 
library(car) # for variance inflation factor
# library(MASS) # stepwiseAIC() - overwrites other functions !!!
library(corrplot)
library(tidymodels) #

# 1.2 Inspection and question
glimpse(Hitters)
?Hitters

# Salary: 1987 annual salary on opening day in thousands of dollars
# HmRun: Number of home runs in 1986
# CHmRun: Number of home runs during his career

# Let's check for missing values
summary(is.na(Hitters)) # 59 Salary observations are missing


# Predictive question:
# Let's try and predict the salaries of the players with NAs.

# Assumptions: Salary ...
## ... depends on Performance
## ... depends on career track
## ... information is missing AT RANDOM



# 1.3 Split the dataset

# Make a separate set for prediction of unknown values
Hit_pred <- Hitters %>% 
  filter(is.na(Salary))

# ... and one for Machine Learning to develop a model to make predictions
Hit <- na.omit(Hitters) # 263 observations ... not much


set.seed(0421)
# ... which we split into training and test data
data_split <- initial_split(Hit, prop = 0.8) # 80% training data is relatively high

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

?training()


# 2. Exploration and pre-processing

# 2.1 Let's inspect the response (dependent variable)

# 2.1.1 Salaries
hist(train_data$Salary)

# This distribution is skewed, which often happens with count data and monetary data.
# This can cause problems for statistical inference (not our primary concern here).
# A common way to deal with this is to take the log of the variable.
hist(log(train_data$Salary))




# 2.1.2 Home Runs made during career

hist(train_data$CHmRun) # also skewed distribution

# Let's look at the Home Runs and Salaries:
train_data |>
  ggplot(aes(x = CHmRun, y = Salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_light()



# ... and look at the data again
train_data |>
  ggplot(aes(x = log(CHmRun +1), y = log(Salary))) +
  # CHmRun +1: log(0) causes infinite values. THis is a quick-and-dirty fix
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_light()

# You can see the data points are more evenly spread, and numerical values are smaller.
# Also, it mostly thins out at the lower rather than the higher numerical end.
# The hierarchy of the observations is preserved.
# That may reduce distortion in OLS regression (can you guess why?).


# 2.1.3 Duration of career
hist(train_data$Years) #lighter skew, let's also get a log version
hist(log(train_data$Years))

# 2.1.4 Runs
hist(train_data$CRuns)
hist(log(train_data$CRuns))




# 3. Regression for statistical analysis in base-R:

# In statistical analysis, we typically want to test hypotheses.
# For example, we might expect that ...

# H1: the more home runs a player had in his career, the higher the salary
# H2: the longer a player's career has been going on, the higher the salary
# H3: the more runs a player had in his career, the higher the salary


# 3.1 Model development

# Equation: y = ß0 + ß1*x1 + ß2*x2 + ... + e

# So we can start developing a model to test the hypotheses.

# As a base-line, we want the null-model for comparison.

# Algorithm: we use the lm() function for a linear model
?lm()
LM0 <- lm(Salary ~ 1, data = train_data)
summary(LM0) # inspection
AIC(LM0)
BIC(LM0)


# Then, we start with the first variable: home runs
LM1 <- lm(Salary ~ CHmRun, data = train_data)
summary(LM1)
AIC(LM1)
BIC(LM1)

# Here is an important metric: the adjusted R2
# It tells us that 24.98% of the variance of the salary is explained by the variable.
# (Note: The multiple R2 has the same interpretation, but is not suitable to model comparison. Who remembers why?)

# now we add the varaible runs: 
LM2 <- lm(Salary ~ CHmRun + CRuns, data = train_data)
summary(LM2)
AIC(LM2)

# explained variance has increased to 29.84% (expect some minor variation)

# Now we add the duration of the career: 
LM3 <- lm(Salary ~ CHmRun + CRuns + Years, data = train_data)
summary(LM3)
AIC(LM3)
# explained variance has increased to 33.7%


# Let's say we are happy for now. 33.7% is already good in the social sciences.



# 3.2 Model inspection

# Multivariate Regression (MLR) using ordinary least squares is based on a series of assumptions which,
# if violated, negatively affect the validity of the model.


# Gauss-Markov Assumptions:

# MLR.1 Linearity in parameters: the underlying population features a linear connection between the variables of interest

# MLR.2 Random sampling: our data is a random sample representative of the population

# MLR.3 No perfect collinearity: variables should not be linear combinations of one another and not too highly correlated

# MLR.4 Zero-conditional mean of the error given the independent variables (includes: no important variable is missing)
### For example: omitting an important x correlated with both y and one or more x (omitted variable bias)
### For example: an x is correlated with the error (endogeneity)

# If MLR 1 to 4 are given, the OLS estimator is unbiased
# (meaning the procedure; individual random samples may still feature bias;
# calculating a model on all possible random samples will give an unbiased estimate, which is what some machine learning algorithms do)

# MLR.5 Homoskedasticity: the error in the population has the same variance given any value of the explanatory variables
# If MLR 1 - 5: correct variance & error estimation; OLS is unbiased & efficient  (BLUE: Best Linear Unbiased Estimator)

# MLR.6 Normal distribution of errors in the population (unobserved)
## Not required for the estimation
## But required for inference in small samples (t-tests, F-tests, confidence intervals, p-values) 
## in large samples, inference is still asymptotically correct

# Log Transformation is closer to normality

# By default, R provides a series of diagnostic plots that help us to check some of the assumptions.
# Note: these plots show residuals from the sample, the population error is unknown.
# Whether these sample residuals are good estimates of the population error depends on ...
## ... whether the sample is representative, ... 
## ... which in turn depends on random sampling (MLR.2)
## So: if the data is not representative, then the model may be bad and you cannot fix it!

plot(LM3)

# Plot 1 shows the residuals (differences between observed and predicted values in the sample) on the y-axis 
## and the fitted (predicted) values on the x-axis.
## It is connected to MLR.1 (linearity in parameters): 
## if the plot shows a random scatter around y = 0, MLR.1 is reasonable
## Any systematic patterns (curves, slopes) indicate the model does not fit the data well
## Additionally, if the residuals spread out, this points to heteroskedasticity (violation of MLR.5)

# Plot 2: Quantile-Quantile (QQ) Plot: compares quantiles of residuals to a theoretical normal distribution.
## If they are normally distributed, the residuals form a straight line along the dots.
## Deviations from the straight line, particularly at the ends (tails), suggest non-normality,
# which violates the normality assumption of residuals (MLR.6).
## Severe deviations could indicate the presence of outliers (see plot 4) or issues with the model specification (MLR.1 & 4).

# Plot 3: square root of the (absolute value of the) standardized residuals versus the fitted values.
## Standardized residuals are scaled by standard deviation;
## they are better suited to comparing residuals across observations;
## If they are bigger than a certain threshold (2 or 3), they may indicate outliers.
## A horizontal line with evenly spread residuals indicates constant variance (Homoskedasticity, MLR.5).
## If there are patterns (slopes, curves), MLR.5 is violated (Heteroskedasticity) and your error estimates may be problematic.

# Plot 4: standardized residuals versus Leverage
## The leverage measures the influence of an observation on the model results.
## Influential outliers have a high standardized residual and a high leverage.
## Cook's Distance values surpassing thresholds of 0.5 or 1 indicate influential outliers.
## Influential outliers can distort the relationship between x and y (MLR.1), 
## and the residuals so that they do not have the zero-conditional mean (MLR.4) and Homoskedasticity (MLR.5)

# Additionally, we should check for multi-collinearity.
# A typical symptom is inflated explained variance.
# So if (adjusted) R2 is near or above 50%, we have to be cautious.
# The car library offers a function to calculate the variance inflation factor,
# which measures the degree of multicollinearity:
library(car)
vif(LM3)
# VIF = 1: no correlation
# 1 < VIF < 3 or 5: moderate collinearity, still acceptable
# VIF > 3 or 5: problematic collinearity
# VIF > 10: severe collinearity 

# Let's examine a full model with all variables:

LMfull = lm(Salary ~ ., data = train_data)
summary(LMfull) # adj. R2 = 59.94%!!!
vif(LMfull)

# The problem here is the correlation between the variables
library(corrplot)
?select()
train_data %>% 
  dplyr::select( -Division, -League, -NewLeague) %>% 
  cor() %>%  # calculate correlation
  corrplot()





# 3.3 Log Models

# Let's check the influence of taking the logs:

summary(LM3)
plot(LM3)

# a. Log-linear model: log(y) ~ x

LM3_logln <- lm(log(Salary) ~ CHmRun + CRuns + Years, data = train_data)
summary(LM3_logln)
plot(LM3_logln)
# The model is still problematic.

# Note: you can transform an additive log-linear into a regular multiplicative model
#log(y) = ß0 + ß1*x1 + ß2*x2
# y = exp(ß0 + ß1*x1 + ß2*x2) = exp(ß0) * exp(ß1*x1) * exp(ß2*x2)


# b. Log-Log model: log(y) ~ log(x)
log(1)
LM3_loglog <- lm(log(Salary) ~ log(CHmRun + 1) + log(CRuns + 1) + log(Years + 1), data = train_data)
summary(LM3_loglog)
plot(LM3_loglog)
# This is much improved, but also still problematic


# Note: you can transform an additive log-log into a regular multiplicative model
# log(y) = ß0 + ß1*log(x1) + ß2*log(x2)
# y = exp(ß0 + ß1*log(x1) + ß2*log(x2))
#   = exp(ß0) * exp(ß1*log(x1)) * exp(ß2*log(x2)) =
#   = exp(ß0) * x1^ß1 * x2^ß2




# 4. Machine Learning basics:

# 4.1 Feature Selection (intro)

## Orthodox statistical analysis includes extensive model development by hand.

# Machine Learning algorithms increasingly automate the selection of predictors.
# This section provides an example of this approach: stepwise selection.
# This is out-dated and not implemented in Tidymodels, but you can use it to get a list of promising variables ...
# ... until you learn more sophisticated appraoches.


# The AIC criterion can help us compare models with the same data set and y (like adj.R2)?
?AIC()

# This approach can be automated in several ways.

# 4.1.1 Backwards elimination

# The backwards elimination appraoch algorithm starts with a full model.
# It eliminates each variable (calculating one model each), 
# checks improvements in AIC gained, and then selects the best model.
# Then it repeats the process until no more improvements are achieved

LMfull_ln = lm(log(Salary) ~ ., data = train_data)
# here I log y in the function, but remove the logged variables from the dataset
summary(LMfull_ln)
AIC(LMfull_ln)
#BIC(LMfull_ln)

?step() # Note: Tidymodels will block this function in the namespace
LMfull_back <- stats::step(LMfull_ln,
                             direction = c("backward"))
summary(LMfull_back)
AIC(LMfull_back)
BIC(LMfull_back)
plot(LMfull_back) # better
vif(LMfull_back) # serious issues


LMfull_back <- MASS::stepAIC(LMfull_ln,
                    direction = c("backward"))
summary(LMfull_back)
AIC(LMfull_back)
BIC(LMfull_back)
plot(LMfull_back)
# Not perfect, but a lot better

# There are still a few outliers, but they are not influential anymore
library(car)
vif(LMfull_back) # some serious problems


# 4.1.2 Forward selection

# Forward selection starts with a small model or nullmodel and adds variables,
# selecting the one that most decreases AIC at each step
# until there are no more improvements.

LM0_ln <-  lm(log(Salary) ~ 1, data = train_data)
# here I log y in the function, but remove the logged variables from the dataset

summary(LM0_ln)

names(train_data)

LM0_forward <- MASS::stepAIC(LM0_ln, # forward regression in the defined scope
           direction = "forward",
           scope =(~ AtBat + Hits +  HmRun + Runs +  RBI + Walks + Years + 
                     CAtBat +CHits + CHmRun + CRuns + CRBI +  CWalks +League +
                     Division + PutOuts + Assists + Errors + Salary + NewLeague)
)
summary(LM0_forward )
AIC(LM0_forward)
vif(LM0_forward) # that sort of looks ok

# 4.1.3 both

# You can combine both appraoches.
# After reaching the end with forward selection, it will try backward elimination, and vice versa.

LM0_both <- MASS::stepAIC(LM0_ln, # forward regression in the defined scope
                    direction = "both",
                    scope =(~ AtBat + Hits +  HmRun + Runs +  RBI + Walks + Years + 
                              CAtBat +CHits + CHmRun + CRuns + CRBI +  CWalks +League +
                              Division + PutOuts + Assists + Errors + Salary + NewLeague)
)
summary(LM0_both)
# Here it was the same result.
AIC(LM0_both)

plot(LM0_both)
# There are some serious issues here

vif(LM0_both)
# There are some serious issues here




# 4.2 Unsupervised Learning: Principal Component Analysis (PCA)

?Hitters
# Remember the high correlation between different variables?

train_data %>% 
  dplyr::select( -Division, -League, -NewLeague) %>% 
  cor() %>% 
  corrplot::corrplot()

# We can use PCA to condense the highly correlated groups of variables into single mathematical entities.
# PCA works best with numeric variables that are highly correlated.
# If your data is a cloud of points, PCA draws lines through the cloud that best represent its shape.
# It works best with normally distributed data (which we may or may not have.)

# The performance indicators for the ongoing year are highly correlated.
# The performance indicators for the entire career are highly correlated.


?prcomp()

# 4.2 Performance in 1986: PCA (exploratory)

hist(train_data$Salary)
?scale()

hist(scale(train_data$Salary, center = TRUE, scale = TRUE))

PCA_86 <- train_data |> 
  dplyr::select(AtBat, Hits, HmRun, Runs, RBI, Walks) |> 
  prcomp(scale. = TRUE) # centred and scaled

# What do we get?
typeof(PCA_86) # a list of course ...
class(PCA_86) # ... with a generic object class

summary(PCA_86)
# Plot variance explained (using standard deviations)
PCA_86$sdev
tibble(PC = 1:6, sdev = PCA_86$sdev) |> 
  mutate(percent = sdev^2/sum(sdev^2) * 100) |>
  ggplot(aes(PC, percent)) + 
  geom_col(fill="steelblue") + 
  labs(x = "Principal component number",
       y = "Percent of variance explained") +
  scale_x_continuous(breaks = seq(1, 6, by = 1)) +
  theme_light()
# The first Principal Component captures almost 80% of the variance in the data.

# Inspect the data along two strongest principal components
# Color represents the number of Home Runs that year
plotdata <- PCA_86$x |> 
  as_tibble() |> 
  bind_cols(train_data |>  select(AtBat, Hits, HmRun, Runs, RBI, Walks, HmRun))
plotdata |> ggplot(aes(PC1, PC2, color = HmRun)) +
  geom_point() +
  scale_color_gradient(low = "yellow", high = "red") +
  theme_dark()
# Remember: PC1 captures almost 80% of the variation in the data
#           PC2 only captures about 10% of the variation in the data
# A high number of Home Runs loads on both components.


# How do the Principal Components reflect the different performance indicators``
# Let's inspect the variable loadings
PCA_86$rotation

PCA_86$rotation |> 
  as_tibble(rownames = "variable") |> 
  pivot_longer(starts_with("PC"), names_to = "PC", values_to = "value") |> 
  ggplot(aes(value, variable)) + 
  geom_col(fill = "steelblue") + 
  #geom_vline(xintercept = 0, color = "blue") +
  facet_wrap(~PC, nrow = 1) + 
  labs(x = "Loadings on the different components",
       y = "Performance indicators") +
  theme_light()
# The first principal components loads in relatively equal terms on all indicators.
# It reflects general performance.
# The others reflect smaller variations connected to a subset of the indicators.


# So: 
# We can interpret PC1 as the performance of the player in 1986 ...
# ... if we can plausibly assume that the indicators capture the full variation of performance.
# This assumption is important and a bit unrealistic; 
## for prediction, it is ok to make it, because test error will prove us right or wrong ultimately.
## for exploratory statistics, it is better to use factor analysis








# 5. Basic Modern Machine Workflow with Tidymodels

library(tidymodels) 

# 5.1 Preliminaries

# 5.1.1 Terminology

# Machine Learning terminology (for this class): 
# ALGORITHM: Machine learning algorithms are procedures that are implemented in code and are run on data.

# LOSS FUNCTION: The algorithm minimizes a loss function (or error function) when applied to data
## common loss functions include:
### OLS Regression: Mean Squared Error (MSE) and Root Mean Squared Error (RMSE)
### Logistic Regression / Maximum Likelihood Estimation: Log-Likelihood
## In statistics, the ALGORITHM is also called the ESTIMATOR (like OLS or Maximum Likelihood)

# MODEL: Machine learning models are output by algorithms and are comprised of model data and a prediction algorithm.
## The model data includes the selection of variables
## The model parameters (i.e. coefficients) which are LEARNED during training and then generalized to predict labels for unlabeled data.


# 5.1.2 Data, hypothesis testing & generalization

# In statistics, we often work with samples of underlying populations.
# Hypotheses are typically tested on random samples drawn from an underlying population.
# Generalization from the sample to the population is expected to hold if ...
## ... (a) the sample is really a random one (and not a biased sample)
## ... (b) the results of the test are significant (and thus can be generalized)


# In machine learning, this process is mimicked in a way.
## We have an underlying data set that we split (randomly) into training and test data.
## Both are random samples of the original data set.
## Therefore, we expect hypothesis tests that are significant on the training data to also be significant in the test data.




# 5.2 Regression with Tidymodels

## Preparation: specify the algorithm (not necessary in Base-R)

## the syntax provides a coherent front end for various R functions 
linear_reg(mode = "regression",  # mode: Machine Learning, either regression or classification
           engine = "lm") |> # algorithm: lm = least squares; switch to glm for Maximum Likelihood
  translate() # get the code in base-R

# Set the algorithm to regular OLS
algorithm_OLS <- linear_reg(mode = "regression",  engine = "lm")
class(algorithm_OLS)
## alternatively
algorithm_OLS <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") # not necessary for "lm", but for "glm"


TM0 <- fit(algorithm_OLS, Salary ~ 1, data = train_data) # fit the model
glance(TM0) # the glance function provides some basic metrics

# A core metric for model evaluation is the Root Mean Squared Error (RMSE)
# It is the square-root of the mean error (i.e. real values - fitted values).
# Note: in analytical statistics, we would refer to this "error" as residuals" (because error is reserved for the underlying population).
# In Machine Learning, we distinguish training error (on training data) from test error (on test data)
# Models are optimized by minimizing a "Loss Function" (in this case RMSE)

# Getting the RMSE requires a bit more calculation: 

?rmse()

TM0 %>%  # take the model
  predict(new_data = train_data) %>% # make the prediction on the training data
  bind_cols(train_data) %>% # connect it to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error



# 5.3 Training the model

# Let's further develop the model on the training data
TM1 <- fit(algorithm_OLS, Salary ~ CHmRun, data = train_data)
glance(TM1)

TM1 %>%  # take the model
  predict(new_data = train_data) %>% # make the prediction on the training data
  bind_cols(train_data) %>% # connect it to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error

# RMSE has improved


# add the Runs
TM2 <- fit(algorithm_OLS, Salary ~ CHmRun + CRuns, data = train_data)

TM2 %>%  # take the model
  predict(new_data = train_data) %>% # make the prediction on the training data
  bind_cols(train_data) %>% # connect it to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error

# Add the years, as above
TM3 <- fit(algorithm_OLS, Salary ~ CHmRun + CRuns + Years, data = train_data)
glance(TM3) 
TM3 %>%  # take the model
  predict(new_data = train_data) %>% # make the prediction on the training data
  bind_cols(train_data) %>% # connect it to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error


# Fit a model with all variables
TMX <- fit(algorithm_OLS, Salary ~ ., data = train_data)
glance(TMX)

TMX %>%  # take the model
  predict(new_data = train_data) %>% # make the prediction on the training data
  bind_cols(train_data) %>% # connect it to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error
# the lowest training error so far, but it might overfit



?extract_fit_parsnip()


# 5.4 Testing the model on the test data

# Now we fit the model to the test data and see how the RSME changes.
# We can use the predict function on the test data set to get predictions.

predict(TMX, new_data = test_data) # the same in Tidymodels and base-R, but: 
typeof(predict(TMX, new_data = test_data)) # Tidymodels: output is a list ...
str(predict(TMX, new_data = test_data)) # ... containing a numeric vector

# you may want to check for infinite values before proceeding
lapply(predict(TMX, test_data), function(x) summary(is.infinite(x)))


# How to calculate the test error in Tidymodels:
# full model
TMX %>%  # take the model
  predict(new_data = test_data) %>% # make the prediction on the test data
  bind_cols(test_data) %>% # connect it to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error


# Model with 3 variables
TM3 %>%  # take the model
  predict(new_data = test_data) %>% # prediction 
  bind_cols(test_data) %>% # connect it to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error



# 5.5 Predict the missing salaries

Hit_pred$Salary <- unlist(predict(TM3, Hit_pred))
#Hit_pred$Salary <- unlist(predict(TMX, Hit_pred))
hist(Hit_pred$Salary)

Hit_pred$Salary #
?Hitters




# So, training and test data are random samples of the population of players for whom we know the salaries.

# Do our predictions truly match the actual salaries of the players whose salaries we do not know?
# It depends on several factors, including ...
## (a) how good the model is
## (b) whether the salary information was missing AT RANDOM or not.
### Missing COMPLETELY AT RANDOM means missingness not being caused by any (observed or unobserved) variables.
### We can safely assume this with simulated data where we created the missing values ourselves.
### With observational data, the situation is much more tricky, and a lot of the time we do not know.

# If information was missing at random, a good model can accurately predict the salaries.
# If information was missing NOT AT RANDOM, like for a specific group (like the wealthiest or the poorest players),
# then this bias may also affect our predictions.

# Therefore, it is imperative to think about the quality of your data, and not to blindly trust algorithms.
# Ask yourself:
# Is the data I have representative of the population I am interested in? (if not, accept it as a limitation)
# Can I realistically expect that missing values are missing at random? (if not, accept it as a limitation)




# 5.6 Alternative: Integrated workflow:
# We can also summarize the procedure in a workflow:

TM3_workflow <- 
  workflow() %>% 
  add_model(algorithm_OLS) %>%  # add the algorithm
  add_formula(Salary ~ CHmRun + CRuns + Years) # add the formula

?fit()  
TM3 <- fit(TM3_workflow, train_data)

# Training error
TM3 %>%  # take the model
  predict(new_data = train_data) %>% # prediction
  bind_cols(train_data) %>% # connect to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error

# Testing error
TM3 %>%  # take the model
  predict(new_data = test_data) %>% # prediction
  bind_cols(test_data) %>% # connect it to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error





# 6. Pre-processing with Tidymodels


# Modern Machine Learning combines advanced feature selection approaches (which we do not cover here)
# and with feature engineering algorithms (such as Principal Component Analysis or PCA).

# In Tidymodels, we can include a processing steps for log-transformation or principal components.

# Again, let's set the algorithm first
algorithm_OLS <- linear_reg(mode = "regression",  engine = "lm")


# 6.1 Log-Transform the Model

# 6.1.1 Log-Linear MOdel
# The dependent variable needs to be log-transformed separately.

TM3_log_ln_wf <- 
  workflow() %>% 
  add_model(algorithm_OLS) %>%  # add the algorithm
  add_formula(log(Salary) ~ CHmRun + CRuns + Years)  # add the formula for log-linear model

# fit the model
TM3logln <- fit(TM3_log_ln_wf, train_data)
glance(TM3logln)


# Training error
TM3logln %>%  # take the model
  predict(new_data = train_data) %>% # prediction
  exp() %>% # exponentiate to get back to original scale
  bind_cols(train_data) %>% # connect to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error

# Testing error
TM3logln %>%  # take the model
  predict(new_data = test_data) %>% # prediction
  exp() %>% # exponentiate to get back to original scale
  bind_cols(test_data) %>% # connect it to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error



# 6.1.2 The Log-Log Model

# Let's log-transform the dependent variable before we do the calculations

train_data$Salary_ln <- log(train_data$Salary)

# Whoever finds a way of doing this in the preprocessing and getting to valid predictions,
# I invite for a coffee!

# We need to add a pre-processing step in the form of a recipe
?recipe()
?step_log()

log_log_recipe <- recipe(Salary_ln ~ CHmRun + CRuns + Years, data = select(train_data, -Salary)) %>%
  step_log(all_numeric_predictors(), # log the predictors for log-log model
           offset = 1) # offset to avoid log(0)


# Create the workflow
TM3_log_log_wf <- 
  workflow() %>%
  add_model(algorithm_OLS) %>% 
  add_recipe(log_log_recipe)  # Add the prepared recipe

# Fit the model using the preprocessed training data
TM3loglog <- fit(TM3_log_log_wf, data = train_data)
glance(TM3loglog)

# Training error
TM3loglog %>%  # take the model
  predict(new_data = train_data) %>% # prediction
  exp() %>% # exponentiate to get back to original scale
  bind_cols(train_data) %>% # connect to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error

# Testing error
TM3loglog %>%  # take the model
  predict(new_data = test_data) %>% # prediction
  exp() %>% # exponentiate to get back to original scale
  bind_cols(test_data) %>% # connect it to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error


# 6.2 Principal-Component Analysis (PCA)

# Remember the high correlation between different variables?
train_data %>% 
  select( -Division, -League, -NewLeague) %>% 
  cor() %>% 
  corrplot()

# We can use PCA to condense the highly correlated groups of variables into single mathematical entities.
# This way, we can make use of most the variation in the data while avoiding multi-collinearity.
# This comes at a cost: the PCA-inputs are not easily interpretable.
# This is a big problem for orthodox analytical statistics!
# In Machine Learning, we can be more relaxed about it since our goal is to predict rather than to test hypotheses.


# 6.2.1 Regular

# Here, we spcify the inputs so that one PC captures performance in the current year,
# and the other overall performance

# Define a recipe with PCA for two specific groups of variables
pca_recipe <- recipe(Salary ~ ., data = select(train_data, -Salary_ln)) %>% # remove Salary_ln from predictors
  step_center(all_numeric_predictors()) %>% # we need to center ...
  step_scale(all_numeric_predictors()) %>% # ... and scale the preditors
  step_pca(c("AtBat", "Hits", "HmRun", "Runs", "RBI", "Walks"), num_comp = 1, prefix = "PCperform2") %>%
  step_pca(c("Years", "CAtBat", "CHits", "CHmRun", "CRuns", "CRBI", "CWalks"), num_comp = 1, prefix = "PCcareer2")


# Create a workflow with PCA and OLS
?workflow()
?add_model()
pca_ols_wf <- workflow() %>%
  add_recipe(pca_recipe) %>%
  add_model(algorithm_OLS)

# Train the model
TM_pca_ols_fit <- fit(pca_ols_wf, data = select(train_data, -Salary_ln))  # remove Salary_ln from predictors
glance(TM_pca_ols_fit)

# Training error
TM_pca_ols_fit  %>%  # take the model
  predict(new_data = train_data) %>% # prediction
  bind_cols(train_data) %>% # connect to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error

# Testing error
TM_pca_ols_fit  %>%  # take the model
  predict(new_data = test_data) %>% # prediction
  bind_cols(test_data) %>% # connect it to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error



# 6.3 Mixing Log and PCA Pre-processing

log(0)
log(1)

log_log_pca_recipe <- recipe(Salary_ln ~ ., data = select(train_data, -Salary)) %>%
  step_log(all_numeric_predictors(), # log the predictors for log-log model
           offset = 1) %>%  # offset to avoid log(0)
  step_center(all_numeric_predictors()) %>% # we need to center ...
  step_scale(all_numeric_predictors()) %>% # ... and scale the preditors
  step_pca(c("AtBat", "Hits", "HmRun", "Runs", "RBI", "Walks"), num_comp = 1, prefix = "PCperform2") %>%
  step_pca(c("Years", "CAtBat", "CHits", "CHmRun", "CRuns", "CRBI", "CWalks"), num_comp = 1, prefix = "PCcareer2")

log_pca_ols_wf <- workflow() %>%
  add_recipe(log_log_pca_recipe) %>%
  add_model(algorithm_OLS)


# Train the model
TM_log_pca_ols_fit <- fit(log_pca_ols_wf, data = select(train_data, -Salary))  # remove Salary_ln from predictors
glance(TM_pca_ols_fit)


# Training error
TM_log_pca_ols_fit  %>%  # take the model
  predict(new_data = train_data) %>% # prediction
  exp() %>% # exponentiate to get back to original scale
  bind_cols(train_data) %>% # connect to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error

# Testing error
TM_log_pca_ols_fit  %>%  # take the model
  predict(new_data = test_data) %>% # prediction
  exp() %>% # exponentiate to get back to original scale
  bind_cols(test_data) %>% # connect it to the observed values
  rmse(truth = Salary, estimate = .pred) # calculate the error



# 7. Presenting the results

# In Machine Learning, the focus is more on prediction than interpretation.
# So the packages provide you with fewer details on your models.

# 7.1 Get a classic model table

# We can get a classic model table from the modelsummary package:
# https://modelsummary.com/articles/modelsummary.html

library(modelsummary)
?modelsummary()

?gof_map() # which summary statistics to display


# ... in the output viewer
modelsummary(models = list(TM_log_pca_ols_fit, TM3loglog, TM3logln), # list() for multiple models
             estimate = "{estimate}{stars}",
             gof_map = c( "nobs", "adj.r.squared", "AIC", "BIC"),
             output = "default")

# ... in markdown format (for a report in a Quarto File!)
modelsummary(models = list(TM_log_pca_ols_fit, TM3loglog, TM3logln),
             estimate = "{estimate}{stars}",
             gof_map = c( "nobs", "adj.r.squared", "AIC", "BIC"),
             output = "markdown")

# Note: models in such tables should have the same dependent variable.
# So models with logged y and regular y are not directly comparable.



# 7.2 Training and Test Error for Machine Learning


# Step 1: calculate the RMSE for each model

# Log-Log model
TM3loglog_train_rmse <- predict(TM3loglog, new_data = train_data) %>%
  exp() %>% 
  bind_cols(train_data) %>%
  rmse(truth = Salary, estimate = .pred)

TM3loglog_test_rmse <- predict(TM3loglog, new_data = test_data) %>%
  exp() %>% 
  bind_cols(test_data) %>%
  rmse(truth = Salary, estimate = .pred)

# PCA model
TM_pca_ols_train_rmse <- predict(TM_pca_ols_fit, new_data = train_data) %>%
  bind_cols(train_data) %>%
  rmse(truth = Salary, estimate = .pred)

TM_pca_ols_test_rmse <- predict(TM_pca_ols_fit, new_data = test_data) %>%
  bind_cols(test_data) %>%
  rmse(truth = Salary, estimate = .pred)

# Mixed model
TM_log_pca_ols_train_rmse <- predict(TM_log_pca_ols_fit, new_data = train_data) %>%
  exp() %>% 
  bind_cols(train_data) %>%
  rmse(truth = Salary, estimate = .pred)

TM_log_pca_ols_test_rmse <- predict(TM_log_pca_ols_fit, new_data = test_data) %>%
  exp() %>% 
  bind_cols(test_data) %>%
  rmse(truth = Salary, estimate = .pred)

# Step 2: Combine the results into a single table
# create a long-format table with the RMSE figures and name them
bind_rows(
  TM3loglog_train_rmse%>% mutate(model = "OLS_loglog", data = "Training error"),
  TM3loglog_test_rmse %>% mutate(model = "OLS_loglog", data = "Test error"),
  TM_pca_ols_train_rmse%>% mutate(model = "OLS_PCA", data = "Training error"),
  TM_pca_ols_test_rmse %>% mutate(model = "OLS_PCA", data = "Test error"),
  TM_log_pca_ols_train_rmse %>% mutate(model = "OLS_log_PCA", data = "Training error"),
  TM_log_pca_ols_test_rmse %>% mutate(model = "OLS_log_PCA", data = "Test error")
) %>% 
  # remove unnecessary information
  select(-.estimator, -.metric) %>% 
  # transform to wide format
  pivot_wider(names_from = data, values_from = .estimate) # Option 1
  #pivot_wider(names_from = model, values_from = .estimate) # Option 2

# Note: since we transformed our predictions back from logged USD to USD,
# we can directly compare the performance of both types of models.


