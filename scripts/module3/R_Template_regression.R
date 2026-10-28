#-------------------------------------------------------------------------------
# Start Here and READ ME
#-------------------------------------------------------------------------------
# What this document is for:

## I find creating templates very useful when learning about new analyses
### They are great ways for you to make sure you are not missing anything when you start a new analysis

## This document is a template for running a simple linear regression using the data set Billu created for Module 2

# Some things to remember:

## When you are closing R, you'll be asked if you want to save your environment
### It is good practice to NOT save your environment
### Instead, hit "don't save"
### This is better because it forces you to build your code in a way that if your computer shut off suddenly, you wouldn't lose all your progress

## Disclaimer: I used NaviGator to help me create these templates; however, I checked it over as the "human in the loop"
### Therefore, I take full responsibility for all included content

#-------------------------------------------------------------------------------
# Load Libraries
#-------------------------------------------------------------------------------
# Load in all the necessary libraries and packages
library(ggplot2)        # for visualizations
library(dplyr)          # for data manipulation
library(moments)        # for skew and kurtosis
library(lmtest)         # for Breusch-Pagan test (homoscedasticity)
library(summarytools)   # for a great visualization tool

# The function below is commented out; however, if you needed to install a package for the first time, you could un-comment it
# The comment and un-comment shortcut is "CNTRL + Shift + C" for PC and "CMD + Shift + C" for MAC
## You can also just delete the "#"
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("moments")
# install.packages("lmtest")
# install.packages("summarytools")

#-------------------------------------------------------------------------------
# Set Working Directory and Load Data
#-------------------------------------------------------------------------------
# Set our working directory
## This is just a fancy way of saying the folder we want to work in
setwd("~/ABCD_Workshop_Module3")

# You can simply type in where your file is here OR you can do this:
## Click "Session" in the bar at the top
## Click "Set Working Directory"
## Click "Choose Directory"
## Navigate to your file like you would any other file
## Select and open your file
## In the console, you will see the directory become selected
## Copy and paste that code in the place where I have my setwd() function

# Read in the data set that Billu made previously
## While this data is fake, it is modelled after ABCD data
data <- readRDS("data.RDS") # an RDS file is just a type of R file we can easily load or read into our environment

# Check in the Global Environment section to make sure you have your data loaded
## It should have something like 2295 observations (obs.) of 53 variables

#-------------------------------------------------------------------------------
# Explore ALL of our data
#-------------------------------------------------------------------------------
# Let's look at all of our data
View(data) # This opens your data up as a new tab
## You can also click the word "data" in the Global Environment to do the same thing

# Neat! But overwhelming

# Let's look at it in a clean table format
dfSummary(data) |> stview()

#-------------------------------------------------------------------------------
# Explore and clean SOME of our data
#-------------------------------------------------------------------------------
# When we set out to do a simple linear regression, we typically have one continuous outcome variable
# and one continuous predictor variable
## So we are going to choose our variables
### For this example we are going to use height as our outcome and weight as our predictor

# What we will do live is use the names() function to help us look at all the column names easily
## Here is that code, but I usually don't keep it in my scripts. Instead I just type it straight into the console as needed
### names(data)

# Create a new data frame with our variables of interest
## Using the names() function can help us quickly pick out what we need
clean <- subset(data, select = c(participant_id,            # we need to know who is who
                                 session_id,                # we need to know when our data is collected since we have longitudinal data
                                 ab_g_stc__design_id__fam,  # gonna need this later for assumption of independence
                                 ph_y_anthr__weight_mean,   # our weight variable (our predictor; yuck name we'll fix later)
                                 ph_y_anthr__height_mean    # our height variable (our outcome; yuck name we'll fix later)
                                 ## Important thing to NOT include is a comma after "ph_y_anthr__height_mean" since that is our last variable
))

# Should have a new data set called clean with 2295 obs. of 5 variables
## Can use View(clean) or just click on "clean" in the Global Environment

# We don't want to do any longitudinal analyses, so let's get rid of everything besides our baseline visit
## Baseline = ses-00A under the session_id column
### You can find this nomenclature here: https://docs.abcdstudy.org/latest/

# Remove all timepoints besides baseline
clean <- subset(clean, session_id == "ses-00A")
## In English, this is saying:
### 1) save to the clean data frame
### 2) take a subset of the data from the clean data frame
### 3) look at the session_id column
### 4) only keep the rows that say ses-00A. Drop all the others
## Be careful of the "==" here

# Should have replaced the old clean data set with a new data set also called clean with 500 obs. of 5 variables
## Can use View(clean) or just click on "clean" in the Global Environment

# Change variable names
clean <- clean |>
  select(
    participant_id,
    session_id,
    family_id = ab_g_stc__design_id__fam,
    weight    = ph_y_anthr__weight_mean,
    height    = ph_y_anthr__height_mean
  )

# We now have our variables of interest!


# What to do with missing data?
## This is a tricky thing to handle on someone's best day
## For our instances, we are only going to be using "complete cases"
### This means everyone has to have complete data to be kept
### Anyone missing data from our variables of interest will be removed completely from analyses
#### This is NOT the best way to handle missing data (multiple imputation is generally considered the best)
#### HOWEVER, that is well outside the scope of what we are doing here

# Removing missing data
## Basically, only keep the complete cases you find in the clean data set
clean <- clean[complete.cases(clean), ]

#-------------------------------------------------------------------------------
# Visualize variables of interest
#-------------------------------------------------------------------------------
# Let's look at it in a clean table format
dfSummary(clean) |> stview()

# Let's look at the relationship between weight (predictor) and height (outcome)
ggplot(clean, aes(x = weight, y = height)) +
  geom_point(alpha = 0.5) +          # plot individual data points
  geom_smooth(method = "lm",
              se = TRUE,
              color = "steelblue") + # add a regression line with a confidence band
  labs(title = "Relationship Between Weight and Height",
       x = "Weight",
       y = "Height") +
  theme_minimal()

#-------------------------------------------------------------------------------
# Test Assumptions - Normality of the Outcome Variable
#-------------------------------------------------------------------------------
# Normality (i.e., are our outcome values normally distributed?)
## Shapiro-Wilk test, best for n < 50; for larger samples, lean on skew/kurtosis and Q-Q plots
## p > 0.05 suggests data is approximately normal
shapiro.test(clean$height)
shapiro.test(clean$weight)

# Skew and kurtosis of height
## Skewness:
### Acceptable range: -2 to +2
### |skew| > 2 indicates problematic skew for normality
skewness(clean$height)
skewness(clean$weight)

## Kurtosis (excess kurtosis, where normal = 3 using moments::kurtosis()):
### Acceptable range: -7 to +7 (for excess kurtosis referenced to 0)
### Note: moments::kurtosis() returns kurtosis where 3 = normal, so subtract 3 if you want excess kurtosis
kurtosis(clean$height)
kurtosis(clean$weight)

# Q-Q plot to visually check normality of height
## Basically, you want the dots to fall along the line and not scatter
ggplot(clean, aes(sample = height)) +
  stat_qq() +
  stat_qq_line(color = "steelblue") +
  labs(title = "Q-Q Plot of Height") +
  theme_minimal()

ggplot(clean, aes(sample = weight)) +
  stat_qq() +
  stat_qq_line(color = "steelblue") +
  labs(title = "Q-Q Plot of Weight") +
  theme_minimal()

# Are our residuals normally distributed?
## Note: for regression, it is technically the RESIDUALS that need to be normal, not the raw outcome
## We will check residual normality again after fitting the model below

#-------------------------------------------------------------------------------
# Test Assumptions - Independence of Observations
#-------------------------------------------------------------------------------
# Could one participant's answers be influenced by another participant?
## With ABCD we often see this in terms of siblings in the data set
### So, let's just randomly retain one sibling from our data set

# First thing we need to do is set a seed
## This means when we do something randomly, we do it the same random way every time we run the code
## Otherwise, we would randomly select different participants every time and likely change our outcome
### The number is meaningless and can be anything. It just needs to be the same every time.
### I usually use the year I was born
set.seed(1996)  # for reproducibility; change or remove as needed

clean_no_sibs <- clean |>
  group_by(family_id) |> # group by family
  slice_sample(n = 1) |> # randomly retain one person from that family
  ungroup()               # un-group our data

# Removes 38 siblings

#-------------------------------------------------------------------------------
# Fit the Regression Model
#-------------------------------------------------------------------------------
# Simple linear regression: height (outcome) ~ weight (predictor)
reg_result <- lm(height ~ weight, data = clean_no_sibs)

# View a full summary of the model
summary(reg_result)

# Key outputs to interpret:
#   Estimate (Intercept) = predicted height when weight = 0
#   Estimate (weight)    = for every 1-unit increase in weight, height changes by this amount
#   Std. Error           = uncertainty around each estimate
#   t value / p-value    = if p < 0.05, the predictor significantly explains variance in height
#   R-squared            = proportion of variance in height explained by weight
#   Adjusted R-squared   = R-squared penalized for number of predictors (more useful with multiple predictors)
#   F-statistic / p      = overall model significance

# Extract specific values
coef(reg_result)                    # intercept and slope
summary(reg_result)$r.squared       # R-squared
summary(reg_result)$adj.r.squared   # Adjusted R-squared
confint(reg_result)                 # 95% confidence intervals for the coefficients

#-------------------------------------------------------------------------------
# Test Assumptions - Post-Model Checks
#-------------------------------------------------------------------------------
# After fitting the model, check the remaining regression assumptions using the residuals

# --- 1. Normality of Residuals ---
shapiro.test(residuals(reg_result))  # p > 0.05 suggests residuals are approximately normal

ggplot(data.frame(residuals = residuals(reg_result)), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "steelblue") +
  labs(title = "Q-Q Plot of Residuals") +
  theme_minimal()

# --- 2. Homoscedasticity (constant variance of residuals) ---
## Breusch-Pagan test: p > 0.05 suggests homoscedasticity (equal variance across fitted values)
bptest(reg_result)

## Residuals vs. Fitted plot: you want a flat, horizontal spread with no clear funnel shape
ggplot(data.frame(fitted   = fitted(reg_result),
                  residuals = residuals(reg_result)),
       aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "steelblue") +
  geom_smooth(se = FALSE, color = "tomato") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# --- 3. Linearity ---
## The residuals vs. fitted plot above also helps here
## A curved pattern suggests the relationship is not linear

#-------------------------------------------------------------------------------
# Calculate Effect Size
#-------------------------------------------------------------------------------
# For simple linear regression, R-squared is our primary effect size
## Small: ~0.02 | Medium: ~0.13 | Large: ~0.26 (Cohen, 1988)
r_squared <- summary(reg_result)$r.squared
cat("R-squared:", round(r_squared, 3), "\n")

# We can also report the standardized regression coefficient (beta)
## This puts the slope on a standard deviation scale so it is easier to interpret
## It tells us: for every 1 SD increase in weight, height changes by beta SDs
beta <- coef(reg_result)["weight"] * (sd(clean_no_sibs$weight, na.rm = TRUE) /
                                        sd(clean_no_sibs$height, na.rm = TRUE))
cat("Standardized beta:", round(beta, 3), "\n")

#-------------------------------------------------------------------------------
# Visualize Results
#-------------------------------------------------------------------------------
ggplot(clean_no_sibs, aes(x = weight, y = height)) +
  geom_point(alpha = 0.4) +                             # individual data points
  geom_smooth(method = "lm",
              se = TRUE,
              color = "steelblue",
              fill = "lightblue") +                     # regression line with confidence band
  labs(
    title = "Simple Linear Regression: Height Predicted by Weight",
    x = "Weight",
    y = "Height"
  ) +
  theme_minimal()
