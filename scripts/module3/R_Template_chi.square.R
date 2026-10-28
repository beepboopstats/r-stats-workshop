#-------------------------------------------------------------------------------
# Start Here and READ ME
#-------------------------------------------------------------------------------
# What this document is for:

## I find creating templates very useful when learning about new analyses
### They are great ways for you to make sure you are not missing anything when you start a new analysis

## This document is a template for running a chi-squared test of independence using the data set Billu created for Module 2


# Some things to remember:

## When you are closing R, you'll be asked if you want to save your environment
### It is good practice to NOT save your environment
### Instead, hit "don't save"
### This is better because it forces you to build your code in a way that if your computer shut off suddenly, you wouldn't lose all your progress

## Disclaimer: I used Claude to help me create these templates; however, I checked it over as the "human in the loop"
### Therefore, I take full responsibility for all included content

#-------------------------------------------------------------------------------
# Load Libraries
#-------------------------------------------------------------------------------
# Load in all the necessary libraries and packages
library(ggplot2)      # for visualizations
library(dplyr)        # for data manipulation
library(summarytools) # for a great visualization tool

# The functions below are commented out; however, if you needed to install a package for the first time, you could un-comment it
# The comment and un-comment shortcut is "CNTRL + Shift + C" for PC and "CMD + Shift + C" for MAC
## You can also just delete the "#"
# install.packages("ggplot2")
# install.packages("dplyr")
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
# When we set out to do a chi-squared test, we need TWO categorical variables
## A chi-squared test of independence asks: are these two category systems related, or independent?

### For this example we are going to use household income and whether the child feels their neighbourhood is safe

# What we will do live is use the names() function to help us look at all the column names easily
## Here is that code, but I usually don't keep it in my scripts. Instead I just type it straight into the console as needed
### names(data)

# Create a new data frame with our variables of interest
clean <- subset(data, select = c(participant_id,                        # we need to know who is who
                                 session_id,                            # we need to know when our data is collected since we have longitudinal data
                                 ab_g_stc__design_id__fam,              # gonna need this later for assumption of independence
                                 ab_g_dyn__cohort_income__hhold__3lvl,  # our income variable (yuck name we'll fix later)
                                 fc_y_nsc__ns_003                       # our neighbourhood-safety variable (yuck name we'll fix later)
                                 ## Important thing to NOT include is a comma after "fc_y_nsc__ns_003" since that is our last variable
))

# Remove all timepoints besides baseline
## Baseline = ses-00A under the session_id column
### You can find this nomenclature here: https://docs.abcdstudy.org/latest/
clean <- subset(clean, session_id == "ses-00A")

# A wrinkle unique to categorical variables: special codes
## Both variables use 777 ("decline to answer") and 999 ("don't know")
### These are NOT points on the scale, so we drop them before testing
clean <- subset(clean, !(ab_g_dyn__cohort_income__hhold__3lvl %in% c(777, 999)) &
                       !(fc_y_nsc__ns_003 %in% c(777, 999)))

# Change variable names and recode
clean <- clean |>
  select(
    participant_id,
    session_id,
    family_id = ab_g_stc__design_id__fam,
    income    = ab_g_dyn__cohort_income__hhold__3lvl,
    safety    = fc_y_nsc__ns_003
  ) |>
  mutate(
    income = factor(income,
                    levels = c(1, 2, 3),
                    labels = c("< 50k", "50k to 100k", "> 100k")),
    # The safety item is a 5-point scale. Crossed with 3 income bands that is 15
    # cells, and some will be nearly empty. We collapse it to 3 so every cell has
    # enough data for the test to be valid.
    safety = case_when(
      safety %in% c(1, 2) ~ "Disagree",
      safety == 3         ~ "Neutral",
      safety %in% c(4, 5) ~ "Agree",
      .default = NA
    ),
    safety = factor(safety, levels = c("Disagree", "Neutral", "Agree"))
  )

# Removing missing data (keep only complete cases)
clean <- clean[complete.cases(clean), ]

#-------------------------------------------------------------------------------
# Test Assumptions - Independence of Observations
#-------------------------------------------------------------------------------
# Could one participant's answers be influenced by another participant?
## With ABCD we often see this in terms of siblings in the data set
### So, let's just randomly retain one sibling from our data set

# First thing we need to do is set a seed
## This means when we do something randomly, we do it the same random way every time we run the code
### I usually use the year I was born
set.seed(1996)  # for reproducibility; change or remove as needed

clean_no_sibs <- clean |>
  group_by(family_id) |> # group by family
  slice_sample(n = 1) |> # randomly retain one person from that family
  ungroup()              # un-group our data

#-------------------------------------------------------------------------------
# Build the Contingency Table
#-------------------------------------------------------------------------------
# A chi-squared test runs on a cross-tabulation -- a grid of counts, one cell per
# combination of the two variables. This table IS the data for the test.
tab <- table(clean_no_sibs$income, clean_no_sibs$safety)
tab

# Row proportions make the pattern easier to see
## margin = 1 gives proportions WITHIN each income row
round(prop.table(tab, margin = 1), 2)

#-------------------------------------------------------------------------------
# Test Assumptions - Expected Counts
#-------------------------------------------------------------------------------
# Chi-squared's main requirement: every EXPECTED count should be at least 5
## The test stores the expected counts, so fit it once and inspect them
chi_check <- chisq.test(tab)
chi_check$expected # all of these should be >= 5

# If any expected count is below 5:
## collapse categories further, OR
## use Fisher's exact test instead:
### fisher.test(tab)

#-------------------------------------------------------------------------------
# Run the Chi-Squared Test
#-------------------------------------------------------------------------------
chi_result <- chisq.test(tab)
print(chi_result)

# Key outputs to interpret:
#   X-squared = the test statistic
#   df        = degrees of freedom, (rows - 1) * (columns - 1)
#   p-value   = if < 0.05, reject the null that the two variables are independent

#-------------------------------------------------------------------------------
# Calculate Effect Size
#-------------------------------------------------------------------------------
# Cramer's V rescales chi-squared to run from 0 (no association) to 1 (perfect)
## Rough guide (smaller dimension = 3): Small ~0.1 | Medium ~0.3 | Large ~0.5
n <- sum(tab)
k <- min(nrow(tab), ncol(tab)) # the smaller dimension of the table
cramers_v <- sqrt(as.numeric(chi_result$statistic) / (n * (k - 1)))
cat("Cramer's V:", round(cramers_v, 3), "\n")

#-------------------------------------------------------------------------------
# Visualize Results
#-------------------------------------------------------------------------------
plot_df <- clean_no_sibs |>
  count(income, safety) |>
  group_by(income) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

ggplot(plot_df, aes(x = income, y = prop, fill = safety)) +
  geom_col(position = "dodge", alpha = 0.85) + # grouped bars
  labs(
    title = "Perceived Neighbourhood Safety by Household Income",
    x = "Household Income",
    y = "Proportion within income band",
    fill = "Feels safe?"
  ) +
  theme_minimal()
