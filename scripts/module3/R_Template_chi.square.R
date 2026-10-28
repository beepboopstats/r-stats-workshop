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
# We load only our two workhorse packages with library(): dplyr and ggplot2.
# Every other function is written with its package attached (e.g. tidyr::drop_na(),
# tidyr::pivot_wider()) -- the same convention as Module 2 -- so you can always see
# which package a function comes from. You still need those packages installed
# (see the install.packages() lines below).
library(ggplot2)      # for visualizations
library(dplyr)        # for data manipulation

# The functions below are commented out; however, if you needed to install a package for the first time, you could un-comment it
# The comment and un-comment shortcut is "CNTRL + Shift + C" for PC and "CMD + Shift + C" for MAC
## You can also just delete the "#"
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr")
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
summarytools::dfSummary(data) |> summarytools::stview()

#-------------------------------------------------------------------------------
# Explore and clean SOME of our data
#-------------------------------------------------------------------------------
# When we set out to do a chi-squared test, we need TWO categorical variables
## A chi-squared test of independence asks: are these two category systems related, or independent?

### For this example we are going to use household income and whether the child feels their neighbourhood is safe

# What we will do live is use the names() function to help us look at all the column names easily
## Here is that code, but I usually don't keep it in my scripts. Instead I just type it straight into the console as needed
### names(data)

# We'll build our analysis data set in ONE pipeline, the same tidyverse style you
# learned in Modules 1 and 2: select() the columns, filter() the rows, mutate() to
# recode, and drop_na() to handle missing data. Read "|>" as "and then".
clean <- data |>
  # select() our variables of interest, renaming the cryptic ones as new_name = old_name
  select(
    participant_id,                                   # we need to know who is who
    session_id,                                       # we need to know when our data was collected (longitudinal)
    family_id = ab_g_stc__design_id__fam,             # gonna need this later for assumption of independence
    income    = ab_g_dyn__cohort_income__hhold__3lvl, # our income variable (renamed)
    safety    = fc_y_nsc__ns_003                      # our neighbourhood-safety variable (renamed)
  ) |>
  # filter() keeps ONLY our baseline visit (see https://docs.abcdstudy.org/latest/)
  filter(session_id == "ses-00A") |>
  # A wrinkle unique to categorical variables: special codes
  ## Both variables use 777 ("decline to answer") and 999 ("don't know")
  ## These are NOT points on the scale, so we filter() them out before testing
  filter(!income %in% c(777, 999),
         !safety %in% c(777, 999)) |>
  # mutate() recodes both variables with case_when() and makes them factors
  mutate(
    income = case_when(
      income == 1 ~ "< 50k",
      income == 2 ~ "50k to 100k",
      income == 3 ~ "> 100k",
      .default = NA
    ) |>
      factor(levels = c("< 50k", "50k to 100k", "> 100k")),
    # The safety item is a 5-point scale. Crossed with 3 income bands that is 15
    # cells, and many will be nearly empty -- so we collapse it to 3 categories
    # (setting levels = c(...) fixes their order for the table and the plot).
    safety = case_when(
      safety %in% c(1, 2) ~ "Disagree",
      safety == 3         ~ "Neutral",
      safety %in% c(4, 5) ~ "Agree",
      .default = NA
    ) |>
      factor(levels = c("Disagree", "Neutral", "Agree"))
  ) |>
  # drop_na() removes anyone missing our variables of interest ("complete-case analysis")
  tidyr::drop_na(family_id, income, safety)

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
# combination of the two variables. We build it with count() (Module 1) and then
# pivot_wider() (Module 2) to lay the counts out as a grid.
counts <- clean_no_sibs |>
  count(income, safety)

counts |>
  tidyr::pivot_wider(names_from = safety, values_from = n)

# Row proportions make the pattern easier to see -- the share of each income band
# in each safety category. We compute them WITHIN each income group with
# group_by() |> mutate(), then pivot_wider() to display.
counts |>
  group_by(income) |>
  mutate(prop = round(n / sum(n), 2)) |>
  ungroup() |>
  select(income, safety, prop) |>
  tidyr::pivot_wider(names_from = safety, values_from = prop)

#-------------------------------------------------------------------------------
# Test Assumptions - Expected Counts
#-------------------------------------------------------------------------------
# Chi-squared's main requirement: every EXPECTED count should be at least 5
## The test stores the expected counts, so fit it once and inspect them
chi_check <- chisq.test(clean_no_sibs$income, clean_no_sibs$safety)
chi_check$expected # all of these should be >= 5

# If any expected count is below 5:
## collapse categories further, OR
## use Fisher's exact test instead:
### fisher.test(clean_no_sibs$income, clean_no_sibs$safety)

#-------------------------------------------------------------------------------
# Run the Chi-Squared Test
#-------------------------------------------------------------------------------
chi_result <- chisq.test(clean_no_sibs$income, clean_no_sibs$safety)
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
n <- nrow(clean_no_sibs)
k <- min(n_distinct(clean_no_sibs$income), n_distinct(clean_no_sibs$safety)) # smaller dimension
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
