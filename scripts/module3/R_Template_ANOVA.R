#-------------------------------------------------------------------------------
# Start Here and READ ME
#-------------------------------------------------------------------------------
# What this document is for:
## I find creating templates very useful when learning about new analyses
### They are great ways for you to make sure you are not missing anything when you start a new analysis

## This document is a template for running a one-way ANOVA using the data set Billu created for Module 2


# Some things to remember:
## When you are closing R, you'll be asked if you want to save your environment
### It is good practice to NOT save your environment
### Instead, hit "don't save"
### This is better because it forces you to build your code in a way that if your computer shut off suddenly, you wouldn't lose all your progress

## Disclaimer: I used Claude-sonnet-4.6 and NaviGator to help me create these templates; however, I checked it over as the "human in the loop"
### Therefore, I take full responsibility for all included content

#-------------------------------------------------------------------------------
# Load Libraries
#-------------------------------------------------------------------------------
# Load in all the necessary libraries and packages
# We load only our two workhorse packages with library(): dplyr and ggplot2.
# Every other function is written with its package attached (e.g. tidyr::drop_na(),
# car::leveneTest(), moments::skewness()) -- the same convention as Module 2 -- so
# you can always see which package a function comes from. You still need those
# packages installed (see the install.packages() lines below).
library(ggplot2)      # for visualizations
library(dplyr)        # for data manipulation

# The functions below are commented out; however, if you needed to install a package for the first time, you could un-comment it
# The comment and un-comment shortcut is "CNTRL + Shift + C" for PC and "CMD + Shift + C" for MAC
## You can also just delete the "#"
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("car")
# install.packages("moments")
# install.packages("summarytools")

#-------------------------------------------------------------------------------
# Set Working Directory and Load Data
#-------------------------------------------------------------------------------
# Set our working directory
## This is just a fancy way of saying the folder we want to work in
setwd("~/ABCD_Workshop_Module3")
# You can simply type in your where your file is here OR you can do this:
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
# When we set out to do a one-way ANOVA, we typically have one continuous variable and one categorical variable
## ANOVA extends the t-test to situations where we have MORE than two groups (here we have 5)
### For this example we are going to use height and cohort education group (ab_g_dyn__cohort_edu__cgs)

# What we will do live is use the names() function to help us look at all the column names easily
## Here is that code, but I usually don't keep it in my scripts. Instead I just type it straight into the console as needed
### names(data)

# We'll build our analysis data set in ONE pipeline, the same tidyverse style you
# learned in Modules 1 and 2: select() the columns, filter() the rows, mutate() to
# recode, and drop_na() to handle missing data. Read "|>" as "and then".
clean <- data |>
  # select() our variables of interest, renaming the cryptic ones as new_name = old_name
  select(
    participant_id,                           # we need to know who is who
    session_id,                               # we need to know when our data was collected (longitudinal)
    family_id    = ab_g_stc__design_id__fam,  # gonna need this later for assumption of independence
    parental_edu = ab_g_dyn__cohort_edu__cgs, # our 5-level education grouping variable
    height       = ph_y_anthr__height_mean    # our height variable (renamed from its yuck name)
  ) |>
  # filter() keeps ONLY our baseline visit -- we don't want longitudinal analyses here
  ## Baseline = ses-00A under the session_id column (see https://docs.abcdstudy.org/latest/)
  filter(session_id == "ses-00A") |>
  # mutate() recodes education with case_when(), then makes it a factor
  ## Setting levels = c(...) fixes the ORDER, which matters for plots and post-hoc tests
  mutate(
    parental_edu = case_when(
      parental_edu == 1 ~ "less_than_HS",
      parental_edu == 2 ~ "HS_diploma",
      parental_edu == 3 ~ "some_college",
      parental_edu == 4 ~ "bachelor_degree",
      parental_edu == 5 ~ "graduate_degree",
      .default = NA
    ) |>
      factor(levels = c("less_than_HS", "HS_diploma", "some_college",
                        "bachelor_degree", "graduate_degree"))
  ) |>
  # drop_na() removes anyone missing our variables of interest ("complete-case analysis")
  ## This is NOT the best way to handle missing data (multiple imputation is better),
  ## but that is well outside the scope of what we are doing here
  tidyr::drop_na(family_id, parental_edu, height)

# We now have our variables of interest!
## Should be about 500 participants at baseline, minus anyone missing height or education
## Can use View(clean) or just click on "clean" in the Global Environment


#-------------------------------------------------------------------------------
# Visualize variables of interest
#-------------------------------------------------------------------------------
# Let's look at it in a clean table format
summarytools::dfSummary(clean) |> summarytools::stview()


# Let's look at our frequency of height by education cohort
ggplot(clean, aes(x = height, fill = parental_edu)) + # height on the x-axis, group our height values by education cohort
  geom_histogram(binwidth = 0.5, alpha = 0.6, position = "identity") + # just some stuff to make the plot work
  facet_wrap(~parental_edu) + # put the five graphs side by side
  labs(title = "Distribution of Height by Education Cohort Group") + # always good to have a title
  theme_minimal() # keep the graphs free of clutter


#-------------------------------------------------------------------------------
# Test Assumptions - Normality
#-------------------------------------------------------------------------------
# Normality (i.e., are our values normally distributed within each group?)
## We check all three measures at once, BY group, with group_by() |> summarise() --
## the same Module 2 pattern you used for descriptive tables.
## Note: ANOVA is fairly robust to violations of normality when group sizes are large.
## - Shapiro-Wilk test: p > 0.05 suggests approximately normal (best for n < 50)
## - Skewness: acceptable range -2 to +2; |skew| > 2 is problematic
## - Kurtosis: moments::kurtosis() centres a normal at 3; |excess kurtosis| > 7 is problematic
clean |>
  group_by(parental_edu) |>
  summarise(
    shapiro_p = shapiro.test(height)$p.value,
    skew      = moments::skewness(height),
    kurtosis  = moments::kurtosis(height)
  )


# Q-Q plot can help you also check normality
## Basically, you want the dots to fall along the line and not scatter
ggplot(clean, aes(sample = height, color = parental_edu)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~parental_edu) + # check each group separately since we now have 5 groups
  labs(title = "Q-Q Plot by Education Cohort Group") +
  theme_minimal()



# Are we normally distributed within each group?


#-------------------------------------------------------------------------------
# Test Assumptions - Equality of Variances (Levene's Test)
#-------------------------------------------------------------------------------
# Basically, is the spread of variance the same across ALL five of our groups?
## p > 0.05 suggests variances are approximately equal
## This determines whether to use a standard ANOVA or a Welch's ANOVA below
car::leveneTest(height ~ parental_edu, data = clean)


# Are our variances equally distributed across all five groups?


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
  ungroup() # un-group our data
# Removes siblings from the data set


#-------------------------------------------------------------------------------
# Choose and Run an ANOVA
#-------------------------------------------------------------------------------
# Choose ONE of the following based on your Levene's test result:

## Standard one-way ANOVA (assumes equal variances across groups)
### Use when Levene's test p > 0.05
anova_result <- aov(height ~ parental_edu, data = clean_no_sibs)
summary(anova_result)
## Welch's one-way ANOVA (does NOT assume equal variances -- more conservative)
### Use when Levene's test p < 0.05
#### anova_result_welch <- oneway.test(height ~ parental_edu, data = clean_no_sibs, var.equal = FALSE)
#### print(anova_result_welch)

# Key outputs to interpret from summary(anova_result):
#   Df          = degrees of freedom
#   F value     = F-statistic (ratio of between-group variance to within-group variance)
#   Pr(>F)      = p-value -- if < 0.05, at least one group mean is significantly different from the others
# IMPORTANT: A significant ANOVA only tells you THAT groups differ, not WHICH groups differ

# For that, we need post-hoc tests (see below)

#-------------------------------------------------------------------------------
# Post-Hoc Tests (run only if ANOVA is significant)
#-------------------------------------------------------------------------------
# Since we have 5 groups, a significant ANOVA result just tells us *something* is different
## Post-hoc tests do pairwise comparisons between all groups while correcting for multiple comparisons
## Tukey's HSD (Honest Significant Difference) is the most common choice
### It controls the family-wise error rate and works well when group sizes are roughly equal
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# Visualize the Tukey comparisons
plot(tukey_result, las = 1) # las = 1 makes the y-axis labels horizontal so they're readable

# Key outputs to interpret from TukeyHSD:
#   diff    = difference in means between the two groups being compared
#   lwr/upr = lower and upper bounds of the 95% confidence interval for that difference
#   p adj   = p-value adjusted for multiple comparisons -- if < 0.05, those two groups are significantly different

#-------------------------------------------------------------------------------
# Calculate Effect Size
#-------------------------------------------------------------------------------
# Eta-squared (η²): proportion of total variance explained by group membership
## Small: ~0.01 | Medium: ~0.06 | Large: ~0.14
anova_summary <- summary(anova_result)
ss_between <- anova_summary[[1]]["parental_edu", "Sum Sq"] # variance explained by our grouping variable
ss_total   <- sum(anova_summary[[1]][, "Sum Sq"])        # total variance in the model
eta_squared <- ss_between / ss_total
cat("Eta-squared:", round(eta_squared, 3), "\n")


#-------------------------------------------------------------------------------
# Visualize Results
#-------------------------------------------------------------------------------
ggplot(clean_no_sibs, aes(x = parental_edu, y = height, fill = parental_edu)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) + # create a box plot
  geom_jitter(width = 0.1, alpha = 0.4) + # move the data points around so they aren't all on the same line
  labs(
    title = "Comparison of Baseline Height Across Parental Education Groups",
    x = "Parental Education",
    y = "Height"
  ) + # label everything so it's pretty
  theme_minimal() + # get rid of any details we don't need
  theme(legend.position = "none") # get rid of the legend because it says the same thing as the x-axis


