#-------------------------------------------------------------------------------
# Start Here and READ ME
#-------------------------------------------------------------------------------

# What this document is for:

## I find creating templates very useful when learning about new analyses
### They are great ways for you to make sure you are not missing anything when you start a new analysis

## This document is a template for running a t-test using the data set Billu created for Module 2


# Some things to remember:

## When you are closing R, you'll be asked if you want to save your environment
### It is good practice to NOT save your environment
### Instead, hit "don't save"
### This is better because it forces you to build your code in a way that if your computer shut off suddenly, you wouldn't lose all your progress

## Disclaimer: I used Claude-sonnet-4.6 to help me create these templates; however, I checked it over as the "human in the loop"
### Therefore, I take full responsibility for all included content

#-------------------------------------------------------------------------------
# Load Libraries
#-------------------------------------------------------------------------------

# Load in all the necessary libraries and packages
library(ggplot2) # for visualizations
library(dplyr)   # for data manipulation
library(car)     # for Leven's test which tests for equal variances
library(moments) # for skew and kurtosis
library(summarytools)   # for a great visualization tool

# The function below is commented out; however, if you needed to install a package for the first time, you could un-comment it
# The comment and un-comment shortcut is "CNTRl + Shift + C" for PC and "CMD + Shift + C" for MAC
## You can also just delete the "#"
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("car")
# install.packages("moments")
# install.packages("summarytools")

#-------------------------------------------------------------------------------
# Set Working Director and Load Data
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
## Copy and past that code in the place where I have my setwd() function


# Read in the data set that Billu made previously
## While this data is fake, it is modelled after ABCD data
data <- readRDS("data.rds") # an RDS file is just a type of R file we can easily load or read into our environment


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
dfSummary(data) %>% stview()

#-------------------------------------------------------------------------------
# Explore and clean SOME of our data
#-------------------------------------------------------------------------------
# When we set out to do a t-test, we typically have one continuous variable and one categorical variable
## So we are going to choose our variables

### For this example we are going to do height and sex at birth

# What we will do live is use the names() function to help us look at all the column names easily
## Here is that code, but I usually don't keep it in my scripts. Instead I just type it straight into the console as needed
### names(data)

# Create a new data frame with our variables of interest
## Using the names() function can help us quickly pick out what we need
clean <- subset(data, select = c(participant_id,           # we need to know who is who
                                 session_id,               # we need to know when our data is collected since we have longitudinal data
                                 ab_g_stc__design_id__fam, # gonna need this later for assumption of independence
                                 ab_g_stc__cohort_sex,     # our sex variable (yuck name we'll fix later)
                                 ph_y_anthr__height_mean   # our height variable (yuck name we'll fix later)
                                 ## Important thing to NOT include is a comma after "ph_y_anthr__height_mean" since that is our last variable
                                 ))
# Should have a new data set called clean with 2295 obs. of 4 variables
## Can use View(clean) or just click on "clean" in the Global Environment

# We don't want to do any longitudinal analyses, so let's get ride of everything besides our baseline visit
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

# Should have replaced the old clean data set with a new data set also called clean with 500 obs. of 4 variables
## Can use View(clean) or just click on "clean" in the Global Environment

# Change variable names
clean <- clean |>
  select(
    # by name
    participant_id,
    session_id,
    family_id = ab_g_stc__design_id__fam,
    sex = ab_g_stc__cohort_sex,
    height = ph_y_anthr__height_mean
  ) |>
  mutate(
    sex = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
      .default = NA
    )
  )

# Better to turn our grouping variable into a factor
clean$sex <- as.factor(clean$sex)

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
dfSummary(clean) %>% stview()


# Let's look at our frequency of height by sex
ggplot(clean, aes(x = height, fill = sex)) + # height on the x-axis, group our height values by sex
  geom_histogram(binwidth = 0.5, alpha = 0.6, position = "identity") + # just some stuff to make the plot work
  facet_wrap(~sex) + # put the two graphs side by side
  labs(title = "Distribution of Height by Sex") + # always good to have a title
  theme_minimal() # keep the graphs free of clutter


#-------------------------------------------------------------------------------
# Test Assumptions- Normality
#-------------------------------------------------------------------------------
# Normality (i.e., are our values normally distributed)

## Shapiro-Wilk test, best for n < 50
## p > 0.05 suggests data is approximately normal
by(clean$height, clean$sex, shapiro.test) # this "by" function is super handy to look at our continuous variable BY our group


# Skew and kurtosis
## Skewness:
### Acceptable range: -2 to +2
### |skew| > 2 indicates problematic skew for normality
by(clean$height, clean$sex, skewness)
## Kurtosis (excess kurtosis, where normal = 0):
### Acceptable range: -7 to +7
### |excess kurtosis| > 7 indicates problematic kurtosis for normality
### Note: moments::kurtosis() returns EXCESS kurtosis (normal distribution = 3), so 3 is the center of the distribution
by(clean$height, clean$sex, kurtosis)


# Q-Q plot can help you also check normality
## Basically, you want the dots to fall along the line and not scatter
ggplot(clean, aes(sample = height, color = sex)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot by Sex") +
  theme_minimal()


# Are we normally distributed?


#-------------------------------------------------------------------------------
# Test Assumptions- Equality of variances (Levene's Test)
#-------------------------------------------------------------------------------
# Basically is the spread of variance the same across our groups?
## p > 0.05 suggests variances are approximately equal
## This determines whether to use var.equal = TRUE or FALSE below
leveneTest(height ~ sex, data = clean)


# Are our variances equally distributed?


#-------------------------------------------------------------------------------
# Test Assumptions- Independence of Observations
#-------------------------------------------------------------------------------
# Could one participant's answers be influenced by another participant?
## With ABCD we often see this in terms of siblings in the data set
### So, let's just randomly retain a sibling from our data set

# First thing we need to do is set a seed
## This means when we do something randomly, we do it the same random way every time we run the code
## Otherwise, we would randomly different participants every time and likely change our outcome
### The number is meaningless and can be anything. It just needs to be the same every time.
### I usually use the year I was born
set.seed(1996)  # for reproducibility; change or remove as needed

clean_no_sibs <- clean |>
  group_by(family_id) |> # group by family
  slice_sample(n = 1) |> # randomly retain one person from that family
  ungroup() # un-group our data
# Removes 38 siblings


#-------------------------------------------------------------------------------
# Choose and Run a T-Test
#-------------------------------------------------------------------------------
# Choose ONE of the following based on your design:

## Independent samples t-test (two separate groups)
### If Levene's test p > 0.05 (equal variances assumed):
t_result <- t.test(height ~ sex, data = clean_no_sibs, var.equal = TRUE)

### If Levene's test p < 0.05 (use Welch's correction):
#### t_result <- t.test(height ~ sex, data = clean_no_sibs, var.equal = FALSE)

## One-sample t-test (compare to a known value)
### t_result <- t.test(clean_no_sibs$height, mu = 0)

## Paired t-test (same subjects, two conditions)
### t_result <- t.test(clean_no_sibs$time1, clean_no_sibs$time2, paired = TRUE)

# View our t-test results
print(t_result)

# Key outputs to interpret:
#   t       = t-statistic
#   df      = degrees of freedom
#   p-value = if < 0.05, reject null hypothesis of equal means
#   95% CI  = confidence interval for the difference in means

# Extract specific values
t_result$statistic   # t-statistic
t_result$p.value     # p-value
t_result$conf.int    # 95% confidence interval


#-------------------------------------------------------------------------------
# Calculate Effect Size
#-------------------------------------------------------------------------------
# Small: ~0.2 | Medium: ~0.5 | Large: ~0.8

group_means <- tapply(clean_no_sibs$height, clean_no_sibs$sex, mean)   # Get the mean by group
group_sds   <- tapply(clean_no_sibs$height, clean_no_sibs$sex, sd)     # Get the standard deviation by group
group_ns    <- tapply(clean_no_sibs$height, clean_no_sibs$sex, length) # Get the number of people by group

# Pool the standard distributions
pooled_sd <- sqrt(((group_ns[1] - 1) * group_sds[1]^2 +
                     (group_ns[2] - 1) * group_sds[2]^2) /
                    (sum(group_ns) - 2))
# Calculate Cohen's D
cohens_d <- abs(diff(group_means)) / pooled_sd

# Report Cohen's d
## This function basically says "one the same line in the console
### print the words "Cohen's d:", then print the number we just calculated but round to the third decimal, then end the line"
cat("Cohen's d:", round(cohens_d, 3), "\n")


#-------------------------------------------------------------------------------
# Visualize Results
#-------------------------------------------------------------------------------
ggplot(clean_no_sibs, aes(x = sex, y = height, fill = sex)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) + # Create a box plot
  geom_jitter(width = 0.1, alpha = 0.4) + # move the data points around so they aren't all on the same line
  labs(
    title = "Comparison of Baseline Height Between Sexes",
    x = "Sex",
    y = "Height"
  ) + # Label everything so it's pretty
  theme_minimal() + # get rid of any details we don't need
  theme(legend.position = "none") # get rid of the legend because it says the same thing as the x-axis


