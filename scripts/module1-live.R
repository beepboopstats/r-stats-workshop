# =====================================================================
# Module 1 - Foundations of R Programming
# Live-coding script | Sep 10, 2026
# =====================================================================
#
# Follow along here rather than retyping from the website. Section
# numbers match the module page:
#   https://beepboopstats.github.io/r-stats-workshop/content/module1.html
#
# Run the line your cursor is on with:
#   Ctrl+Enter   (Windows/Linux)
#   Cmd+Enter    (macOS)
#
# Fill in each "TODO". If you fall behind, the solutions are all on
# the module page - do not worry about catching up live.
# =====================================================================


# ---------------------------------------------------------------------
# 1. Running code, and storing results
# ---------------------------------------------------------------------

2 + 2

# TODO: store your age in years in a name called `age_years`, then
#       print it. Shortcut for the <- arrow is Alt+- / Option+-



# ---------------------------------------------------------------------
# 2. Base R notation you need in order to read code
# ---------------------------------------------------------------------

# Functions take arguments inside round brackets
sqrt(16)
round(3.14159, digits = 2)

# c() combines values into a vector
scores <- c(104, 98, 116, 91)
scores
mean(scores)

# TODO (Exercise 1.1): make a vector `sleep` holding 9.5, 8, 10, 7.5
#       then find its mean and its maximum



# library() switches a package on. Every script starts with these.
library(readr)    # reading data files
library(dplyr)    # transforming data
library(tidyr)    # tidying and reshaping
library(ggplot2)  # plots


# ---------------------------------------------------------------------
# 3. Data types
# ---------------------------------------------------------------------

class(10.5)
class("baseline")
class(TRUE)

# Mixing types coerces everything to the safest common type
mixed <- c(1, 2, "three")
class(mixed)

# TODO: predict what class(c(TRUE, 5)) gives, then run it



# ---------------------------------------------------------------------
# 4. Reading in the data
# ---------------------------------------------------------------------

# TODO: read data/abcd-synthetic.csv into a name called `abcd`
#       (adjust the path if you put the file somewhere else)



# ---------------------------------------------------------------------
# 5. Looking at what you loaded
# ---------------------------------------------------------------------

# TODO: glimpse() the data



# TODO: how many rows and columns? (dim)



# TODO: first 6 rows (head), and then summary() - look at the NA counts



# TODO (Exercise 1.2): 200 participants x 3 visits would be how many
#       rows? Compare with what you actually have. Why the difference?



# ---------------------------------------------------------------------
# 6. The pipe:  |>
# ---------------------------------------------------------------------

# Nested - read inside-out
mean(abcd$cognition_score, na.rm = TRUE)

# Piped - read left to right, as "and then"
abcd$cognition_score |> mean(na.rm = TRUE)

# Shortcut for |> is Ctrl+Shift+M / Cmd+Shift+M


# ---------------------------------------------------------------------
# 7. The five verbs
# ---------------------------------------------------------------------

# filter() - keep rows
abcd |> filter(visit == "baseline")

# TODO: keep only the female participants at baseline



# TODO: select() just subject_id, visit and cognition_score



# TODO: arrange() the data by cognition_score, highest first (desc)



# TODO: mutate() a new column age_years = age_months / 12,
#       then select subject_id, visit, age_months, age_years



# summarise() collapses to one row
abcd |>
  summarise(
    n        = n(),
    mean_cog = mean(cognition_score, na.rm = TRUE)
  )

# TODO: add group_by(visit) before the summarise above.
#       What changes?



# TODO: count() the rows per visit, then per site and sex



# TODO (Exercise 1.3): year_2 visits only - mean screen_time_hrs by sex



# TODO (Exercise 1.4): add sleep_deficit = 9 - sleep_hrs, sort it
#       largest first, show subject_id / visit / sleep_deficit, top 5



# ---------------------------------------------------------------------
# 8. Missing data
# ---------------------------------------------------------------------

mean(abcd$cognition_score)                 # NA - why?
mean(abcd$cognition_score, na.rm = TRUE)   # skip the missing ones

# TODO: count the missing values in cognition_score, total_problems
#       and screen_time_hrs  (hint: sum(is.na(...)) inside summarise)



# TODO: is the missingness even across visits? group_by(visit) and
#       work out the percentage missing per visit



# TODO: drop_na(cognition_score) into a new name `abcd_cog`,
#       then compare nrow() before and after



# ---------------------------------------------------------------------
# 9. Your first plots
# ---------------------------------------------------------------------
# Data goes IN with |>  ... layers are added with +

# TODO: histogram of cognition_score
#       ggplot(aes(x = ...)) + geom_histogram(binwidth = 5)



# TODO: boxplot of total_problems by visit
#       ggplot(aes(x = visit, y = ...)) + geom_boxplot()



# TODO: line chart of mean cognition_score by visit.
#       First group_by + summarise, THEN pipe into ggplot.
#       Remember group = 1 inside aes()



# TODO (Exercise 1.5): boxplot of sleep_hrs by visit,
#       then add colour = sex inside aes()



# ---------------------------------------------------------------------
# 10. Saving your work
# ---------------------------------------------------------------------

# TODO: build a visit_summary (n, mean cognition, mean total_problems
#       per visit) and write_csv() it to output/visit-summary.csv



# TODO: ggsave() your favourite plot into output/
#       ggsave("output/my-plot.png", width = 6, height = 4, dpi = 300)



# ---------------------------------------------------------------------
# Putting it together
# ---------------------------------------------------------------------

# TODO: one pipeline - baseline visits only, drop rows missing
#       cognition_score, add age_years, group by sex, and report
#       n / mean age / mean cognition / mean screen time



# ---------------------------------------------------------------------
# Done. Next: Module 2, Sep 24 2026.
# ---------------------------------------------------------------------
