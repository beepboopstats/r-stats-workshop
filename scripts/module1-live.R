x <- 1:10
x
sqrt(x)
x^2

# fruits <- apple, carrot, orange

fruits <- c("apple", "carrot", "orange")

paste0("Hello", "World")

paste("Hello", fruits)


# Read Data

# install.packages("tidyr")
# install.packages(c("dplyr", "readr", "ggplot2"))

library("dplyr")
library(readr)
# library(xyz)

rm(list = ls())


?read_csv()

# readr::read_csv
abcd <- read_csv(
  file = "data/abcd-synthetic.csv"
)

x = 2

class(x)
class(abcd)

View(abcd)

abcd2 <- read.csv(file = "data/abcd-synthetic.csv")

class(abcd2)

# Dimensions
dim(abcd)
# rows, cols


# Glimpse

# pkg: dplyr

glimpse(abcd)
abcd

glimpse(abcd2)


# Explore Data

abcd

head(abcd, n = 10)


?head

tail(abcd, n = 2)

summary(abcd)

# abcd
abcd

# SELECT

select(abcd, subject_id, visit)

abcd |> select(subject_id, visit)
abcd %>% select(subject_id, visit)


sqrt(9)
9 |> sqrt()

abcd$subject_id

class(abcd)

abcd$subject_id |> 
  unique()

length(1:9)



abcd$subject_id |> 
  unique() |> 
  length()


abcd



# abcd |> 
#   # select()
#   select the cols: cognition_score and sleep_hours |> 
#   # filter()
#   for baseline |> 
#   # mutate()
#   take the mean


abcd |> 
  select(visit, cognition_score, sleep_hrs) |> 
  filter(
    visit == "baseline"
  ) |> 
  summarize(
    mean_cog = mean(cognition_score, na.rm = TRUE),
    mean_sleep = mean(sleep_hrs, na.rm = TRUE)
  )


abcd |> 
  glimpse() |> 
  group_by(visit) |> 
  summarize(
    mean_cog = mean(cognition_score, na.rm = TRUE),
    mean_sleep = mean(sleep_hrs, na.rm = TRUE)
  )



abcd |> 
  glimpse() |> 
  mutate(
    age_yrs = age_months / 12
  ) |> 
  glimpse()


library(ggplot2)

out <- abcd |> 
  glimpse() |> 
  group_by(visit) |> 
  summarize(
    mean_cog = mean(cognition_score, na.rm = TRUE),
    mean_sleep = mean(sleep_hrs, na.rm = TRUE)
  )

out |> 
  # glimpse()
  ggplot(aes(x = visit, y = mean_cog)) +
  # geom_point() +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 110)) +
  theme_minimal(base_size = 20)

















