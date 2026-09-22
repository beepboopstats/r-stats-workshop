
# RECAP -------------------------------------------------------------------


# 0. Load libraries -------------------------------------------------------

library(dplyr)
library(ggplot2)


# 1. Reading files into R -------------------------------------------------

# - CSV

readr::read_csv("data/abcd-synthetic.csv") |> 
  glimpse()

# - RDS (R Data Set)

data <- readRDS("data/data.RDS") |> 
  glimpse()


# 2. Select columns of interest -------------------------------------------

sub <- data |> 
  select(
    # by position
    1:2,
    # by name, simultaneously rename
    age = ab_g_dyn__visit_age,
    site = ab_g_dyn__design_site,
    income = ab_g_dyn__cohort_income__hhold__3lvl,
    sex = ab_g_stc__cohort_sex,
    ethn = ab_g_stc__cohort_ethn,
    # by pattern
    matches("_anthr_"),
    matches("_screen_"),
    matches("_cbcl_")
  ) |> 
  rename(
    height = ph_y_anthr__height_mean,
    weight = ph_y_anthr__weight_mean,
    waist = ph_y_anthr__waist_001
  ) |> 
  as_tibble() |> 
  glimpse()

# 3. Format data ----------------------------------------------------------

# - Check data types 
# - Rename factor levels
# - Look for implausible values (data quality)
# - Handle missingness

levels_dict <- readr::read_csv("data/data_dictionary_levels.csv") |> 
  glimpse()

site_levels <- levels_dict |> 
  filter(
    name == "ab_g_dyn__design_site"
  ) |> 
  select(
    value,
    site_label = label
  )

sub2 <- sub |> 
  glimpse() |> 
  mutate(
    across(
      where(is.numeric),
      ~ round(.x)
    )
  ) |> 
  mutate(
    sex = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
      .default = NA
    ),
    visit = stringr::str_replace(session_id, "ses-", "") |> 
      readr::parse_number() |> 
      factor(),
    site = as.character(site)
  ) |> 
  left_join(
    site_levels |> 
      mutate(
        value = as.character(value)
      ),
    join_by(site == value)
  ) |> 
  glimpse()

sub2 |> 
  count(site, site_label)


# Q1. Height, Weight, and Waist  ------------------------------------------

sub2 |> 
  glimpse() |> 
  # tidyr::drop_na(height) |> 
  mutate(
    sex = factor(
      sex,
      levels = c("Male", "Female")
    ),
    visit = stringr::str_replace(session_id, "ses-", "") |> 
      readr::parse_number() |> 
      factor()
  ) |> 
  glimpse() |> 
  ggplot(
    aes(
      x = visit, 
      # x = session_id, 
      y = waist, 
      fill = sex,
      col = sex
    )
  ) +
  theme_bw(20) +
  # geom_hline(yintercept = 60, colour = "grey60") +
  # geom_vline(xintercept = 12.5, colour = "red") +
  # geom_point()
  # geom_violin(col = "black") +
  geom_boxplot(col = "black") +
  # geom_jitter(alpha = 0.5, size = 0.2) +
  # scale_fill_viridis_d() +
  scale_fill_manual(values = c("orange", "grey60")) +
  scale_color_viridis_d() +
  coord_flip() +
  theme(
    legend.position = "top"
  )

  
sub2 |> 
  mutate(
    sex = factor(
      sex,
      levels = c("Male", "Female")
    )
  ) |> 
  select(
    participant_id,
    visit,
    sex,
    height,
    weight,
    waist
  ) |> 
  tidyr::pivot_longer(
    cols = height:waist,
    names_to = "what",
    values_to = "value"
  ) |> 
  mutate(
    visit = paste0("yr-", visit) |> factor()
  ) |> 
  ggplot(
    aes(
      x = visit, 
      y = value, 
      fill = sex,
      col = sex
    )
  ) +
  geom_boxplot(col = "black") +
  facet_grid(~ what, scales = "free") + 
  coord_flip() +
  theme_bw(20) +
  scale_fill_manual(values = c("orange", "grey60")) +
  scale_color_viridis_d() +
  # labs(
  #   x = "",
  #   y = "",
  #   title = "Title here",
  #   subtitle = "sub-title goes here",
  #   caption = "some additional info here"
  # ) +
  theme(
    legend.position = "top"
  )


# Exercise 1: Make boxplots with the _screen_ variables -------------------

# Time: 15 mins



# Q2. Mean + Error bars ---------------------------------------------------

sub2 |> 
  # summary()
  select(
    participant_id,
    visit,
    age,
    sex,
    screen_wkday = nt_y_stq__screen__wkdy_sum,
    screen_wknd = nt_y_stq__screen__wknd_sum,
    cbcl_int = mh_p_cbcl__synd__int_sum,
    cbcl_ext = mh_p_cbcl__synd__ext_sum
  ) |> 
  # count(age)
  filter(
    age >= 10 & age <= 15,
    !is.na(screen_wkday),
    !is.na(screen_wknd),
    !is.na(cbcl_int),
    !is.na(cbcl_ext)
  ) |> 
  # summary()
  mutate(
    age = factor(age)
  ) |>
  group_by(age, sex) |> 
  reframe(
    n = n(),
    mean_screen_wkday = mean(screen_wkday),
    mean_screen_wknd = mean(screen_wknd),
    sd_wkday = sd(screen_wkday),
    sd_wknd = sd(screen_wknd)
  ) |> 
  mutate(
    mean = mean_screen_wkday,
    sd = sd_wkday,
    se = sd / sqrt(n)
  ) |> 
  ggplot(
    aes(x = age, y = mean, fill = sex, col = sex, group = sex)
  ) +
  # geom_ribbon(
  #   aes(
  #     ymin = mean - se, 
  #     ymax =  mean + se
  #   ),
  #   alpha = 0.5
  # ) +
  # geom_line(
  #   size = 2,
  #   aes(col = sex)
  # ) +
  geom_errorbar(
     aes(
       ymin = mean - se,
       ymax =  mean + se
     ),
     position = position_dodge(width = 0.7),
     width = .3
  ) +
  geom_line(
    position = position_dodge(width = 0.7)
  ) +
  geom_point(
    position = position_dodge(width = 0.7),
    size = 7
  ) +
  theme_bw(20) +
  scale_color_manual(values = c("orange", "grey60")) +
  scale_fill_manual(values = c("orange", "grey60")) +
  theme(
    legend.position = "top"
  )



