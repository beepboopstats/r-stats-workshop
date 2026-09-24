library(dplyr)
library(readr)

read_csv("data/abcd-synthetic.csv")


data <- readRDS("data/data.RDS") |> 
  as_tibble()


data |> glimpse()

dict <- read_csv("data/data_dictionary.csv")

dict


# Subset the data ---------------------------------------------------------


# pipes:              cmd + shft + m
# section breaks:     cmd + shft + r
# new r script:       cmd + shift + n
# commenting / uncommenting: cmd + shft + c


data |> 
  glimpse() |> 
  select(
    matches("ab_g_dyn")
  ) |> 
  glimpse()

sub <- data |>
  glimpse() |> 
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


# Height, Weight, Wasit ---------------------------------------------------


sub |> 
  mutate(
    sex2 = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
      .default = NA
    ),
    .after = sex
  ) |> 
  count(sex, sex2)

levels_dict <- read_csv("data/data_dictionary_levels.csv") |> 
  glimpse()


foo <- sub |> 
  head(10) |> 
  select(
    1:2, 
    sex
  )


bar <- levels_dict|>
# dict_levels |>
  filter(
    name == "ab_g_stc__cohort_sex"
  ) |> 
  select(
    value,
    label
  ) |> 
  mutate(
    value = as.factor(value)
  )

foo |> 
  left_join(
    bar,
    join_by(sex == value)
  )



# Relabeling --------------------------------------------------------------

sub |> 
  glimpse() |> 
  mutate(
    # option 1
    # height = round(height)
    
    # option 2
    # across(
    #   # names of columns,
    #   # do something to each column
    #   c("height", "weight"),
    #   ~ round(.x)
    # )
    
    # option 3
    across(
      where(is.numeric),
      ~ round(.x)
    )
  ) |> 
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



# Waist -------------------------------------------------------------------

library(ggplot2)

sub2 |> 
  glimpse()



sub2 |> 
  filter(
    age >= 10 & age <= 15
  ) |> 
  glimpse() |> 
  # any modifications |> 
  ggplot(
    # aes(x = age, y = height)
    aes(x = session_id, y = height, group = interaction(session_id, sex))
  ) +
  # geom_point()
  geom_jitter(
    alpha = 0.2, size = 0.5,
    col = "black"
  ) +
  geom_boxplot(
    aes(fill = sex)
  ) +
  theme_bw(24) +
  scale_fill_manual(
    values = c("orange", "grey")
  ) +
  coord_flip()


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
      y = visit,
      # x = session_id,
      x = mh_p_cbcl__synd__ext_sum,
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







