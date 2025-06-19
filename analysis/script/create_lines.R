# This request has a very "unique" line of therapy assignment where gem or 5-fu
#   counts as first line even if happens before met dx.
# What we decided to do was start counting lines at either first met reg or
#   at the first gem.  This may not be robust but it will work for this cohort.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

ca_ind <- readr::read_csv(
  here('data-raw', 'PANC', 'cancer_level_dataset_index.csv')
)
reg <- readr::read_csv(
  here('data-raw', 'PANC', 'regimen_cancer_level_dataset.csv')
)
met_times <- get_dmet_time(ca_ind, annotate_type = F)

line_of_ther <- reg %>%
  filter(ca_seq %in% 0) %>% # current restriction
  mutate(
    gem_based = str_detect(regimen_drugs, 'Gemcitabine'),
    fluoro_based = str_detect(regimen_drugs, 'Fluorouracil')
  )

line_of_ther %<>%
  left_join(
    .,
    met_times,
    by = c('record_id', 'ca_seq'),
    relationship = 'many-to-one'
  ) %>%
  mutate(
    dx_dmet_days = dx_dmet_yrs * 365.25,
    post_met_reg = case_when(
      is.na(dx_dmet_days) ~ F,
      T ~ dx_reg_start_int >= dx_dmet_days - 0.5 # half day tolerance for rounding.
    )
  )

line_of_ther %<>%
  # line flag indicates that we start counting here.
  # line eligible means that the regimen is eligible to be a line, based on our 1L definition.
  mutate(
    line_flag = gem_based | fluoro_based | post_met_reg,
    line_eligible = cumsum(line_flag) >= 1
  )

line_of_ther %<>% filter(line_eligible)
# would expect at least 500 or so people still in since that's the ~number who came in with mets.
# line_of_ther %>% summarize(people_still_in = length(unique(record_id)))

# I'm not going to remove investigational agents or anything here - we will do that later on.

# Duplicate regimen merging is crude because we don't need the end date: we literally just axe the second one from the list.
line_of_ther %<>%
  group_by(record_id) %>%
  arrange(dx_reg_start_int) %>%
  mutate(
    .prev_reg_drugs = lag(regimen_drugs),
    .dup_reg = case_when(
      is.na(.prev_reg_drugs) ~ F, # can't check, should be first row.
      .prev_reg_drugs == regimen_drugs ~ T,
      T ~ F
    )
  ) %>%
  ungroup(.) %>%
  filter(!.dup_reg) %>%
  select(-c(.prev_reg_drugs, .dup_reg))

line_of_ther %<>%
  group_by(record_id) %>%
  arrange(dx_reg_start_int) %>%
  mutate(line_of_therapy = 1:n()) %>%
  ungroup(.)

line_of_ther %<>%
  select(
    record_id,
    regimen_number,
    line_of_therapy,
    regimen_drugs,
    gem_based,
    fluoro_based,
    post_met_reg,
    dx_dmet_days,
    dx_reg_start_int
  )

readr::write_rds(
  line_of_ther,
  here('data', 'drug', 'lot.rds')
)
