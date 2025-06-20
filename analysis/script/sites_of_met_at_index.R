library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

cohort <- readr::read_rds(
  here('data', 'cohort_prog_verified.rds')
)
ca_ind <- readr::read_csv(
  here('data-raw', 'PANC', 'cancer_level_dataset_index.csv')
)
reg <- readr::read_csv(
  here('data-raw', 'PANC', 'regimen_cancer_level_dataset.csv')
)
lot <- readr::read_rds(
  here('data', 'drug', 'lot.rds')
)

# This is redundant with other places in the files, sorry:
reg <- reg |>
  mutate(
    # creating the analogous version of dx_reg_start_int...
    dob_reg_start_int = pmin(
      drugs_startdt_int_1,
      drugs_startdt_int_2,
      drugs_startdt_int_3,
      drugs_startdt_int_4,
      drugs_startdt_int_5,
      na.rm = T
    )
  )

index_line_times <- cohort %>%
  select(record_id, index_line) %>%
  left_join(
    .,
    select(lot, record_id, line_of_therapy, regimen_number),
    by = c('record_id', index_line = 'line_of_therapy')
  ) %>%
  left_join(
    .,
    select(reg, record_id, regimen_number, dob_reg_start_int),
    by = c('record_id', 'regimen_number')
  )

met_tab <- ca_ind %>%
  select(record_id, dob_ca_dx_days, matches("dx_to_dmets_.*_days")) %>%
  left_join(
    select(cohort, record_id),
    .,
    by = 'record_id'
  ) %>%
  pivot_longer(
    cols = -c(record_id, dob_ca_dx_days),
    names_to = 'var',
    values_to = 'dx_to_met_days'
  ) %>%
  mutate(
    var = str_replace_all(var, 'dx_to_dmets_', ""),
    var = str_replace_all(var, '_days$', ""),
    dob_to_met_days = dob_ca_dx_days + dx_to_met_days
  ) %>%
  select(-c(dob_ca_dx_days, dx_to_met_days))

met_tab %<>%
  left_join(
    .,
    select(index_line_times, record_id, dob_reg_start_int),
    by = 'record_id'
  )

met_tab %<>%
  mutate(
    met_at_index = case_when(
      is.na(dob_to_met_days) ~ F,
      # half day tolerance in case of rounding:
      dob_reg_start_int >= dob_to_met_days - 0.5 ~ T,
      T ~ F
    )
  )

met_sum <- met_tab %>%
  select(-c(dob_to_met_days, dob_reg_start_int)) %>%
  pivot_wider(
    names_from = var,
    values_from = met_at_index
  )

readr::write_rds(
  met_sum,
  here('data', 'met_site_sum.rds')
)
