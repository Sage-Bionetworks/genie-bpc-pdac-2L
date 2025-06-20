# Create a "table one"

library(purrr)
library(fs)
library(here)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)


cohort <- readr::read_rds(
  here('data', 'cohort_prog_verified.rds')
)
pt <- readr::read_csv(
  here('data-raw', 'PANC', 'patient_level_dataset.csv')
)
ca_ind <- readr::read_csv(
  here('data-raw', 'PANC', 'cancer_level_dataset_index.csv')
)


pt_baseline_sub <- pt %>%
  mutate(
    `Race (primary)` = format_ptlevel_naaccr_race_code_primary(
      naaccr_race_code_primary
    ),
    `Ethnicity` = format_ptlevel_naaccr_ethnicity_code(
      naaccr_ethnicity_code,
    ),
    `Sex at birth` = factor(naaccr_sex_code)
  ) %>%
  select(
    record_id,
    Institution = institution,
    `Race (primary)`,
    `Ethnicity`,
    `Sex at birth`,
    birth_year
  )

ca_ind_baseline_sub <- ca_ind %>%
  mutate(
    stage_mets_dx = format_stage_mets_dx(
      var_stage_dx = stage_dx,
      var_ca_dmets_yn = ca_dmets_yn
    )
  ) %>%
  select(
    record_id,
    `Age at dx (years)` = dob_ca_dx_yrs,
    `Stage at dx` = stage_mets_dx
  )

dft_demo <- left_join(
  select(cohort, record_id),
  pt_baseline_sub,
  by = "record_id"
) %>%
  left_join(
    ca_ind_baseline_sub,
    by = "record_id"
  )

dft_demo %<>%
  rename(
    `Year of birth` = birth_year
  ) %>%
  mutate(
    Ethnicity = fct_drop(Ethnicity),
    `Stage at dx` = fct_drop(`Stage at dx`)
  )


readr::write_rds(
  dft_demo,
  here('data', 'baseline_char.rds')
)
