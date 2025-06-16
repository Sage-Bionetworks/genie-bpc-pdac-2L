library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

reg <- readr::read_csv(
  here('data-raw', 'PANC', 'regimen_cancer_level_dataset.csv')
)
med_onc <- readr::read_csv(
  here('data-raw', 'PANC', 'med_onc_note_level_dataset.csv')
)
img <- readr::read_csv(
  here('data-raw', 'PANC', 'imaging_level_dataset.csv')
)

med_onc_prog(med_onc)

med_onc_prog(med_onc, impute_longitudinal = T) |>
  count(progression)
med_onc_prog(med_onc, impute_longitudinal = F) |>
  count(progression)

med_onc_prog(med_onc, impute_longitudinal = T) |>
  count(response)
med_onc_prog(med_onc, impute_longitudinal = F) |>
  count(response)
# Seems about right - getting some hits in both cases, and more hits when we're using the longitudinal inferences.
# One more thing:  should get NO complete responses without inferring longitudinally:
med_onc_prog(med_onc, impute_longitudinal = T) |>
  count(comp_resp)
med_onc_prog(med_onc, impute_longitudinal = F) |>
  count(comp_resp)


img_prog(img, impute_longitudinal = T) |>
  count(progression)
img_prog(img, impute_longitudinal = F) |>
  count(progression)

img_prog(img, impute_longitudinal = T) |>
  count(response)
img_prog(img, impute_longitudinal = F) |>
  count(response)
