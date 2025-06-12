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
imaging <- readr::read_csv(
  here('data-raw', 'PANC', 'imaging_level_dataset.csv')
)

find_med_onc_prog(med_onc)

img_prog <- function() {}
