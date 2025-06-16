IMPUTE_LONGTIUDINAL <- F

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

med_onc_sum <- med_onc_prog(
  med_onc,
  impute_longitudinal = IMPUTE_LONGTIUDINAL
)

img_sum <- img_prog(
  img,
  impute_longitudinal = IMPUTE_LONGTIUDINAL
)

# interestingly there is almost NO overlap on when these happen.
# probably some delay in reporting imaging?  Hard to say.
status_sum <- combine_med_onc_img_sum(med_onc_sum, img_sum)

# Taking the "OR" version for progression/response.
status_sum %<>%
  mutate(
    eval = img_eval | med_onc_eval,
    prog = img_prog | med_onc_prog,
    resp = img_resp | med_onc_resp
  )

lot <- readr::read_rds(here('data', 'drug', 'lot.rds'))

first_lines <- lot |>
  filter(line_of_therapy %in% 1) |>
  
