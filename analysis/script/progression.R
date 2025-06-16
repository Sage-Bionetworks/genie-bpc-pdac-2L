IMPUTE_LONGTIUDINAL <- F

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

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

reg <- readr::read_csv(
  here('data-raw', 'PANC', 'regimen_cancer_level_dataset.csv')
)

cli_abort("Need to create the regimen start variable (reference the newer data guides) and apply that to the line of therapy data before splitting into first and second lines.  Then mark the first therapy switch using the second line data, merge in the other stuff, and you can find the people who progressed within 6 weeks to 6 months.  How ghastly this all is...")

lot |> 
  select(

# There are lots of people in ehre who won't be in our final cohort, but that's OK.
first_lines <- lot |>
  filter(line_of_therapy %in% 1) |>
  filter(gem_based | fluoro_based)

second_lines <- lot |>
  filter(record_id %in% first_lines$record_id) |>
  filter(line_of_therapy %in% 2)
