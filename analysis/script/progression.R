# Note on longitudinal imputation for imaging and med onc:
# I changed this to FALSE for both, because it requires fewer assumptions and less explanation, now that this is situated as a sensitivity analysis.

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
  impute_longitudinal = F
)

img_sum <- img_prog(
  img,
  impute_longitudinal = F
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
  ) %>%
  replace_na(replace = list(eval = F, prog = F, resp = F))


lot <- readr::read_rds(here('data', 'drug', 'lot.rds'))

reg <- readr::read_csv(
  here('data-raw', 'PANC', 'regimen_cancer_level_dataset.csv')
)

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

lot <- left_join(
  lot,
  select(reg, record_id, regimen_number, dob_reg_start_int),
  by = c('record_id', 'regimen_number'),
  # comment:  technically record_id and regimen number are not unique.
  # however, everyone that we're working with has one cancer diagnosis,
  #   so they should be here.  We exploit that for simpler code.
  relationship = 'one-to-one'
)


# There are lots of people in here who won't be in our final cohort, but that's OK.
first_lines <- lot |>
  filter(line_of_therapy %in% 1) |>
  filter(gem_based | fluoro_based)

second_lines <- lot |>
  filter(record_id %in% first_lines$record_id) |>
  filter(line_of_therapy %in% 2)

first_lines <- second_lines |>
  select(record_id, dob_reg2_start_int = dob_reg_start_int) |>
  left_join(
    first_lines,
    y = _,
    by = 'record_id'
  )


first_eval <- filter_times_by_ref(
  dat = filter(status_sum, eval),
  ref_dat = first_lines,
  t_col = 'dob_eval_days',
  t_ref_col = 'dob_reg_start_int',
  lower_int = 1,
  upper_int = 26 * 7
) %>%
  group_by(record_id) %>%
  summarize(first_eval_in_range = min(dob_eval_days), .groups = 'drop')

first_prog <- filter_times_by_ref(
  dat = filter(status_sum, prog),
  ref_dat = first_lines,
  t_col = 'dob_eval_days',
  t_ref_col = 'dob_reg_start_int',
  lower_int = 6 * 7, # in a decision I don't totally endorse yet, 6 week minimum on progressions.
  upper_int = 26 * 7
) %>%
  group_by(record_id) %>%
  summarize(first_prog_in_range = min(dob_eval_days), .groups = 'drop')

first_lines <- left_join(
  first_lines,
  first_eval,
  by = 'record_id'
) %>%
  left_join(
    .,
    first_prog,
    by = 'record_id'
  )

# A couple of conditions here that I think should be true:
if (
  with(
    first_lines,
    any(first_eval_in_range > first_prog_in_range, na.rm = T)
  )
) {
  cli_abort("Something wrong with evals and progressions")
}

if (
  with(
    first_lines,
    any(first_eval_in_range < dob_reg_start_int, na.rm = T)
  )
) {
  cli_abort("Something wrong with evals and line starts")
}
# Ok, moving on if those don't fire.

# Our algorithm, questionable though it may be, is that a progression is triggered by:
# 1. Any med onc or imaging progression 6-26 weeks after starting 1L.
# 2. Any switching of medications (start of 2L) if no imaging or med onc evaluation took place.

prog_flags <- first_lines |>
  mutate(
    prog_in_range = case_when(
      !is.na(first_prog_in_range) ~ T,
      first_eval_in_range <= dob_reg2_start_int ~ F,
      is.na(dob_reg2_start_int) ~ F,
      # cases left: first_eval_in_range NA or greater than 2L start.
      # started a medication within 6m:
      (dob_reg2_start_int - dob_reg_start_int) < 26 * 7 ~ T,
      T ~ F # did not
    )
  )

readr::write_rds(
  prog_flags,
  here('data', 'prog_flags.rds')
)
