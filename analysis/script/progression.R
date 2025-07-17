# This has been reduced substantially in scope to just imaging.

LOWER_PROG <- 1
UPPER_PROG <- Inf

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

img <- readr::read_csv(
  here('data-raw', 'PANC', 'imaging_level_dataset.csv')
)

img_sum <- img_prog(
  img,
  impute_longitudinal = F
)

# interestingly there is almost NO overlap on when these happen.
# probably some delay in reporting imaging?  Hard to say.
status_sum <- combine_med_onc_img_sum(med_onc_sum, img_sum)

# status_sum is a leftover from when med onc was in, now it's
#   just a renaming of the imaging data.
status_sum <- img_sum %>%
  rename(dob_eval_days = image_scan_int) %>%
  mutate(
    eval = evaluated,
    prog = progression,
    resp = response
  ) %>%
  replace_na(replace = list(eval = F, prog = F, resp = F)) %>%
  select(
    cohort,
    record_id,
    dob_eval_days,
    eval,
    prog,
    resp
  )


lot <- readr::read_rds(here('data', 'drug', 'lot.rds'))

# There are lots of people in here who won't be in our final cohort, but that's OK.
first_lines <- lot |>
  filter(line_of_therapy %in% 1) |>
  filter(gem_based | fluoro_based)

# second_lines <- lot |>
#   filter(record_id %in% first_lines$record_id) |>
#   filter(line_of_therapy %in% 2)

first_lines <- second_lines |>
  select(record_id, dob_reg2_start_int = dob_reg_start_int) |>
  left_join(
    first_lines,
    y = _,
    by = 'record_id'
  )

# now add in the index line - which could be either the second or third line.
line_eval <- readr::read_rds(here('data', 'drug', 'line_evaluation.rds'))

# missing a lot of the data we need here sadly - merge back in:
line_eval %<>%
  filter(!is.na(index_line)) %>%
  select(record_id, index_line) %>%
  left_join(
    .,
    select(lot, record_id, line_of_therapy, regimen_number),
    by = c('record_id', index_line = 'line_of_therapy'),
    relationship = 'one-to-one'
  ) %>%
  left_join(
    .,
    select(reg, record_id, regimen_number, dob_reg_start_int),
    by = c('record_id', 'regimen_number')
  )

# merge it into the main dataframe we're working with:
first_lines <- line_eval |>
  select(record_id, dob_reg_index_start_int = dob_reg_start_int) |>
  left_join(
    first_lines,
    y = _,
    by = 'record_id',
    relationship = 'one-to-one'
  )


first_eval <- filter_times_by_ref(
  dat = filter(status_sum, eval),
  ref_dat = first_lines,
  t_col = 'dob_eval_days',
  t_ref_col = 'dob_reg_start_int',
  lower_int = LOWER_PROG,
  upper_int = UPPER_PROG
) %>%
  group_by(record_id) %>%
  summarize(first_eval_in_range = min(dob_eval_days), .groups = 'drop')

first_prog <- filter_times_by_ref(
  dat = filter(status_sum, prog),
  ref_dat = first_lines,
  t_col = 'dob_eval_days',
  t_ref_col = 'dob_reg_start_int',
  lower_int = LOWER_PROG,
  upper_int = UPPER_PROG
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
      first_prog_in_range > dob_reg_index_start_int ~ F,
      !is.na(first_prog_in_range) ~ T,
      first_eval_in_range <= dob_reg2_start_int ~ F,
      is.na(dob_reg2_start_int) ~ F,
      # cases left: first_eval_in_range NA or greater than 2L start.
      # Update: removing this case.
      # Should delete some of the code above, too.
      (dob_reg2_start_int - dob_reg_start_int) < UPPER_PROG ~ F,
      T ~ F # did not progress, did not start new med in range.
    )
  )

readr::write_rds(
  prog_flags,
  here('data', 'prog_flags.rds')
)
