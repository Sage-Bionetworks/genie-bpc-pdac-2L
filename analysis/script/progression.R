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

img_sum <- img_process(
  img,
  return_minimal = T
)

# status_sum is a leftover from when med onc was in, now it's
#   just a renaming of the imaging data.
status_sum <- img_sum %>%
  mutate(
    eval = evaluated,
    prog = status_change %in% "worsening",
    resp = status_change %in% "improving" # don't really need this now but that's OK.
  ) %>%
  replace_na(replace = list(eval = F, prog = F, resp = F)) %>%
  select(
    cohort,
    record_id,
    dob_event_days,
    eval,
    prog,
    resp
  )


lot <- readr::read_rds(here('data', 'drug', 'lot.rds'))

# There are lots of people in here who won't be in our final cohort, but that's OK.
first_lines <- lot |>
  filter(line_of_therapy %in% 1) |>
  filter(gem_based | fluoro_based)

# now add in the index line - which could be either the second or third line.
line_eval <- readr::read_rds(here('data', 'drug', 'line_evaluation.rds'))

# missing a lot of the data we need here sadly - merge back in:
line_eval %<>%
  filter(!is.na(index_line)) %>%
  select(record_id, index_line) %>%
  left_join(
    .,
    select(lot, record_id, line_of_therapy, regimen_number, dob_reg_start_int),
    by = c('record_id', index_line = 'line_of_therapy'),
    relationship = 'one-to-one'
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

first_prog <- filter_times_by_ref(
  dat = filter(status_sum, prog),
  ref_dat = first_lines,
  t_col = 'dob_event_days',
  t_ref_col = 'dob_reg_start_int',
  lower_int = LOWER_PROG,
  upper_int = UPPER_PROG
) %>%
  group_by(record_id) %>%
  summarize(first_prog_in_range = min(dob_event_days), .groups = 'drop')

first_lines <- left_join(
  first_lines,
  first_prog,
  by = 'record_id'
)

# A sanity check here:
if (
  with(
    first_lines,
    any(first_prog_in_range < dob_reg_start_int, na.rm = T)
  )
) {
  cli_abort("Something wrong with first_prog_in_range and line starts")
}

# Our algorithm, is that a progression is triggered by any imaging progression after starting 1L (but before the index line).
prog_flags <- first_lines |>
  mutate(
    prog_in_range = case_when(
      is.na(first_prog_in_range) ~ F, # never progressed
      first_prog_in_range > dob_reg_index_start_int ~ F,
      T ~ T
    )
  )

readr::write_rds(
  prog_flags,
  here('data', 'prog_flags.rds')
)
