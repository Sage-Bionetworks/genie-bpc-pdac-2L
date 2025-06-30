# Note on longitudinal imputation for imaging and med onc:
# I changed this to FALSE for both, because it requires fewer assumptions and less explanation, now that this is situated as a sensitivity analysis.

OBS_WINDOW_MIN <- 1 # min time from regimen start to resp.
OBS_WINDOW_MAX <- 365 # max times for same.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

img <- readr::read_csv(
  here('data-raw', 'PANC', 'imaging_level_dataset.csv')
)

# All the names are currently wrong.
# We will need to redo complete response.
img_sum <- img_prog(
  img,
  impute_longitudinal = F
)


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

cohort <- readr::read_rds(
  here('data', 'cohort_prog_verified.rds')
)

index_lines <- cohort %>%
  select(record_id, index_line)

# Add the timing data in to this:
index_lines <- lot %>%
  select(record_id, line_of_therapy, regimen_drugs, dob_reg_start_int) %>%
  left_join(
    index_lines,
    .,
    by = c('record_id', index_line = 'line_of_therapy')
  )

# Add timing in for the line after index:
index_lines <- lot %>%
  # could always add in regimen_drugs_here.
  select(
    record_id,
    line_of_therapy,
    dob_line_after_index_start_int = dob_reg_start_int
  ) %>%
  left_join(
    (index_lines %>%
      mutate(line_after_index = index_line + 1)),
    .,
    by = c('record_id', line_after_index = 'line_of_therapy')
  )

index_lines %<>%
  mutate(
    max_obs_time = pmin(
      dob_line_after_index_start_int,
      dob_reg_start_int + OBS_WINDOW_MAX,
      na.rm = T
    ),
    min_obs_time = dob_reg_start_int + OBS_WINDOW_MIN
  )

incl_img <- left_join(
  img_sum,
  select(
    index_lines,
    record_id,
    min_obs_time,
    max_obs_time
  ),
  by = 'record_id'
) %>%
  filter(
    min_obs_time - 0.5 < image_scan_int,
    # here we DON'T want the endpoint to count.
    max_obs_time - 0.5 > image_scan_int
  )

incl_img %>%
  group_by(record_id) %>%
  arrange(image_scan_int) %>%
  filter(!is.na(raw_response)) %>%
  slice(1) %>%
  ungroup(.) %>%
  filter(raw_response %in% "improving") %>%
  pull(record_id) %>%
  output_cbio_lines(
    .,
    file = here(
      'analysis',
      'explore',
      'responders.txt'
    )
  )

# were there any scans that said no cancer?
incl_img %>%
  group_by(record_id) %>%
  filter(cancer %in% FALSE) %>%
  summarize(n = n()) %>%
  pull(record_id) %>%
  output_cbio_lines(
    .,
    file = here(
      'analysis',
      'explore',
      'people_with_no_cancer_scans.txt'
    )
  )

# State change example: GENIE-DFCI-109719
