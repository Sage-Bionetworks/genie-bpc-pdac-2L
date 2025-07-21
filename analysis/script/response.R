# This is re-imagined to only include imaging data.

# We can remove all the stuff where we play with windows - the decision was no windows at all.
resp_obs_min = 1
resp_obs_max = Inf

img <- readr::read_csv(
  here('data-raw', 'PANC', 'imaging_level_dataset.csv')
)

# I am not filtering here to CT only because we would be eliminating mostly PET and PET-CT, which are valuable scan types.
img_sum <- img_process(
  img,
  return_minimal = F
)

lot <- readr::read_rds(here('data', 'drug', 'lot.rds'))

cohort <- readr::read_rds(
  here('data', 'cohort_prog_verified.rds')
)

index_lines <- cohort %>%
  select(record_id, index_line)

# Add in some timing data:
index_lines <- lot %>%
  select(
    record_id,
    line_of_therapy,
    regimen_drugs,
    dob_reg_start_int,
    dob_next_line_int
  ) %>%
  left_join(
    index_lines,
    .,
    by = c('record_id', index_line = 'line_of_therapy')
  )

# We can remove all the stuff where we play with windows - the decision was no windows at all.
index_lines %<>%
  mutate(
    max_obs_time = pmin(
      dob_next_line_int,
      dob_reg_start_int + resp_obs_max,
      na.rm = T
    ),
    min_obs_time = dob_reg_start_int + resp_obs_min
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
    # with resp_obs_min = 1 this gets only the day after reg start.
    min_obs_time - 0.5 < dob_event_days,
    # I'm torn on this.  Flatiron and friends of cancer did not count the endpoint - but if someone progressed and started their new regimen on the same day I would find that indicative of a progression on the PREVIOUS regimen.
    # I will include the endpoint for now - there are some scans (~4) where this matters.  Just change to -0.5 to exclude the endpoint.
    max_obs_time + 0.5 > dob_event_days
  )

resp <- incl_img %>%
  add_response_levels(.) %>%
  group_by(record_id) %>%
  arrange(dob_event_days, scan_number) %>%
  summarize(
    n_scans = n(),
    n_scans_eval = sum(!(response_code %in% 'n')),
    resp_codes = paste(response_code, collapse = ""),
    .groups = 'drop'
  )

resp %>% filter(n_scans_eval > 0)

# Sponsor made the request that we truncate followup after the first progression.
resp %<>%
  mutate(
    resp_codes_trunc = str_replace(resp_codes, "p.*", "p")
  )

resp %<>%
  mutate(
    # Confirmed response breakdown (the first one is a subset):
    # - not evaluable or stable disease, 0 or more times.
    # - response.
    # - not evaluable, 0 or more times.
    # - stable or response, 1 or more times.
    resp_cat = stringr::str_detect(resp_codes_trunc, "[ns]*r"),
    resp_cat_conf = stringr::str_detect(resp_codes_trunc, "[ns]*r[n]*[sr]+"),
    # I'll go ahead and write these out as RECIST-speak:
    bor = best_response_one_letter_codes(resp_codes_trunc)
  )

readr::write_rds(
  resp,
  here('data', 'response', 'responses.rds')
)

# Checking (uncomment if needed).
# count(resp, resp_codes, resp_codes_trunc, sort = T) %>% print(n = 100)
# count(resp, resp_codes_trunc, bor, sort = T) %>% print(n = 100)
# resp %>% count(resp_cat_conf)
