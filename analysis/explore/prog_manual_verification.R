# To check our progression algorithm is working I'm going to pull 10 random people who (1) had a progression and (2) did not for the last step of our cohort derivation.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

flow_track <- readr::read_rds(
  here('data', 'flow_track.rds')
)

pts_prog <- flow_track %>%
  filter(str_detect(message, "prog after 1L")) %>%
  pull(dat) %>%
  .[[1]]

pts_no_prog <- flow_track %>%
  filter(str_detect(message, "No investigational")) %>%
  pull(dat) %>%
  .[[1]]
pts_no_prog %<>% filter(!(record_id %in% pts_prog$record_id))

pts_prog <- pts_prog$record_id
pts_no_prog <- pts_no_prog$record_id

# Select 10 random ones from each:
set.seed(1234)
prog_samp <- sample(pts_prog, 10, replace = F)
no_prog_samp <- sample(pts_no_prog, 10, replace = F)

output_cbio_lines(
  ids = prog_samp,
  file = here('analysis', 'explore', 'test_cases_who_progressed.txt')
)

output_cbio_lines(
  ids = no_prog_samp,
  file = here('analysis', 'explore', 'test_cases_who_did_not_progress.txt')
)

output_cbio_lines(
  ids = pts_no_prog,
  file = here('analysis', 'explore', 'complete_cases_who_did_not_progress.txt')
)
