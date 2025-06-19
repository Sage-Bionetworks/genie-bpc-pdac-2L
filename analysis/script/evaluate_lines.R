library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

lot <- readr::read_rds(
  here('data', 'drug', 'lot.rds')
)
reg <- readr::read_csv(
  here('data-raw', 'PANC', 'regimen_cancer_level_dataset.csv')
)

# because our line requirement is so convoluted we're doing it in a
#  computationally slow way.
evaluate_first_line_drug <- function(dat_person) {
  if (max(pull(count(dat_person, line_of_therapy), n), na.rm = T) > 1) {
    cli_abort("This function is made for just one person")
  }

  line_1_chk <- dat_person %>%
    filter(line_of_therapy %in% 1) %>%
    mutate(chk = gem_based | fluoro_based) %>%
    pull(chk)

  return(line_1_chk)
}

evaluate_met_at_line <- function(dat_person, line) {
  if (max(pull(count(dat_person, line_of_therapy), n), na.rm = T) > 1) {
    cli_abort("This function is made for just one person")
  }

  chk <- dat_person %>%
    filter(line_of_therapy %in% line) %>%
    pull(post_met_reg)

  if (length(chk) %in% 0) {
    chk <- FALSE
  }

  return(chk)
}


# This is slow - tons of function calls you could avoid if you cared.  We're patient, careful scientists so this is fine.
line_eval <- lot %>%
  nest(.by = record_id) %>%
  mutate(
    line1_gem_or_fluoro = purrr::map_lgl(
      .f = evaluate_first_line_drug,
      .x = data
    ),
    # was this person metastatic at the time of their second line?
    line2_as_met = purrr::map_lgl(
      .f = \(z) evaluate_met_at_line(z, line = 2),
      .x = data
    ),
    line3_as_met = purrr::map_lgl(
      .f = \(z) evaluate_met_at_line(z, line = 3),
      .x = data
    ),
    no_invest_thru_line2 = purrr::map_lgl(
      .f = \(z) evaluate_no_invest_up_to_line(z, line = 2),
      .x = data
    ),
    no_invest_thru_line3 = purrr::map_lgl(
      .f = \(z) evaluate_no_invest_up_to_line(z, line = 3),
      .x = data
    )
  )

# lot %>% filter(record_id %in% 'GENIE-DFCI-003227 ') %>% glimpse
#     evaluate_no_invest_up_to_line(., line = 2)

line_eval %<>%
  mutate(
    line2or3_as_met = line2_as_met | line3_as_met,
    index_line = case_when(
      !line1_gem_or_fluoro ~ NA_real_,
      line2_as_met ~ 2,
      line3_as_met ~ 3
    ),
    no_invest_to_index = case_when(
      index_line %in% 2 ~ no_invest_thru_line2,
      index_line %in% 3 ~ no_invest_thru_line3,
      T ~ NA
    )
  )

readr::write_rds(
  line_eval,
  here('data', 'drug', 'line_evaluation.rds')
)
