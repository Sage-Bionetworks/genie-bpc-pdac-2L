library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

ca_ind <- readr::read_csv(
  here('data-raw', 'PANC', 'cancer_level_dataset_index.csv')
)
cohort <- ca_ind
# flow_track monitors attrition at each step for us.
flow_track <- flow_record_helper(cohort, "BPC PANC v1.2")


dmet_times <- get_dmet_time(ca_ind)
cohort <- left_join(
  select(dmet_times, record_id, ca_seq),
  cohort,
  by = c('record_id', 'ca_seq')
)
flow_track %<>% flow_record_helper(cohort, "Met dx. (anytime)", .)


cohort %<>%
  filter(institution %in% c("DFCI", "MSK", "VICC"))
flow_track %<>% flow_record_helper(cohort, "US sites only", .)

cohort %<>% filter(ca_seq %in% 0) # very few with ca_seq = 1 (~28)
flow_track %<>% flow_record_helper(cohort, "Only one cancer", .)


sample_kras_g12d <- readr::read_rds(
  here('data', 'genomic', 'samp_kras_g12d.rds')
)
cpt <- readr::read_csv(
  here('data-raw', 'PANC', 'cancer_panel_test_level_dataset.csv')
)
kras_g12d_ever <- cpt %>%
  filter(cpt_genie_sample_id %in% sample_kras_g12d) %>%
  pull(record_id)
cohort %<>% filter(record_id %in% kras_g12d_ever)
flow_track %<>% flow_record_helper(cohort, "KRAS G12D+ (ever)", .)


line_evaluations <- readr::read_rds(
  here('data', 'drug', 'line_evaluation.rds')
)
lot <- readr::read_rds(
  here('data', 'drug', 'lot.rds')
)
cohort <- left_join(
  cohort,
  select(
    line_evaluations,
    record_id,
    line1_gem_or_fluoro,
    line2or3_as_met,
    no_invest_to_index,
    index_line
  ),
  by = c('record_id')
)
cohort %<>% filter(line1_gem_or_fluoro)
flow_track %<>% flow_record_helper(cohort, "1L gem/5FU-based", .)


cohort %<>% filter(line2or3_as_met)
flow_track %<>% flow_record_helper(cohort, "Metastatic at 2L/3L", .)


cohort %<>% filter(no_invest_to_index)
flow_track %<>% flow_record_helper(cohort, "No investigational", .)

library(flextable)
flow_track %>%
  mutate(
    n = purrr::map_dbl(
      .x = dat,
      .f = nrow
    )
  ) %>%
  select(-dat) %>%
  rename(step = message) %>%
  flextable(.) %>%
  autofit(.)
flow_process_wrap(flow_track)
