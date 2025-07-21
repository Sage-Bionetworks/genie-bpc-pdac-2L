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


cpt <- readr::read_csv(
  here('data-raw', 'PANC', 'cancer_panel_test_level_dataset.csv')
)
# my google research tells me that these two are PDAC, while PAAC and UCP are not:
cohort <- cpt %>%
  filter(cpt_oncotree_code %in% c('PAAD', 'PAASC')) %>%
  select(record_id, ca_seq) %>%
  distinct(.) %>%
  inner_join(
    .,
    cohort,
    by = c('record_id', 'ca_seq')
  )
flow_track %<>% flow_record_helper(cohort, "PDAC tumor", .)


cohort %<>%
  filter(institution %in% c("DFCI", "MSK", "VICC"))
flow_track %<>% flow_record_helper(cohort, "US sites only", .)

cohort %<>% filter(ca_seq %in% 0) # very few with ca_seq = 1 (~28)
flow_track %<>% flow_record_helper(cohort, "Only one cancer", .)


dmet_times <- get_dmet_time(ca_ind)
cohort <- inner_join(
  select(dmet_times, record_id, ca_seq),
  cohort,
  by = c('record_id', 'ca_seq')
)
flow_track %<>% flow_record_helper(cohort, "Met dx. (anytime)", .)

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
flow_track %<>% flow_record_helper(cohort, "Met @ index line", .)


cohort %<>% filter(no_invest_to_index)
flow_track %<>% flow_record_helper(cohort, "No investigational", .)


readr::write_rds(
  cohort,
  here('data', 'cohort_prog_not_verified.rds')
)


prog_flags <- readr::read_rds(here('data', 'prog_flags.rds'))
prog_flags %<>% filter(prog_in_range)
cohort %<>% filter(record_id %in% prog_flags$record_id)
flow_track %<>% flow_record_helper(cohort, "Documented prog after 1L", .)

readr::write_rds(
  flow_track,
  here('data', 'flow_track.rds')
)

# Going to put the time/drug of index therapy in here.
cohort <- lot %>%
  select(
    record_id,
    index_line = line_of_therapy,
    regimen_drugs,
    dob_reg_start_int
  ) %>%
  left_join(
    cohort,
    .,
    by = c('record_id', 'index_line'),
    relationship = 'one-to-one'
  )


readr::write_rds(
  cohort,
  here('data', 'cohort_prog_verified.rds')
)

#
#
#
#
# One additional check here, which is not a part of the main flow yet.
# This should probably be it's own script but.... it's not.
index_line_times <- select(cohort, record_id, index_line) %>%
  left_join(
    .,
    select(lot, record_id, line_of_therapy, dx_reg_start_int),
    by = c('record_id', index_line = 'line_of_therapy')
  )
first_g12d_pos <- cpt %>%
  filter(cpt_genie_sample_id %in% sample_kras_g12d) %>%
  group_by(record_id) %>%
  mutate(
    dx_cpt_ord_days = dx_cpt_rep_days - (dob_cpt_report_days - cpt_order_int)
  ) %>%
  summarize(
    dx_first_kras_g12d_pos_rep = min(dx_cpt_rep_days, na.rm = T),
    dx_first_kras_g12d_pos_ord = min(dx_cpt_ord_days, na.rm = T)
  )

index_line_times %<>%
  left_join(
    .,
    first_g12d_pos,
    by = 'record_id'
  ) %>%
  mutate(
    pos_rep_before_index = dx_reg_start_int > dx_first_kras_g12d_pos_rep,
    pos_ord_before_index = dx_reg_start_int > dx_first_kras_g12d_pos_ord
  )

readr::write_rds(
  index_line_times,
  here('data', 'drug', 'cpt_index_timing.rds')
)

# count(index_line_times, pos_rep_before_index)
# count(index_line_times, pos_ord_before_index)
