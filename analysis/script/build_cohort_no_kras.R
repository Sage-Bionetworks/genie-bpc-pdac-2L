# A copy of the build_cohort file, except we skip the KRAS step.  Useful in
#   the genomic analysis that has an unclear purpose.
# I'm going to leave the flow track steps in, but not save them or do anything to them.
library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

ca_ind <- readr::read_csv(
  here('data-raw', 'PANC', 'cancer_level_dataset_index.csv')
)
cohort <- ca_ind

# select just the stuff we need:
cohort %<>%
  select(
    cohort,
    record_id,
    institution,
    ca_seq
  )

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

prog_flags <- readr::read_rds(here('data', 'prog_flags.rds'))
prog_flags %<>% filter(prog_in_range)
cohort %<>% filter(record_id %in% prog_flags$record_id)
flow_track %<>% flow_record_helper(cohort, "Documented prog after 1L", .)


cohort %<>% filter(line2or3_as_met)
flow_track %<>% flow_record_helper(cohort, "Met @ index line", .)


cohort %<>% filter(no_invest_to_index)
flow_track %<>% flow_record_helper(cohort, "No investigational", .)


# REMOVED: KRAS G12D+ before index checked here.
# We DO need to check that they have at least one sample resulted before though, to make this comparison valid.

# Then we'll check that it was path/ordered/resulted before index line.
cpt_ind_reg_timing <- readr::read_rds(
  here('data', 'drug', 'cpt_index_reg_timing.rds')
)
# for each person who has such a case, get their first sample:
cohort <- cpt_ind_reg_timing %>%
  filter(
    report_lte_reg
  ) %>%
  group_by(record_id) %>%
  arrange(dob_cpt_report_days) %>%
  slice(1) %>%
  ungroup(.) %>%
  select(
    record_id,
    first_samp = cpt_genie_sample_id,
    dob_path_proc_days,
    dob_cpt_order_days,
    dob_cpt_report_days,
    path_lte_reg,
    order_lte_reg,
    report_lte_reg
  ) %>%
  inner_join(
    cohort,
    .,
    by = 'record_id'
  )
flow_track %<>% flow_record_helper(cohort, "NGS result before index", .)

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
  here('data', 'cohort_prog_verified_no_kras_check.rds')
)
