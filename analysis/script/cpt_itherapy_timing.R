library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

line_evaluations <- readr::read_rds(
  here('data', 'drug', 'line_evaluation.rds')
)
lot <- readr::read_rds(
  here('data', 'drug', 'lot.rds')
)
cpt <- readr::read_csv(
  here('data-raw', 'PANC', 'cancer_panel_test_level_dataset.csv')
)

# Note that this is POSSIBLE index lines. Other stuff needs to be checked later on.
index_lines <- line_evaluations %>%
  filter(!is.na(index_line)) %>%
  select(
    record_id,
    index_line
  ) %>%
  left_join(
    .,
    select(lot, record_id, line_of_therapy, regimen_drugs, dob_reg_start_int),
    by = c('record_id', index_line = 'line_of_therapy')
  )

cpt_mini <- cpt %>%
  mutate(
    dob_path_proc_days = dob_cpt_report_days - path_proc_cpt_rep_days
  ) %>%
  select(
    record_id,
    cpt_number,
    ca_seq,
    dob_path_proc_days,
    dob_cpt_order_days = cpt_order_int,
    dob_cpt_report_days,
    cpt_genie_sample_id
  )

cpt_mini %<>%
  # taking advantage of the fact that we're doing solo cancers only right now:
  filter(ca_seq %in% 0) %>%
  select(-ca_seq) %>%
  relocate(cpt_genie_sample_id, .after = cpt_number)

sample_kras_g12d <- readr::read_rds(
  here('data', 'genomic', 'samp_kras_g12d.rds')
)
cpt_mini %<>%
  mutate(kras_g12d = cpt_genie_sample_id %in% sample_kras_g12d)

# Inner join because if they don't have a ca_seq = 0 cpt test and
#   they don't have an index line, we don't need them.
timing_df <- inner_join(
  index_lines,
  cpt_mini,
  by = 'record_id',
  relationship = 'one-to-many'
)

timing_df %<>%
  mutate(
    path_lte_reg = dob_path_proc_days <= dob_reg_start_int,
    order_lte_reg = dob_cpt_order_days <= dob_reg_start_int,
    report_lte_reg = dob_cpt_report_days <= dob_reg_start_int
  )

# We expect the number of people with event before reg to be: path > order > report.
# timing_df %>% count(path_lte_reg)
# timing_df %>% count(order_lte_reg)
# timing_df %>% count(report_lte_reg)
# Correct.

readr::write_rds(
  timing_df,
  here('data', 'drug', 'cpt_index_reg_timing.rds')
)
