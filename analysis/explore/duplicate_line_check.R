# builds on create_lines.R - run that first and this may work.  but it's exploratory so no whining if not.

cohort_prog_not_verified <- readr::read_rds(
  here('data', 'cohort_prog_not_verified.rds')
)

rel_group <- cohort_prog_not_verified %>%
  select(record_id, index_line)

line_small <- line_of_ther %>%
  filter(record_id %in% rel_group$record_id) %>%
  left_join(., rel_group, by = 'record_id') %>%
  filter(line_of_therapy <= index_line)

line_small %>%
  group_by(record_id) %>%
  mutate(
    prev_reg_drugs = lag(regimen_drugs),
    dup_reg = prev_reg_drugs == regimen_drugs,
    any_dup = any(dup_reg, na.rm = T)
  ) %>%
  ungroup(.) %>%
  filter(any_dup) %>%
  select(record_id, line_of_therapy, regimen_drugs, prev_reg_drugs) %>%
  arrange(record_id, line_of_therapy) %>%
  View(.)

# Hack:  I'm going to save the cohort before and after duplicate lines (which requires a code edit - there's no easy flag here):
readr::write_rds(
  cohort,
  here('analysis', 'explore', 'cohort_before
