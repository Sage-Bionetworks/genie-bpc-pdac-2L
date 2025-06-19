# Derive the survival estimates for 2L panc

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

cohort_pv_no_kras <- readr::read_rds(here(
  'data',
  'cohort_prog_verified_no_kras_check.rds'
))
kras_groups <- readr::read_rds(
  here(
    'data',
    'genomic',
    'kras_groups.rds'
  )
)
cpt <- readr::read_csv(
  here(
    'data-raw',
    'PANC',
    'cancer_panel_test_level_dataset.csv'
  )
)

cohort <- get_first_cpt(cohort_pv_no_kras, cpt, include_sample_id = T) %>%
  select(record_id, sample_id = cpt_genie_sample_id) %>%
  left_join(cohort_pv_no_kras, ., by = 'record_id') %>%
  left_join(., kras_groups, by = 'sample_id')


cohort <- cohort %>%
  mutate(
    g12d_vs_g12 = case_when(
      alt_g12d ~ "G12D",
      alt_g12other ~ "G12[*]",
      T ~ NA_character_
    ),
    g12d_vs_wt = case_when(
      alt_g12d ~ "G12D",
      no_kras_g12 ~ "WT",
      T ~ NA_character_
    )
  )

cpt <- readr::read_csv(
  here('data-raw', 'PANC', 'cancer_panel_test_level_dataset.csv')
)

dat_surv <- cohort |>
  select(
    g12d_vs_g12,
    g12d_vs_wt,
    record_id,
    ca_seq,
    stage_dx_iv,
    dmets_stage_i_iii,
    line1_gem_or_fluoro,
    line2or3_as_met,
    no_invest_to_index,
    index_line
  )

first_cpt <- get_first_cpt(
  ca_ind_dat = distinct(select(cohort, record_id, ca_seq)),
  cpt_dat = cpt
)

dat_surv <- left_join(
  dat_surv,
  first_cpt,
  by = c('record_id', 'ca_seq')
)

reg <- readr::read_csv(
  here('data-raw', 'PANC', 'regimen_cancer_level_dataset.csv')
)
lot <- readr::read_rds(
  here('data', 'drug', 'lot.rds')
)

dat_surv <- dat_surv |>
  left_join(
    x = _,
    y = lot,
    by = c('record_id', index_line = "line_of_therapy"),
    relationship = 'many-to-one' # now that we can have two copies of one person.
  ) |>
  left_join(
    x = _,
    y = select(
      reg,
      record_id,
      ca_seq,
      regimen_number,
      os_g_status,
      tt_os_g_days
    ),
    by = c('record_id', 'ca_seq', 'regimen_number'),
    relationship = 'many-to-one' # now that we can have two copies of one person.
  )

dat_surv %<>%
  mutate(
    reg_cpt_days = dx_cpt_rep_days - dx_reg_start_int
  )

dat_surv <- remove_trunc_gte_event(
  dat_surv,
  trunc_var = 'reg_cpt_days',
  event_var = 'tt_os_g_days'
)

# months are a silly unit to use, but here we go:
dat_surv %<>%
  mutate(
    reg_cpt_mos = reg_cpt_days / 30.4,
    tt_os_g_mos = tt_os_g_days / 30.4
  )

surv_obj_os <- with(
  dat_surv,
  Surv(
    time = reg_cpt_mos,
    time2 = tt_os_g_mos,
    event = os_g_status
  )
)

gg_os_geno_1 <- plot_one_survfit(
  dat = dat_surv,
  surv_form = surv_obj_os ~ g12d_vs_g12,
  plot_title = "OS from initiation of index therapy",
  plot_subtitle = "Adjusted for delayed entry (independent)",
  x_breaks = seq(0, 500, by = 3),
  x_title = "Months",
  x_exp = 0,
  pal = c('#ee7733', '#0077bb')
) +
  add_confidence_interval(type = 'ribbon') +
  coord_cartesian(xlim = c(0, 3 * 12))

gg_os_geno_2 <- plot_one_survfit(
  dat = dat_surv,
  surv_form = surv_obj_os ~ g12d_vs_wt,
  plot_title = "OS from initiation of index therapy",
  plot_subtitle = "Adjusted for delayed entry (independent)",
  x_breaks = seq(0, 500, by = 3),
  x_title = "Months",
  x_exp = 0,
  pal = c('#ee7733', '#0077bb')
) +
  add_confidence_interval(type = 'ribbon') +
  coord_cartesian(xlim = c(0, 3 * 12))

model_bundle <- list(
  dat_surv = dat_surv,
  gg_os_geno_1 = gg_os_geno_1,
  gg_os_geno_2 = gg_os_geno_2
)

readr::write_rds(
  model_bundle,
  here('data', 'survival', 'geno_comp_bundle.rds')
)

# Relevant comparator:
# https://pmc.ncbi.nlm.nih.gov/articles/PMC6962478/
# 7.3 mo (5.3, 9.3)
