# Derive the survival estimates for 2L panc

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)
cohort <- readr::read_rds(here('data', 'cohort_prog_verified.rds'))

cpt <- readr::read_csv(
  here('data-raw', 'PANC', 'cancer_panel_test_level_dataset.csv')
)

dat_surv <- cohort |>
  select(
    record_id,
    ca_seq,
    stage_dx_iv,
    dmets_stage_i_iii,
    line1_gem_or_fluoro,
    line2or3_as_met,
    no_invest_to_index,
    index_line
  )

first_cpt <- get_first_cpt(ca_ind_dat = cohort, cpt_dat = cpt)

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
    relationship = 'one-to-one'
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
    relationship = 'one-to-one'
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

# months are a really stupid unit to use, but here we go:
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


gg_os <- plot_one_survfit(
  dat = dat_surv,
  surv_form = surv_obj_os ~ 1,
  plot_title = paste0(
    "OS from initiation of index therapy (n=",
    nrow(dat_surv),
    ")"
  ),
  plot_subtitle = "Adjusted for delayed entry (independent)",
  x_breaks = seq(0, 500, by = 3),
  x_title = "Months",
  x_exp = 0
) +
  add_confidence_interval() +
  coord_cartesian(xlim = c(0, 1.5 * 12))

model_bundle <- list(
  dat_surv = dat_surv,
  gg_os = gg_os
)

surv_table <- survfit(
  data = dat_surv,
  surv_obj_os ~ 1,
)

gt_median_surv <- gtsummary::tbl_survfit(
  surv_table,
  probs = 0.5,
  label_header = "**Median Survival**"
)

gt_surv_times <- gtsummary::tbl_survfit(
  surv_table,
  times = seq(3, 12, by = 3),
  label_header = "Month {time}"
)

gt_surv_times


model_bundle <- c(
  model_bundle,
  gt_med_surv = list(gt_median_surv),
  gt_surv_times = list(gt_surv_times)
)

readr::write_rds(
  model_bundle,
  here('data', 'survival', 'main_model_bundle.rds')
)

# Relevant comparator:
# https://pmc.ncbi.nlm.nih.gov/articles/PMC6962478/
# 7.3 mo (5.3, 9.3)
