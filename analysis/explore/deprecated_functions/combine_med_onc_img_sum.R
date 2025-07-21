combine_med_onc_img_sum <- function(
  med_onc_sum,
  img_sum
) {
  rtn <- full_join(
    select(
      med_onc_sum,
      cohort,
      record_id,
      dob_eval_days = md_onc_visit_int,
      med_onc_eval = evaluated,
      med_onc_resp = response,
      med_onc_prog = progression
    ),
    select(
      img_sum,
      cohort,
      record_id,
      dob_eval_days = image_scan_int,
      img_eval = evaluated,
      img_resp = response,
      img_prog = progression
    ),
    by = c('cohort', 'record_id', 'dob_eval_days')
  )

  return(rtn)
}
