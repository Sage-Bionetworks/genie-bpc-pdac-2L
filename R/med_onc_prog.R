med_onc_prog <- function(dat_med_onc, return_minimal = T) {
  # In this dataset there are no duplicates reports for one
  #  person on one day, but let's add a check to bake in that assumption.
  chk_unique <- dat_med_onc |>
    count(record_id, md_onc_visit_int) |>
    pull(n) |>
    max()
  if (chk_unique > 1) {
    cli_abort(
      "There are people with more than one med onc note on a given day - will break function"
    )
  }

  rtn <- dat_med_onc |>
    select(
      cohort,
      record_id,
      md_onc_visit_int,
      md_ca,
      md_ca_status
    )

  # shorter names for the response levels
  resp_lev <- c(
    'worsened',
    'stable',
    'mixed',
    'improved'
  )

  rtn <- rtn |>
    mutate(
      evaluated = !(str_detect(md_ca, "does not mention cancer") |
        str_detect(md_ca, "uncertain, indeterminate")),
      cancer = md_ca %in%
        "Yes, the Impression/Plan states or implies there is evidence of cancer",
      raw_response = case_when(
        md_ca_status %in% "Progressing/Worsening/Enlarging" ~ resp_lev[1],
        md_ca_status %in% "Stable/No change" ~ resp_lev[2],
        md_ca_status %in% "Mixed" ~ resp_lev[3],
        md_ca_status %in% "Improving/Responding" ~ resp_lev[4],
        T ~ NA_character_
      ),
      raw_response = factor(raw_response, levels = resp_lev)
    )

  rtn |>
    arrange(md_onc_visit_int) |>
    group_by(record_id) |>
    mutate(
      prev_cancer = lag(cancer),
      improvement = case_when(
        raw_response %in% "improving" ~ T,
        is.na(prev_cancer) ~ F, # nothing to go on then.
        prev_cancer & !cancer ~ T, # went from cancer to no cancer.
        T ~ F
      ),
      # in cancer terminology progression is worsening.
      # I know it's silly, I just work here.
      progression = case_when(
        raw_response %in% "worsening" ~ T,
        is.na(prev_cancer) ~ F,
        !prev_cancer & cancer ~ T, # went from no cancer to cancer.
        T ~ F
      )
    )

  if (return_minimal) {
    rtn <- rtn |>
      select(
        cohort,
        record_id,
        evaluated,
        cancer,
        raw_response
      )
  }

  return(rtn)
}
