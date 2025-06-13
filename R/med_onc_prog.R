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
      cancer = case_when(
        str_detect(md_ca, "no evidence of cancer") ~ F,
        str_detect(md_ca, "there is evidence of cancer") ~ T,
        T ~ NA
      ),
      raw_response = case_when(
        md_ca_status %in% "Progressing/Worsening/Enlarging" ~ resp_lev[1],
        md_ca_status %in% "Stable/No change" ~ resp_lev[2],
        md_ca_status %in% "Mixed" ~ resp_lev[3],
        md_ca_status %in% "Improving/Responding" ~ resp_lev[4],
        T ~ NA_character_
      ),
      raw_response = factor(raw_response, levels = resp_lev)
    )

  rtn <- rtn |>
    arrange(md_onc_visit_int) |>
    group_by(record_id) |>
    mutate(
      prev_cancer = lag(cancer)
    ) |>
    # this step replaces NA values with the last known value.
    fill(prev_cancer, .direction = 'down') |>
    mutate(
      part_resp = case_when(
        raw_response %in% "improving" ~ T,
        T ~ F
      ),
      comp_resp = case_when(
        is.na(prev_cancer) ~ F,
        prev_cancer & !cancer ~ T, # went from cancer to no cancer.
        T ~ F
      ),
      response = part_resp | comp_resp,
      # in cancer terminology progression is worsening.
      # I know it's silly, I just work here.
      progression = case_when(
        raw_response %in% "worsening" ~ T,
        is.na(prev_cancer) ~ F,
        !prev_cancer & cancer ~ T, # went from no cancer to cancer.
        T ~ F
      )
    ) |>
    ungroup()

  if (return_minimal) {
    rtn <- rtn |>
      select(
        cohort,
        record_id,
        evaluated,
        cancer,
        raw_response,
        part_resp,
        comp_resp,
        response,
        progression
      )
  }

  return(rtn)
}
