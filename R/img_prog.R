img_prog <- function(
  dat_img,
  impute_longitudinal = T,
  return_minimal = T
) {
  rtn <- dat_img |>
    select(
      record_id,
      scan_number,
      image_scan_int,
      image_ca,
      image_overall
    ) |>
    # Arrange by record_id and scan date
    arrange(record_id, image_scan_int)

  resp_lev <- c(
    'worsened',
    'stable',
    'mixed',
    'improved'
  )

  rtn <- rtn |>
    mutate(
      # as it stands, we're sort of imbedding the assumption that any scan is an evaluation for cancer.
      evaluated = !(str_detect(image_ca, "does not mention cancer") |
        str_detect(image_ca, "uncertain, indeterminate")),
      cancer = image_ca %in%
        "Yes, the Impression/Plan states or implies there is evidence of cancer",
      raw_response = case_when(
        image_overall %in% "Progressing/Worsening/Enlarging" ~ resp_lev[1],
        image_overall %in% "Stable/No change" ~ resp_lev[2],
        image_overall %in% "Mixed" ~ resp_lev[3],
        image_overall %in% "Improving/Responding" ~ resp_lev[4],
        T ~ NA_character_
      ),
      raw_response = factor(raw_response, levels = resp_lev)
    )

  if (impute_longitudinal) {
    # do something.
  }

  return(rtn)
}
