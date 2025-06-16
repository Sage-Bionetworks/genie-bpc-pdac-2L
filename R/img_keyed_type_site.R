img_keyed_type_site <- function(
  dat_img
) {
  rtn <- dat_img |>
    select(
      cohort,
      record_id,
      institution,
      scan_number,
      image_scan_int,
      dx_scan_days,
      image_ref_scan_int,
      dx_ref_scan_days,
      image_scan_type,
      contains("image_scansite__"),
      image_ca,
      image_overall
    )

  rtn <- rtn |>
    pivot_longer(
      cols = contains("image_scansite__"),
      names_to = "junk",
      values_to = "site"
    ) |>
    select(-junk)

  rtn <- rtn |>
    filter(!is.na(site))

  # convenience so we don't have to work with these insane strings.
  rtn <- rtn |>
    mutate(
      cancer = case_when(
        str_detect(image_ca, "no evidence of cancer") ~ F,
        str_detect(image_ca, "there is evidence of cancer") ~ T,
        # categories left out of the above: equivocal about cancer, and no mention of cancer.
        T ~ NA
      )
    ) |>
    status_processor(col_name = "image_overall")

  # Put the keys up front for clarity
  rtn <- rtn |>
    select(
      cohort,
      record_id,
      institution, # not really a key.
      scan_number,
      site,
      image_scan_type, # not really a key.
      everything()
    )

  return(rtn)
}
