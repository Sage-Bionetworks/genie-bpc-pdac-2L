img_process <- function(
  dat_img,
  return_minimal = F
) {
  rtn <- dat_img |>
    select(
      cohort,
      record_id,
      scan_number,
      image_scan_int,
      image_ca,
      image_overall,
      image_scan_type,
      scan_sites
    ) |>
    # Arrange by record_id and scan date
    arrange(record_id, image_scan_int)

  rtn <- rtn |>
    mutate(
      # indeterminate is basically not evaluable.
      evaluated = case_when(
        str_detect(image_ca, "no evidence of cancer") ~ T,
        str_detect(image_ca, "evidence of cancer") &
          str_detect(image_overall, "Progressing|Improving|Stable|Mixed") ~
          T,
        T ~ F
      ),
      cancer = case_when(
        str_detect(image_ca, "no evidence of cancer") ~ F,
        str_detect(image_ca, "there is evidence of cancer") ~ T,
        T ~ NA
      )
    ) |>
    status_processor(dat = _, col_name = "image_overall")

  # generalizing the time variable so this can be stacked with med onc.
  rtn %<>%
    mutate(
      event_type = "img"
    ) %>%
    rename(dob_event_days = image_scan_int) %>%
    relocate(event_type, .before = dob_event_days)

  if (return_minimal) {
    # drops the original columns.
    rtn <- rtn |>
      select(
        cohort,
        record_id,
        dob_event_days,
        evaluated,
        cancer,
        status_change
      )
  }

  return(rtn)
}
