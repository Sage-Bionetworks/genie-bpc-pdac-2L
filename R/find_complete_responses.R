find_complete_responses <- function(dat_img) {
  img_long <- img_keyed_type_site(dat_img)

  # it is possible, though not common, to have multiple
  #   scan types on a given day.  E.g. two CT scans for the
  #   abdomen on July 19th, 2025.  We flatten here to
  #   any evidence of cancer on that day.

  img_long <- img_long |>
    group_by(record_id, scan_number, image_scan_int, image_scan_type, site) |>
    summarize(cancer = any(cancer, na.rm = T), .groups = 'drop')

  rtn <- img_long |>
    group_by(record_id, image_scan_type, site) |>
    arrange(image_scan_int) |>
    mutate(
      prev_cancer = lag(cancer),
      complete_response = prev_cancer & !cancer,
      new_lesion = !prev_cancer & cancer
    ) |>
    ungroup()

  rtn <- filter(complete_response | new_lesion) |>
    select(-prev_cancer)

  return(rtn)
}
