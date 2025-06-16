img_prog <- function(
  dat_img,
  impute_longitudinal = T,
  return_minimal = T
) {
  rtn <- dat_img |>
    select(
      cohort,
      record_id,
      scan_number,
      image_scan_int,
      image_ca,
      image_overall
    ) |>
    # Arrange by record_id and scan date
    arrange(record_id, image_scan_int)

  rtn <- rtn |>
    mutate(
      # as it stands, we're sort of imbedding the assumption that any scan is an evaluation for cancer.
      evaluated = !(str_detect(image_ca, "does not mention cancer") |
        str_detect(image_ca, "uncertain, indeterminate")),
      cancer = case_when(
        str_detect(image_ca, "no evidence of cancer") ~ F,
        str_detect(image_ca, "there is evidence of cancer") ~ T,
        T ~ NA
      )
    ) |>
    status_processor(dat = _, col_name = "image_overall")

  if (impute_longitudinal) {
    img_long <- dat_img |> img_keyed_type_site()

    sum_long <- img_long |>
      group_by(record_id, site, image_scan_type) |>
      arrange(image_scan_int) |>
      mutate(
        prev_cancer = lag(cancer)
      ) |>
      # this step replaces NA values with the last known value.
      fill(prev_cancer, .direction = 'down') |>
      mutate(
        long_comp_resp = case_when(
          is.na(prev_cancer) ~ F,
          prev_cancer & !cancer ~ T
        ),
        long_progression = case_when(
          is.na(prev_cancer) ~ F,
          !prev_cancer & cancer ~ T,
          T ~ F
        )
      ) |>
      ungroup()

    # collapse over sites/type for each day - any sign is taken as a sign.
    sum_long <- sum_long |>
      group_by(record_id, image_scan_int) |>
      summarize(
        long_comp_resp = any(long_comp_resp, na.rm = T),
        long_progression = any(long_progression, na.rm = T),
        .groups = 'drop'
      )
  } else {
    # next step is merging, this blank tibble will result in NAs.
    sum_long <- tibble(
      record_id = character(0),
      image_scan_int = double(0),
      long_comp_resp = logical(0),
      long_progression = logical(0)
    )
  }

  rtn <- left_join(
    rtn,
    sum_long,
    by = c('record_id', 'image_scan_int')
  )

  rtn <- rtn |>
    mutate(
      part_resp = case_when(
        raw_response %in% "improving" ~ T,
        T ~ F
      ),
      comp_resp = case_when(
        is.na(long_comp_resp) ~ F, # case if impute_longitudinal = F.
        T ~ long_comp_resp
      ),
      response = part_resp | comp_resp,
      # in cancer terminology progression is worsening.
      # I know it's silly, I just work here.
      progression = case_when(
        raw_response %in% "worsening" ~ T,
        is.na(long_comp_resp) ~ F,
        T ~ long_comp_resp
      )
    )

  if (return_minimal) {
    rtn <- rtn |>
      select(
        cohort,
        record_id,
        image_scan_int,
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
