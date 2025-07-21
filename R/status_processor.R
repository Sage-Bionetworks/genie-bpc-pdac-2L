# in a rare moment of sanity the med_onc and image datasets
#   have the same levels for response, so we can use a function.
status_processor <- function(
  dat,
  col_name,
  output_name = "status_change"
) {
  resp_lev <- c(
    'worsening',
    'stable',
    'mixed',
    'improving'
  )

  rtn <- dat |>
    mutate(
      .temp = case_when(
        .data[[col_name]] %in% "Progressing/Worsening/Enlarging" ~ resp_lev[1],
        .data[[col_name]] %in% "Stable/No change" ~ resp_lev[2],
        .data[[col_name]] %in% "Mixed" ~ resp_lev[3],
        .data[[col_name]] %in% "Improving/Responding" ~ resp_lev[4],
        T ~ NA_character_
      ),
      .temp = factor(.temp, levels = resp_lev)
    )

  rtn <- rename(rtn, {{ output_name }} := .temp)

  return(rtn)
}
