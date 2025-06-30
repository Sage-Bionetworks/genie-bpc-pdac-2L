filter_times_by_ref <- function(
  dat,
  ref_dat,
  t_col,
  t_ref_col,
  merge_cols = 'record_id',
  lower_int = 0,
  upper_int = NULL, # by default does not filter on upper bound.
  tol = 0.5
) {
  if (is.null(lower_int) & is.null(upper_int)) {
    cli_abort("lower_int or upper_int must be supplied")
  }

  # some checking that the times seem to be on the same scale seems smart - but it's not here yet.

  new_dat <- ref_dat %>%
    # just in case they share a name:
    rename(.t_ref_col = .data[[t_ref_col]]) %>%
    select(all_of(merge_cols), .t_ref_col) %>%
    left_join(
      dat,
      .,
      by = merge_cols
    )

  new_dat %<>%
    mutate(.diff = .data[[t_col]] - .t_ref_col)

  if (!is.null(lower_int)) {
    new_dat %<>%
      filter(.diff > lower_int - tol)
  }

  if (!is.null(upper_int)) {
    new_dat %<>%
      filter(.diff < upper_int + tol)
  }

  new_dat %<>%
    select(-c(.diff, .t_ref_col))

  return(new_dat)
}
