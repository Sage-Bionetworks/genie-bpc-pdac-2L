add_response_levels <- function(
    dat_img_sum
) {
   rtn <- dat_img_sum %>%
    mutate(
      # Short codes use RECIST-like naming, pared down to a single letter.
      response_code = case_when(
        # for longitudinal imputation it will make sense to use evaluated, but for the simple case when we're not doing that we can use assume NA is not evaluable.
        is.na(status_change) ~ "n", # for not evaluable"
        # Capital R for complete response is the plan.
        status_change %in% "improving" ~ "r", # for partial response.
        status_change %in% "worsening" ~ "p", # for progressive disease.
        status_change %in% "mixed" ~ "m",
        status_change %in% "stable" ~ "s",
        T ~ "e"
      )
    )
   
   if (any(rtn$response_code %in% "e")) { 
     cli_abort("Unknown response code case encountered")
   }
   return(rtn)
}
        # for longitudinal imputation it will make sense to use evaluated, but for the simple case when we're not doing that we can use assume NA is not evaluable.
        