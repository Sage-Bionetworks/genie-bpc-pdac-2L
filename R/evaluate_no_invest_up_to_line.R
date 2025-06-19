# Evaluate that there are no investigational agents up to the line specified.
evaluate_no_invest_up_to_line <- function(dat_person, line) {
  if (max(pull(count(dat_person, line_of_therapy), n), na.rm = T) > 1) {
    cli_abort("This function is made for just one person")
  }

  chk <- dat_person %>%
    filter(line_of_therapy <= line) %>%
    mutate(chk = !str_detect(regimen_drugs, "Investigational")) %>%
    pull(chk) %>%
    all(.)

  if (length(chk) %in% 0) {
    chk <- FALSE
  }

  return(chk)
}
