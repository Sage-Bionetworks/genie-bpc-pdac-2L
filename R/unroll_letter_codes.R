unroll_letter_codes <- function(
  str
) {
  str %>%
    # have to do this one first obviously:
    str_replace_all(., "R", "CR, ") %>%
    str_replace_all(., "r", "PR, ") %>%
    str_replace_all(., "s", "SD, ") %>%
    str_replace_all(., "m", "MR, ") %>%
    str_replace_all(., "p", "PD, ") %>%
    str_replace_all(., "n", "NE, ") %>%
    str_replace_all(., ", $", "") # trim the last comma.
}
