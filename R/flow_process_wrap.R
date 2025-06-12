flow_process_wrap <- function(dat) {
  dat %>%
    cohort_process_help(.) %>%
    flextable(.) %>%
    color(color = "gray70", part = "body") %>%
    color(i = 1:2, color = "black", part = "body") %>%
    color(j = 1:2, color = "black", part = "body") %>%
    # bold(part = "body", i = 1, j = 3) %>%
    # italic(part = "body", i = 1, j = 3) %>%
    autofit(.)
}
