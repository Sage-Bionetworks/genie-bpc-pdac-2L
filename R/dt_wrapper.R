dt_wrapper <- function(x, cap = NULL, font.size = '10pt', page_length = 10) {
  DT::datatable(
    x,
    style = "bootstrap4",
    fillContainer = F,
    rownames = F,
    caption = cap,
    options = list(
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        paste0(
          "$(this.api().table().container()).css({'font-size': '",
          font.size,
          "'});"
        ),
        "}"
      ),
      pageLength = page_length
    )
  )
}
