output_cbio_lines <- function(
  ids,
  file = NULL,
  cohort = 'panc_genie_bpc'
) {
  stub1 <- "https://genie-private.cbioportal.org/patient?studyId="
  stub2 <- "&caseId="

  out_lines <- paste0(
    stub1,
    cohort,
    stub2,
    ids
  )

  if (is.null(file)) {
    cat(paste0(out_lines, '\n'))
  } else {
    writeLines(out_lines, file)
  }
}
