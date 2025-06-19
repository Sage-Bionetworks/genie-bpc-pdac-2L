output_cbio_lines <- function(
  ids,
  file,
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

  writeLines(out_lines, file)
}
