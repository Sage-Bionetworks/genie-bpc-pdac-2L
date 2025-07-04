# Turns the variable into a factor for easier processing.
format_ptlevel_naaccr_race_code_primary <- function(col, drop_unused = T) {
  f <- factor(
    col,
    # pulled straight from AACR data guide:
    levels = c(
      "White",
      "Black",
      "American Indian, Aleutian, or Eskimo  ",
      "Chinese",
      "Japanese",
      "Filipino",
      "Hawaiian",
      "Korean",
      "Vietnamese",
      "Laotian",
      "Hmong",
      "Kampuchean (Cambodian)",
      "Thai",
      "Asian Indian or Pakistani NOS",
      "Asian Indian",
      "Pakistani",
      "Micronesian NOS",
      "Chamorro/Chamoru",
      "Guamanian NOS",
      "Polynesian NOS",
      "Tahitian",
      "Samoan",
      "Tongan",
      "Melanesian NOS",
      "Fiji Islander",
      "New Guinean",
      "Other Asian",
      "Pacific Islander NOS",
      "Other",
      "Unknown"
    )
  )

  f <- forcats::fct_na_value_to_level(f, level = "Unknown")

  f %<>%
    forcats::fct_collapse(
      `Asian` = c(
        "Chinese",
        "Japanese",
        "Filipino",
        "Korean",
        "Vietnamese",
        "Laotian",
        "Hmong",
        "Kampuchean (Cambodian)",
        "Thai",
        "Asian Indian or Pakistani NOS",
        "Asian Indian",
        "Pakistani",
        "Other Asian",
        "Pacific Islander NOS"
      ),
      `Other or Unk.` = c(
        "Other",
        "Unknown"
      )
    )

  if (drop_unused) {
    f <- forcats::fct_drop(f)
  }

  return(f)
}
