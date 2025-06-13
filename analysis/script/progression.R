library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

reg <- readr::read_csv(
  here('data-raw', 'PANC', 'regimen_cancer_level_dataset.csv')
)
med_onc <- readr::read_csv(
  here('data-raw', 'PANC', 'med_onc_note_level_dataset.csv')
)
img <- readr::read_csv(
  here('data-raw', 'PANC', 'imaging_level_dataset.csv')
)

med_onc_prog(med_onc)

img %>% glimpse

img |>
  select(record_id, image_scan_int, matches("image_casite")) |>
  pivot_longer(
    cols = -c(record_id, image_scan_int),
    names_to = "var",
    values_to = "site"
  ) |>
  filter(!is.na(site)) |>
  count(site, sort = T)

img |> count(image_scan_type)

img |> count(image_ca)

img |>
  mutate(
    ref_scan_known = !is.na(image_ref_scan_int),
    image_ca = str_sub(image_ca, 1, 10)
  ) |>
  tabyl(
    ref_scan_known,
    image_ca
  )


# make a versin of the imaging dataset which is keyed by site and type in addition to record_id and scan number.
# What's the point of this?  Tracking cancer disappearances over time.

find_complete_responses <- function(dat_img) {
  img_long <- img_keyed_type_site(dat_img)

  # it is possible, though not common, to have multiple
  #   scan types on a given day.  E.g. two CT scans for the
  #   abdomen on July 19th, 2025.  We flatten here to
  #   any evidence of cancer on that day.

  img_long <- img_long |>
    group_by(record_id, scan_number, image_scan_int, image_scan_type, site) |>
    summarize(cancer = any(cancer, na.rm = T), .groups = 'drop')

  rtn <- img_long |>
    group_by(record_id, image_scan_type, site) |>
    arrange(image_scan_int) |>
    mutate(
      prev_cancer = lag(cancer),
      complete_response = prev_cancer & !cancer
    ) |>
    ungroup()

  rtn <- rtn |> filter(complete_response)

  return(rtn)
}

find_complete_responses(img)

img_keyed_type_site(img) |>
  count(record_id, image_scan_type, site, image_scan_int, sort = T)


img |>
  select(record_id, image_ca, image_scan_int, matches("image_casite")) |>
  filter(str_detect(image_ca, "No,")) |>
  pivot_longer(
    cols = -c(record_id, image_ca, image_scan_int),
    names_to = "var",
    values_to = "site"
  ) |>
  filter(!is.na(site)) |>
  count(site, sort = T)

img_prog <- function(
  dat_img,
  return_minimal = T
) {
  img_analysis <- dat_img |>
    select(
      record_id,
      scan_number,
      image_scan_int,
      image_ca,
      image_overall
    ) |>
    # Arrange by record_id and scan date
    arrange(record_id, image_scan_int)

  resp_lev <- c(
    'worsened',
    'stable',
    'mixed',
    'improved'
  )

  rtn <- rtn |>
    mutate(
      # as it stands, we're sort of imbedding the assumption that any scan is an evaluation for cancer.
      evaluated = !(str_detect(image_ca, "does not mention cancer") |
        str_detect(image_ca, "uncertain, indeterminate")),
      cancer = image_ca %in%
        "Yes, the Impression/Plan states or implies there is evidence of cancer",
      raw_response = case_when(
        image_ca_status %in% "Progressing/Worsening/Enlarging" ~ resp_lev[1],
        image_ca_status %in% "Stable/No change" ~ resp_lev[2],
        image_ca_status %in% "Mixed" ~ resp_lev[3],
        image_ca_status %in% "Improving/Responding" ~ resp_lev[4],
        T ~ NA_character_
      ),
      raw_response = factor(raw_response, levels = resp_lev)
    )
}
