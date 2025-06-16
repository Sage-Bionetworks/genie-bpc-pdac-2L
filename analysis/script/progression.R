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

# Seems about right on med onc... test cases:
med_onc_prog(med_onc, impute_longitudinal = T) |>
  count(progression)
med_onc_prog(med_onc, impute_longitudinal = F) |>
  count(progression)

med_onc_prog(med_onc, impute_longitudinal = T) |>
  count(response)
med_onc_prog(med_onc, impute_longitudinal = F) |>
  count(response)

img |> img_prog(impute_longitudinal = F)


img |> img_prog(impute_longitudinal = T)

img |>
  img_keyed_type_site() |>
  group_by(record_id, site, image_scan_type) |>
  arrange_by(image_scan_int) |>
  mutate(
    prev_cancer = lag(cancer)
  ) |>
  # this step replaces NA values with the last known value.
  fill(prev_cancer, .direction = 'down') |>
  mutate(
    comp_resp = case_when(
      is.na(prev_cancer) ~ F,
      prev_cancer & !cancer ~ T
    ),
    long_progression = case_when(
      is.na(prev_cancer) ~ F,
      !prev_cancer & cancer ~ T,
      T ~ F
    )
  )


#
#   group_by(record_id, site, image_scan_type) |>
#   mutate(
#     prev_cancer = lag(cancer)
#   ) |>
#   # this step replaces NA values with the last known value.
#   fill(prev_cancer, .direction = 'down') |>
#   mutate(

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


img_prog(img)
