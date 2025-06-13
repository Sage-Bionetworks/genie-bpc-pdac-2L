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
