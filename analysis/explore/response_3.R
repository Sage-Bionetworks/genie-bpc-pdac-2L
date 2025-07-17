# This is re-imagined to only include imaging data.

img <- readr::read_csv(
  here('data-raw', 'PANC', 'imaging_level_dataset.csv')
)

# I am not filtering here to CT only because we would be eliminating mostly PET and PET-CT, which are valuable scan types.
img_sum <- img_prog(
  img,
  impute_longitudinal = F,
  return_minimal = F
)


lot <- readr::read_rds(here('data', 'drug', 'lot.rds'))


cohort <- readr::read_rds(
  here('data', 'cohort_prog_verified.rds')
)

index_lines <- cohort %>%
  select(record_id, index_line)

# Add the timing data in to this:
index_lines <- lot %>%
  select(record_id, line_of_therapy, regimen_drugs, dob_reg_start_int) %>%
  left_join(
    index_lines,
    .,
    by = c('record_id', index_line = 'line_of_therapy')
  )

# Add timing in for the line after index:
index_lines <- lot %>%
  # could always add in regimen_drugs_here.
  select(
    record_id,
    line_of_therapy,
    dob_line_after_index_start_int = dob_reg_start_int
  ) %>%
  left_join(
    (index_lines %>%
      mutate(line_after_index = index_line + 1)),
    .,
    by = c('record_id', line_after_index = 'line_of_therapy')
  )

skel <- expand_grid(
  obs_min = c(0, 28),
  obs_max = c(91, 182, 364, 364 * 5)
)

skel %<>%
  mutate(
    dat_index = list(index_lines),
    dat_img = list(img_sum),
    dat_mo = list(med_onc_sum)
  )

skel %<>%
  mutate(
    # index data with observation windows.
    dat_index_obs_wind = purrr::pmap(
      .l = list(
        o_min = obs_min,
        o_max = obs_max,
        dat = dat_index
      ),
      # add columns for min/max time that account for next regimen, start time of this regimen, etc.
      .f = \(o_min, o_max, dat) {
        dat %>%
          mutate(
            max_obs_time = pmin(
              dob_line_after_index_start_int,
              dob_reg_start_int + o_max,
              na.rm = T
            ),
            min_obs_time = dob_reg_start_int + o_min
          )
      }
    )
  )

skel %<>%
  mutate(
    incl_img = purrr::map2(
      .x = dat_index_obs_wind,
      .y = dat_img,
      # filter to only the images in the window.
      .f = \(ind, img) {
        left_join(
          img,
          select(
            ind,
            record_id,
            min_obs_time,
            max_obs_time
          ),
          by = 'record_id'
        ) %>%
          filter(
            min_obs_time - 0.5 < image_scan_int,
            # here we DON'T want the endpoint to count.
            max_obs_time - 0.5 > image_scan_int
          )
      }
    )
  )


skel %<>%
  mutate(
    incl_mo = purrr::map2(
      .x = dat_index_obs_wind,
      .y = dat_mo,
      # filter to only the med onc observations in the window.
      .f = \(ind, mo) {
        left_join(
          mo,
          select(
            ind,
            record_id,
            min_obs_time,
            max_obs_time
          ),
          by = 'record_id'
        ) %>%
          filter(
            min_obs_time - 0.5 < md_onc_visit_int,
            # here we DON'T want the endpoint to count.
            max_obs_time - 0.5 > md_onc_visit_int
          )
      }
    )
  )

count_mo_img <- function(
  ind,
  incl_img,
  incl_mo
) {
  img_sum <- incl_img %>%
    group_by(record_id) %>%
    summarize(
      n_img = n(),
      n_img_eval = sum(evaluated)
    )

  mo_sum <- incl_mo %>%
    group_by(record_id) %>%
    summarize(
      n_mo = n(),
      n_mo_eval = sum(evaluated)
    )

  # For now I'm just going to keep everything in the index lines data and return it with extra columns.
  rtn <- left_join(
    ind,
    img_sum,
    by = 'record_id'
  ) %>%
    left_join(
      .,
      mo_sum,
      by = 'record_id'
    )

  rtn %<>%
    replace_na(
      list(
        n_img = 0,
        n_img_eval = 0,
        n_mo = 0,
        n_mo_eval = 0
      )
    )

  rtn %<>%
    mutate(
      n_img_or_mo = n_img + n_mo,
      n_img_or_mo_eval = n_img_eval + n_mo_eval
    )

  return(rtn)
}

skel %<>%
  mutate(
    img_mo_count = purrr::pmap(
      .l = list(
        ind = dat_index_obs_wind,
        incl_img = incl_img,
        incl_mo = incl_mo
      ),
      .f = count_mo_img
    ),
    n_unobs_img = purrr::map_dbl(
      .x = img_mo_count,
      .f = \(x) x %>% filter(n_img_eval %in% 0) %>% nrow(.)
    ),
    n_unobs_mo = purrr::map_dbl(
      .x = img_mo_count,
      .f = \(x) x %>% filter(n_mo_eval %in% 0) %>% nrow(.)
    ),
    n_unobs_img_or_mo = purrr::map_dbl(
      .x = img_mo_count,
      .f = \(x) x %>% filter(n_img_or_mo_eval %in% 0) %>% nrow(.)
    )
  )

get_obs_ecdf_data <- function(
  dat
) {
  select(
    dat,
    record_id,
    `Med onc` = n_mo_eval,
    `Image` = n_img_eval,
    `Either` = n_img_or_mo_eval
  ) %>%
    pivot_longer(
      cols = -c(record_id),
      names_to = 'obs_type',
      values_to = 'n_obs'
    ) %>%
    mutate(
      obs_type = factor(obs_type, levels = c('Image', 'Med onc', 'Either'))
    )
}

plot_ecdf_data <- function(
  dat
) {
  ggplot(
    dat,
    aes(x = n_obs, color = obs_type)
  ) +
    stat_ecdf() +
    labs(
      x = "Observations per person",
      y = 'eCDF'
    ) +
    theme_bw() +
    scale_color_bmj(name = NULL) +
    theme(
      legend.position = "bottom"
    )
}

skel %<>%
  mutate(
    ecdf_dat = purrr::map(
      .x = img_mo_count,
      .f = get_obs_ecdf_data
    )
  )

# Take two extreme rows and plot them.
ecdf_plot_sum <- skel %>%
  filter(
    obs_min %in% 0 & obs_max %in% 364 | obs_min %in% 28 & obs_max %in% 91
  ) %>%
  select(obs_min, obs_max, ecdf_dat) %>%
  unnest(ecdf_dat)

ecdf_plot_sum %<>%
  mutate(
    obs_window = glue("Window: [{obs_min}, {obs_max}] days")
  )

gg_mo_img <- plot_ecdf_data(ecdf_plot_sum) +
  facet_wrap(vars(obs_window))

ggsave(
  plot = gg_mo_img,
  filename = here('analysis', 'explore', 'n_obs_mo_img.png'),
  height = 4,
  width = 8
)

skel %>%
  select(obs_min, obs_max, n_unobs_img, n_unobs_mo, n_unobs_img_or_mo) %>%
  flextable(.) %>%
  autofit(.)

#
#
# Question from meeting:  How far are med onc reports from imaging reports?

ex_incl_img <- skel %>%
  filter(obs_min %in% 0 & obs_max %in% 1820) %>%
  pull(incl_img) %>%
  .[[1]] %>%
  # for each group of images on one day just take one for this purpose.
  group_by(record_id, image_scan_int) %>%
  slice(1) %>%
  ungroup(.)

ex_incl_mo <- skel %>%
  filter(obs_min %in% 0 & obs_max %in% 1820) %>%
  pull(incl_mo) %>%
  .[[1]] %>%
  # for med onc we only take those that are useful:
  filter(evaluated)
# may want to filter down to only evaluated med onc here.

all_pairs <- left_join(
  select(ex_incl_img, record_id, image_scan_int),
  select(ex_incl_mo, record_id, md_onc_visit_int),
  by = 'record_id',
  relationship = 'many-to-many'
)

smallest_dist <- all_pairs %>%
  mutate(
    diff = md_onc_visit_int - image_scan_int,
    abs_diff = abs(diff)
  ) %>%
  group_by(record_id, image_scan_int) %>%
  arrange(abs_diff) %>%
  slice(1) %>%
  ungroup(.)

smallest_dist %>%
  count(is.na(diff))

ggplot(smallest_dist %>% filter(diff >= -28 & diff <= 28), aes(x = diff)) +
  stat_ecdf() +
  labs(
    title = "ECDF of time differences between imaging scans and med onc visits",
    x = "Days difference (med onc - imaging)",
    y = "Cumulative proportion"
  ) +
  theme_bw() +
  scale_x_continuous(limits = c(-28, 28), breaks = seq(-28, 28, by = 7)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -7, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "blue")


all_pairs %>%
  mutate(
    diff = md_onc_visit_int - image_scan_int,
    abs_diff = abs(diff),
    within_a_week = diff >= 0 & diff <= 7
  ) %>%
  group_by(record_id, image_scan_int) %>%
  summarize(
    any_within_a_week = any(within_a_week, na.rm = T),
    .groups = 'drop'
  ) %>%
  count(any_within_a_week)
