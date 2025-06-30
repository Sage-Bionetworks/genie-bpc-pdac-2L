library(tidyverse)
library(ggarrow)
x_pos_after <- 3

skel <- expand_grid(
  x = c(1, x_pos_after),
  y = -1:-8
)

skel %<>%
  left_join(
    tibble(
      y = -1:-8,
      site = c(
        "Brain/Head",
        "Spine",
        "Neck",
        "Chest",
        "Abdomen",
        "Pelvis",
        "Extremity",
        "Full body"
      )
    ),
    by = c('y')
  ) %>%
  mutate(site = fct_rev(fct_inorder(site)))

add_cancer <- function(s_df, b, a) {
  s_df %>%
    mutate(
      cancer = case_when(
        x %in% 1 & site %in% b ~ T,
        x %in% x_pos_after & site %in% a ~ T,
        T ~ F
      )
    )
}

plot_longitudinal_explainer(
  add_cancer(
    skel,
    b = "Abdomen",
    a = c("Abdomen", "Chest")
  ),
  title = "PD example"
) %>%
  ggsave(
    plot = .,
    file = here('output', 'img', 'explainer_pd.png'),
    height = 3,
    width = 2
  )

plot_longitudinal_explainer(
  add_cancer(
    skel,
    b = c("Chest", "Abdomen", "Pelvis"),
    a = c("Chest", "Pelvis")
  ),
  title = "PR example"
) %>%
  ggsave(
    plot = .,
    file = here('output', 'img', 'explainer_pr.png'),
    height = 3,
    width = 2
  )

plot_longitudinal_explainer(
  add_cancer(
    skel,
    b = c("Chest", "Abdomen", "Spine"),
    a = c()
  ),
  title = "CR example"
) %>%
  ggsave(
    plot = .,
    file = here('output', 'img', 'explainer_cr.png'),
    height = 3,
    width = 2
  )
