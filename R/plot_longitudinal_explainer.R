plot_longitudinal_explainer <- function(
  dat,
  pal = c('gray90', '#7777aa'),
  title = NULL
) {
  gg <- ggplot(
    dat,
    aes(x = x, y = site, fill = cancer)
  ) +
    geom_tile(height = 1, width = 1, color = "gray20", linewidth = 0.5) +
    scale_fill_manual(values = pal) +
    scale_x_continuous(
      breaks = c(1, x_pos_after),
      labels = c("Before", "After"),
      position = 'top'
    ) +
    coord_fixed() +
    theme_void() +
    theme(
      axis.text.x = element_text(),
      axis.text.y = element_text(hjust = 1),
      legend.position = 'none'
    ) +
    annotate(
      geom = 'arrow',
      x = c(1.6, x_pos_after - 0.6),
      y = c(4.5, 4.5)
    )

  if (!is.null(title)) {
    gg <- gg +
      labs(title = title) +
      theme(
        title = element_text(face = 'bold'),
        plot.title.position = 'plot'
      )
  }

  return(gg)
}
