plots <- lapply(pairs, function(i) {
  is_bottom <- grepl(paste0(",", n), i)
  is_right <- grepl(paste0(n - 1, ","), i)
  is_left <- grepl("1,", i)
  is_top <- grepl(",2", i)
  is_col <- i %in% names(df)
  rowname <- sub("^r_\\{(\\d+),.*$", "\\1", i)
  colname <- sub("^.*,(\\d+).*$", "\\1", i)
  if (!is_col) {
    p <- ggplot(df, aes(X, runif(length(x)))) +
      geom_blank()
  } else {
    p <- ggplot(df, aes(X, .data[[i]])) +
      geom_line(linewidth = 1.5, color = "gray50", lineend = "round")
  }
  p <- p + scale_y_continuous(NULL, limits = c(0, 1)) +
    scale_x_continuous(NULL, 1:10) +
    theme_classic()
  f <- quote(. ~ .)
  if (is_top) f[3] <- rowname
  if (is_right) f[2] <- colname
  if (is_top | is_right) p <- p + facet_grid(f)
  p + theme(
    axis.text.x = if (is_bottom) element_text() else element_blank(),
    axis.title.x = if (is_bottom) element_text() else element_blank(),
    axis.text.y = if (is_left) element_text() else element_blank(),
    panel.border = if (is_col) element_rect(fill = NA) else element_blank(),
    axis.ticks = if (is_col) element_line() else element_blank(),
    axis.line = element_blank()
  )
})
