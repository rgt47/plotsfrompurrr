df <- data.frame(
  row = c("K", "r"),
  col = rep(c("Female", "Male"), each = 2),
  x = rep(1:2, each = 4),
  y = rep(1:2, each = 4)
)

# Split by facet row and col
df_split <- split(df, ~row + col)
# Order
df_split <- df_split[c("K.Female", "K.Male", "r.Female", "r.Male")]

plot_fun <- function(x, y) {
	browser()
  facet_layer <- if (grepl("Female$", y) && !grepl("^r", y)) 
    facet_grid(.~col) 
  else if (grepl("Male$", y) && !grepl("^r", y)) 
    facet_grid(row~col)
  else if (grepl("Male$", y) && grepl("^r", y)) 
    facet_grid(row~.)
  ggplot(x, aes(x, y)) +
    geom_point() +
    facet_layer
}

library(purrr)
library(ggplot2)
library(patchwork)

purrr::imap(df_split, plot_fun) %>% 
  wrap_plots() 
