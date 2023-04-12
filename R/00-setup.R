# description -------------------------------------------------------------



# packages ---------------------------------------------------------------

library(ggplot2)
library(scales)

# theme adapted from:
# https://github.com/teunbrand/ggplot_tricks
# https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r

axis_title_just = "rt"
xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

base_size = 7
axis_title_size = 6

theme_set(
  # Pick a starting theme
  theme_gray(base_family = "Arial",
             base_size = base_size) +
    # Add your favourite elements
    theme(
      axis.line = element_line(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title=element_text(size=axis_title_size),
      #axis.title.x=element_text(hjust=xj),
      #axis.title.y=element_text(hjust=yj),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line("#cccccc", linewidth = 0.25),
      legend.key = element_rect(fill = NA),
      plot.margin = margin(2, 2, 2, 2)
    )
)

# paramaters for ggsave

param_dpi = 1000
param_width_single = 90
param_width_double = 190

# test

source("R/01-data-preparation.R")

dat_in_sum_day |>
  ggplot(aes(x = date, y = mean, group = indicator, color = indicator)) +
  geom_point(size = 0.8) +
  geom_line() +
  labs(x = NULL) +
  ylab(bquote(uq/m^3)) +
  facet_wrap(~location, ncol = 4) +
  scale_y_continuous(limits = c(0, 400)) +
  scale_color_brewer(type = "qual", palette = 2)




