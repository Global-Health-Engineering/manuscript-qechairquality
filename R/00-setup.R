# description -------------------------------------------------------------



# packages ---------------------------------------------------------------

library(ggplot2)
library(scales)
library(gt)

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
  theme_gray(#base_family = "Arial",
             base_size = base_size) +
    # Add your favourite elements
    theme(
      axis.line = element_line(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      legend.title = element_text(size = base_size),
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
width_height_ratio = 190/275

# test

# source("R/01-data-preparation.R")

#ggplot(data = percent_exposure,
#       aes(fill = air_quality_who_annual, values = exposure_percent)) +
#  geom_waffle(color ="white", size = 0.5, n_rows = 5, na.rm = TRUE) +
#  coord_equal() +
#  labs(fill = "WHO 2021 targets (annual)") +
#  scale_fill_brewer(type = "div", palette = "RdYlBu", direction = -1) +
#  scale_x_discrete(expand=c(0,0)) +
#  scale_y_discrete(expand=c(0,0)) +
#  facet_grid(location ~ indicator, switch = "y") +
#
#  #facet_wrap(~location, ncol = 2) +
#  #theme_minimal(base_size = 14) +
#  # help from: https://stackoverflow.com/questions/34749636/left-justify-text-from-multi-line-facet-labels
#  theme(strip.text.x = element_text(hjust = 0),
#        strip.text.y = element_text(vjust = 0))
#

