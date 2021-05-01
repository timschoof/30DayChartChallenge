# 30DayChartChallenge Day 30

# load packages
library(tidyverse)
library(rayshader)
library(av)
library(palmerpenguins)

# load data
d <- palmerpenguins::penguins

# plot data
penguin_plot <- d %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  stat_density_2d(aes(fill = stat(nlevel)), 
                  geom = "polygon",
                  n = 100,bins = 10,contour = TRUE) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Penguin bill dimensions",
       caption = "Source: palmerpenguin package",
       x = "Bill length (mm)",
       y = "Bill depth (mm)") +
  theme_minimal() +
  theme(plot.title.position = "plot",
        plot.caption = element_text(size = 10, face = "italic", hjust = 1.5),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

plot_gg(penguin_plot,multicore=TRUE,width=5,height=5,scale=250)

# save plot
render_movie("Day30_3D/Day30")
render_snapshot("Day30_3D/Day30")


