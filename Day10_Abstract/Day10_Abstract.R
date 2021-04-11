# 30DayChartChallenge Day 10: Abstract
# Abstract representation of (the distribution of dots on a) shower curtain
# http://mfviz.com/r-image-art/

# load packages
library(tidyverse)
library(imager)

# load image
img <- load.image(file = "Day10_Abstract/shower_curtain.jpg")

# turn image into data frame
img_df <- as.data.frame(img)

# Reorganize the data frame, create RGB colour column
img_wide <- img_df %>%
  mutate(channel = case_when(cc == 1 ~ "Red",
                             cc == 2 ~ "Green", 
                             cc == 3 ~ "Blue")) %>% 
  select(x, y, channel, value) %>%
  spread(key = channel, value = value) %>%
  mutate(colour = rgb(Red, Green, Blue)) 

# take a sample of data points to plot
sample_size <- 1500
img_sample <- img_wide[sample(nrow(img_wide), sample_size), ]

# plot
img_sample %>% 
  ggplot(aes(x = y, y = x, color = colour, size = rev(Blue))) +
  geom_point(alpha = 0.7) +
  scale_color_identity() +
  scale_y_reverse(labels = c(72, 54, 36, 18, 0)) +
  scale_x_reverse(position = "top", labels = c(72, 48, 24, 0)) +
  labs(x = "Distance from shower head (in)",
       y = "Distance from floor (in)") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# save plot
ggsave("Day10_Abstract/Day10.jpeg", width = 10, height = 14, units = "cm") 
