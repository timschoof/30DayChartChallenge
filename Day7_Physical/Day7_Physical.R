# 30DayChartChallenge Day 7 - Physical
# Distribution of climbed and unclimbed Himalayan peaks by elevation

# load packages
library(tidytuesdayR)
library(tidyverse)
library(ggtext)

options(scipen = 999)

# get data
tuesdata <- tidytuesdayR::tt_load('2020-09-22')

peaks <- tuesdata %>% 
  pluck("peaks")

# create density plot
peaks %>% 
  ggplot(aes(x = height_metres, fill = climbing_status)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = c("#548bb0", "#84aab7")) +
  labs(title = "Himalayan peaks",
       caption = "*Data: The Himalayan Database, TidyTuesday*",
       x = "Elevation (m)",
       y = "Density") +
  coord_cartesian(expand= FALSE, clip = "off") +
  #ylim(0, 12e-04) +
  scale_y_continuous(breaks = seq(0, 12e-04, 3e-04), limits = c(0, 12e-04)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#ebecf0"),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_markdown(),
        plot.margin= margin(15, 15, 10, 15))

# save figure
ggsave("Day7_Physical/Day7.jpeg") 
