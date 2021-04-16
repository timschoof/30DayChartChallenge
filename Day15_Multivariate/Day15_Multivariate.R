# 30DayChartChallenge Day 14 - Multivariate
# I'm going to resist to change all the things I don't like about this graph

# load packages
library(tidytuesdayR)
library(tidyverse)
library(ggtext)

# get data
tuesdata <- tidytuesdayR::tt_load('2020-09-22')

peaks <- tuesdata %>% 
  pluck("peaks")

expeditions <- tuesdata %>% 
  pluck("expeditions")

# create some new variables, merge expeditions & peaks data
d <- expeditions %>% 
  mutate(climb_dur = as.numeric(highpoint_date - basecamp_date),
         success = if_else(termination_reason == "Success (main peak)", TRUE, FALSE),
         staff_ratio = hired_staff/(hired_staff + members)) %>% 
  left_join(peaks, by = c("peak_id", "peak_name"))

# plot
d %>% 
  filter(year == 1986) %>%
  ggplot(aes(y = climb_dur, x = height_metres, colour = success, size = staff_ratio)) + 
  geom_point(alpha = 0.5) +
  scale_size(range = c(1, 5)) +
  labs(title = "Himalayan expeditions in 1986",
       subtitle = "Duration of ascent for <span style='color:#466D1D'>successful</span> and <span style='color:#CD001A'>unsuccessful</span> <br>expeditions. The size of the bubble indicates the <br> proportion of staff members among the expedition.",
       x = "Peak's elevation (m)",
       y = "Duration of ascent (days)") +
  coord_cartesian(clip = "off") + 
  scale_colour_manual(values = c("#CD001A", "#466D1D")) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#ebecf0"),
        plot.subtitle = element_markdown(),
        legend.position = "none")

# save plot
ggsave("Day15_Multivariate/Day15.jpeg")
