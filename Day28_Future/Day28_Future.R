# 30DayChartChallenge Day 27 - Educational
# data drawn using http://robertgrantstats.co.uk/drawmydata.html

# load packages
library(tidyverse)
library(gganimate)
library(showtext)

# install font
font_add_google("Krona One", "krona")
font_add_google("Roboto", "roboto")
showtext_auto()

# load data 
d <- read.csv('Day28_Future/drawn_data.csv', header = TRUE)

# reorder grouping variable (dataset)
d <- d %>% 
  mutate(dataset = factor(dataset, levels = c("empty", "up", "down", "cloud", "shrug")))

# plot data
d %>% 
  ggplot(aes(x = x, y = y, group = dataset)) +
  geom_point(size = 2) +
  ylim(0,100) +
  scale_x_continuous(limits = c(0,100), breaks = c(0, 25, 50, 75, 100), labels = c(2021, 2031, 2041, 2051, 2061)) +
  coord_fixed(clip = "off") +
  labs(title = "What does the future hold?",
       x = "",
       y = "") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(family = "krona", size = 16, hjust = 0.5, face = "bold"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.background = element_rect(fill = "#ffffff"),
        axis.text = element_text(family = "roboto", size = 16)) + 
  transition_states(dataset, state_length = 1) + 
  enter_fade() + 
  exit_fade()

# save gif
anim_save("Day28_Future/Day28.gif")
