# 30DayChartChallenge Day 12 - Strips
# beeswarm plot of honey production (by bees)

# load packages
library(tidyverse)
library(emojifont)
library(ggbeeswarm)
library(ggdark)

# load data
# https://www.kaggle.com/jessicali9530/honey-production
d <- read.csv("Day12_Strips/honeyproduction.csv", header = TRUE)

# plot
d %>% 
  filter(state %in% c("IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI")) %>% 
  mutate(state = factor(state, level = c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "MI", "IN", "OH"))) %>% 
  ggplot(aes(x = totalprod, y = state, colour = state)) +
  geom_beeswarm(groupOnX=FALSE, size = 0.5) +  
  scale_x_continuous(breaks = c(0e+00, 1e+07, 2e+07, 3e+07, 4e+07), labels = c(0, 10, 20, 30, 40)) + 
  geom_curve(aes(x = 4.3e+07, xend = 4.6e+07, y = 2, yend = 1.3), curvature = -0.25, size = 0.25, colour = "yellow", arrow = arrow(length = unit(0.025, "npc"))) +
  geom_text(aes(x = 3.9e+07, y = 2.1, label = "2010"), hjust = 0, size = 8, colour = "yellow") +
  geom_emoji(x = 4.1e+07, y = 13, alias = "bee", size = 30, color = "yellow") + 
  labs(title = "Honey production in the Midwest",
       subtitle = "1998 - 2012", 
       caption = "Data: kaggle.com/jessicali9530/honey-production",
       x = "Annual yield (in millions of pounds)", 
       y = "") +
  scale_colour_manual(values = rep("yellow", times = 12)) + 
  dark_theme_gray() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 28),
        plot.caption = element_text(size = 16))

# save plot
ggsave("Day12_Strips/Day12.jpeg")
