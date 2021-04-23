# 30DayChartChallenge Day 22 - Animation

# load packages
library(tidyverse)
library(tidytuesdayR)
library(gganimate)
library(lubridate)
library(showtext)

# load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 12)
games <- tuesdata$games

# load font
font_add_google("Roboto", "roboto")
showtext_auto()

# prep data
d <- games %>% 
  filter(gamename == "Minecraft: Story Mode - A Telltale Games Series") %>% 
  mutate(date = paste("01",month, year, sep = "-"),
         date = dmy(date))

# plot
d %>% 
  ggplot(aes(x = date, y = avg)) +
  geom_line(colour = "#825432", lwd = 1) +
  #geom_point(pch = 22, size = 3, fill = "#80c71f", colour = "#825432") +
  labs(title = "Minecraft: Story Mode - A Telltale Games Series", 
       subtitle = "Average number of simultaneous players \n",
       caption = "Source: TidyTuesday | Dataviz: Tim Schoof",
       x = "",
       y = "") +
  theme_classic() +
  theme(plot.title.position = "plot",
        #text = element_text(family = "roboto", size = 24),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12, lineheight = 0.5),
        plot.caption = element_text(size = 8, face = "italic"),
        plot.background = element_rect(fill = "#f1f1f1"),
        panel.background = element_rect(fill = "#f1f1f1"),
        axis.text = element_text(colour = "black"),
        axis.ticks.y = element_blank(),
        axis.line = element_blank()) +
  transition_reveal(along = date)

# save plot
anim_save("Day22_Animation/Day22.gif")
