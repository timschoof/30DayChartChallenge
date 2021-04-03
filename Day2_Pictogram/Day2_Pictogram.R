# 30DayChartChallenge - Day 2: pictogram
# Which cats are keeping me awake?

# load packages
library(tidytuesdayR)
library(dplyr)
library(purrr)
library(ggplot)
library(ggimage)
library(showtext)
library(ggtext)

# install font
font_add_google("Bree Serif", "bree")
showtext_auto()

# load Tidy Tuesday animal crossing data
AC <- tidytuesdayR::tt_load('2020-05-05')

# select AC peppy cat villager data
cats <- pluck(AC,"villagers") %>% 
  filter(species == "cat",
         personality == "peppy") %>% 
  select(name, url) %>% 
  mutate(category = "AC") %>% 
  # add a row of data for my own cat
  rbind(c("Snufkin","Day2_Pictogram\\Snufkin.jpg", "pet")) %>% 
  # add coordinates for plotting
  mutate(x = c(1, 2, 1, 2, 1, 2),
         y = c(7, 7, 5, 5, 3, 3))

# plot
cats %>% 
  ggplot() + 
  geom_image(
    aes(x = x, y = y, image = url),
    asp = 1.5, size = 0.2, by = "height"
  ) + 
  geom_point(aes(x=x, y=y, colour = category), size=22, shape=0, stroke = 2) + 
  scale_color_manual(values = c("AC" = "#bc833c", "pet" = "#1b9693")) + 
  xlim(-0.5,3.5) + ylim(2,8) +
  labs(
    title = "<span style='color:#bc833c'>Cats</span> keeping me up until 1 am",
    subtitle = "<span style='color:#1b9693'>Cats</span> waking me up at 4 am"
  ) +
  theme_void()+
  theme(text=element_text(size=16,family="bree"),
        plot.title = element_markdown(size = 12, hjust = 0.5),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5),
        legend.position="none")
  