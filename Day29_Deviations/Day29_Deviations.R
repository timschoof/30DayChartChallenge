# 30DayChartChallenge Day 30 - Deviations

# load packages
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(emojifont)

# load font
font_add_google("Roboto", "roboto")
showtext_auto()

# load data
tuesdata <- tidytuesdayR::tt_load('2020-01-21') 
spotify_songs <- tuesdata$spotify_songs

# prep data for plotting
d <- spotify_songs %>% 
  filter(playlist_genre == "latin") %>% 
  mutate(duration = duration_ms/1000/60)

# plot data
d %>% 
  ggplot(aes(x = playlist_subgenre, y = duration)) +
  geom_violin(fill = "#821c0d") +
  geom_emoji(x = c(0.7, 2.2, 2.1, 2.8, 3.5, 4.2), y = c(5, 7, 6, 2.2, 7.2, 5), alias = "musical_note", size = 10, color = "black") +
  labs(x = "",
       y = "",
       title = "Duration of Latin tracks on Spotify (in minutes)",
       caption = "Source: Spotify | TidyTuesday") +
  theme_minimal() +
  theme(plot.title.position = "plot",
        panel.grid = element_blank(),
        text=element_text(family="roboto", size = 30))

# save plot
ggsave("Day29_Deviations/Day29.jpeg")
