# 30DayChartChallenge Day 24 - Monochrome
# Monochrome doesn't need to be black and white!

# load packages
library(tidyverse)
library(ggplot2movies)
library(ggstream)
library(showtext)

# load font
font_add_google("Schoolbell", "schoolbell")
showtext_auto()

# Make up data
d <- tibble(year=rep(seq(2491,2600), each=4),
  name=rep(letters[1:4], 110),
  value=sample(seq(0,1,0.0001), 440))

# make data more fun for plotting
d <- d %>% 
  mutate(value = if_else(name == "b" & year <2520, value*2, 
                         if_else(name == "c" & year > 2550, value/2, value)),
         value = if_else(year >2515 & year < 2525, value/2,
                         if_else(year >2550 & year < 2560, value*2, value)))

# plot data
d %>% 
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_stream() +
  scale_fill_manual(values = c("#26408B", "#3D60A7", "#81B1D5", "#A0D2E7")) +
  ylim(-1.5,1.5) +
  labs(title = "Stream Graph",
       caption = "Data: 100% fake",
       x = "",
       y = "") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#CEE5F2"),
        plot.title = element_text(hjust = 0.5, vjust = 0.1, family = "schoolbell", size = 50),
        plot.caption = element_text(family = "schoolbell", face = "italic", size = 20, hjust = 0.95))

# save plot
ggsave("Day24_Monochrome/Day24.jpeg")
