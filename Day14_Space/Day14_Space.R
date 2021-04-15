# 30DayChartChallenge Day 14 - Space

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-14/readme.md

# load packages
library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(lubridate)
library(rtrek)
library(trekcolors)
library(trekfont)
library(ggtext)

# load astronaut data
tuesdata <- tidytuesdayR::tt_load('2020-07-14')
astronauts <- tuesdata$astronauts

# load fonts
font <- c("Khan", "StarNext")
path <- system.file(paste0("fonts/", font, ".ttf"), package = "trekfont")
for(i in seq_along(font)) font_add(font[i], path[i])
font_families()
showtext_auto()

# plot
astronauts %>%  
  mutate(age = year_of_mission - year_of_birth) %>% 
  ggplot(aes(x = year_of_mission, y = age, colour = sex)) + 
  geom_point() +
  labs(title = "Space: The final frontier",
       subtitle = "These are the voyages of <span style='color:#5B1414'>male</span> and <span style='color:#AD722C'>female</span> astronauts. <br> Their continuing mission: to explore strange new worlds. <br> To seek out new life and new civilizations. <br> To boldly go where no one has gone before!",
       caption = "Source: TidyTuesday | Mariya Stavnichuk & Tatsuya Corlett",
       x = "",
       y = "Age at mission") +
  scale_color_manual(values = c("#AD722C", "#5B1414")) +
  geom_curve(aes(x = 1990, xend = 1997, y = 75, yend = 76), curvature = 0.25, size = 0.25, colour = "#1A6384", arrow = arrow(length = unit(0.025, "npc"))) +
  geom_text(aes(x = 1975, y = 77, label = "John Herschel Glenn Jr. (77)"), hjust = 0, size = 6, colour = "#1A6384") +
  theme_rtrek() +
  theme(plot.margin = margin(10,15,10,10),
        text=element_text(family="Khan", colour = "#1A6384"),
        plot.title = element_text(family = "StarNext", size = 36),
        plot.subtitle = element_markdown(lineheight = 0.2, size = 26),
        plot.caption = element_text(size = 16),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 28, colour = "#1A6384"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#0C090A"),
        plot.background = element_rect(fill = "#0C090A"),
        legend.position = "none")
  
# save plot
ggsave("Day14_Space/Day14.jpeg")
