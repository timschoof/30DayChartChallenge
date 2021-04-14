# 30DayChartChallenge Day 13 - Correlation

# load packages
library(bakeoff)
library(tidyverse)
library(showtext)
library(ggtext)

# install font
font_add_google("Dancing Script", "dancing")
font_add_google("Amatic SC", "amatic")
showtext_auto()

# data
d <- bakeoff::baker_results

# plot
d %>% 
  ggplot(aes(x = percent_technical_top3, y = percent_episodes_appeared)) + 
  geom_jitter(width = 2.5, height = 2.5, colour = "#efa5c8") +
  xlim(0,100) + ylim(0,100) +
  #coord_cartesian(expand = FALSE) + 
  labs(title = "The Great British Bake Off", 
       subtitle = "Does doing well in the technical challenge keep you in the game? ",
       caption = "Data: https://github.com/apreshill/bakeoff",
       x = "Top 3 in technical challenge (%)",
       y = "Episodes appeared (%)") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(colour = "#8d91c9", face = "bold"),
        axis.title = element_text(family = "amatic", size = 28),
        axis.text = element_text(family = "amatic", colour = "#8d91c9", face = "bold", size = 28),
        axis.line = element_line(colour = "#8d91c9"),
        plot.subtitle = element_markdown(family = "dancing", size = 30),
        plot.title = element_text(family="dancing", size = 36),
        plot.caption = element_text(family = "dancing", size = 20, face = "italic")
        )

# save plot
ggsave("Day13_Correlation/Day13.jpeg")
