# 30DayChartChallenge Day 27 - Educational

# load packages
library(tidyverse)
library(gganimate)

# load data 
datasaurus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')

# compute summary statistics
dino_stats <- datasaurus %>% #datasauRus::datasaurus_dozen %>% 
  group_by(dataset) %>% 
  summarize(
    mean_x    = mean(x),
    mean_y    = mean(y),
    std_dev_x = sd(x),
    std_dev_y = sd(y),
    corr_x_y  = cor(x, y)
  )

# merge raw data & summary stats
d <- datasaurus %>%  #datasauRus::datasaurus_dozen %>% 
  left_join(dino_stats, by = "dataset")

# plot data
d %>% 
  filter(dataset %in% c("dino", "star", "circle", "x_shape")) %>%   
  ggplot(aes(x = x, y = y, colour = dataset)) +
  geom_point(size = 2) +
  xlim(0,100) + ylim(0,100) +
  coord_fixed(clip = "off") +
  scale_colour_manual(values = c("#298fca", "#4C5F28", "#ffe12b", "#E3242B")) +
  labs(title = "Same stats, different graph",
       subtitle = "The importance of visually inspecting your data",
       caption = "Source: datasauRus package / TidyTuesday",
       x = "",
       y = "") +
  geom_text(x = 10, y = 98, label = paste("r = ", signif(d$corr_x_y[1], 3), sep = ""), colour = "black", hjust = 0.5, size = 6) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        plot.caption = element_text(size = 12, face = "italic"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.background = element_rect(fill = "#ffffff"),
        axis.text = element_text(size = 16)) + 
  transition_states(dataset, state_length = 1)

# save gif
anim_save("Day27_Educational/Day27.gif")
