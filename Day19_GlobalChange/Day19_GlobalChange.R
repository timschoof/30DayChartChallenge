# 30DayChartChallenge - Day 19 Global Change
# combining data from two TidyTuesday weeks to look at soybean production

# load packages
library(tidyverse)
library(showtext)
library(patchwork)
library(ggtext)

# load data
key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')

# load font
font_add_google("Source Code Pro", "source")
showtext_auto()

# clean data
crops <- key_crop_yields %>% 
  janitor::clean_names() %>% 
  filter(entity == "World") %>% 
  select(-code, -entity) %>% 
  pivot_longer(-year, names_to = "crop", values_to = "yield") %>%   
  mutate(crop = str_remove_all(crop, "_tonnes_per_hectare"),
         crop = str_replace(crop, "_", " ")) %>% 
  filter(!is.na(yield),
         year <= 2013,
         crop %in% c("beans", "soybeans", "cocoa beans"))

soybeans <- soybean_use %>% 
  filter(entity == "World") %>% 
  select(-code, -entity) %>% 
  pivot_longer(-year, names_to = "use", values_to = "amount") %>% 
  filter(!is.na(amount)) %>% 
  mutate(amount = amount / 1000000) %>% # convert to million tonnes
  mutate(use = str_replace(use, "_", " "))

# prep for claus wilke's second axis for labels trick
# https://datavizpyr.com/direct-labeling-with-secondary-axis-trick-ggplot2-r/
crops_last <- crops %>% 
  group_by(crop) %>%
  summarize(last = dplyr::last(yield))

soybeans_last <- soybeans %>% 
  group_by(use) %>%
  summarize(last = dplyr::last(amount)) %>% 
  mutate(last = if_else(use == "animal feed", last + 15, last))

# plot data
# plot bean crop yields
p1 <- crops %>% 
  ggplot(aes(x = year, y = yield, colour = crop)) +
  geom_line(lwd = 0.8) +
  scale_colour_manual(values = c("#802804", "#bda44d", "#0d7680")) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(limits = c(0, 3),
                     expand = c(0,0),
                     sec.axis = dup_axis(breaks = crops_last$last,
                                         labels = crops_last$crop,
                                         name = NULL)) +
  labs(title = "Global crop yields (tonnes per hectare) 1961 - 2013",
       y = "",
       x = "") +
  theme(legend.position = "none",
        text = element_text(family = "source", size = 18),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"),
        axis.text = element_text(size = 16),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#fff9f5"),
        plot.background = element_rect(fill = "#fff9f5"))

# plot soybean use
p2 <- soybeans %>%  
  ggplot(aes(x = year, y = amount, colour = use)) +
  geom_line(lwd = 0.8) +
  scale_colour_manual(values = c("#802804", "#bda44d", "#0d7680")) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(limits = c(0, 250),
                     expand = c(0,0),
                     sec.axis = dup_axis(breaks = soybeans_last$last,
                                         labels = soybeans_last$use,
                                         name = NULL)) +
  labs(title = "Soybean usage (million tonnes) 1961 - 2013",
       y = "",
       x = "") +
  theme(legend.position = "none",
        text = element_text(family = "source", size = 18),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"),
        axis.text = element_text(size = 16),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#fff9f5"),
        plot.background = element_rect(fill = "#fff9f5"))

# patchwork plots together
p1 / p2 +
  plot_annotation(title = "Cool beans...",
                  subtitle = " Soybean yields have increased substantially since the 1960s. However, \n this doesn't mean we have all started eating more tofu. Over the past \n few decades there has been a dramatic increase in the use of soybeans \n for the production of vegetable oil, biofuel, and processed animal feed.",
                  caption = "#30DayChartChallenge | Tim Schoof | Data: Our World in Data / TidyTuesday",
                  theme = theme(panel.background = element_rect(fill = "#fff9f5"),
                                plot.background = element_rect(fill = "#fff9f5"),
                                plot.title = element_text(family = "source", 
                                                          face = "bold", size = 28),
                                plot.subtitle = element_text(family = "source", 
                                                             size = 19, 
                                                             lineheight = 0.3),
                                plot.caption = element_text(family = "source",
                                                             size = 12)
                                )) 

# save plot
ggsave("Day19_GlobalChange/Day19.jpeg")
