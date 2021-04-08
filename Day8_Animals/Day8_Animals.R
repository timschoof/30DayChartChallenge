# 30DayChartChallenge - Day 8: Animals
# Just a good excuse to finally play with the palmerpenguins package

# load packages
library(palmerpenguins)
library(tidyverse)
library(ggtext)
library(patchwork)
library(ggridges)

# load data
d <- palmerpenguins::penguins

# scatter plot + histograms
p1 <- d %>% 
  ggplot(aes(y = bill_depth_mm, x = bill_length_mm, colour = species)) + 
  geom_jitter(alpha = 0.5) +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4")) +
  labs(title = "Penguin bill dimensions",
       subtitle = "Bill length and depth for <span style='color:darkorange'>Adelie</span>, <span style='color:darkorchid'>Chinstrap</span>, and <span style='color:cyan4'>Gentoo</span> penguins",
       x = "Bill length (mm)",
       y = "Bill depth (mm)") +
  xlim(28,62) + ylim(13,22) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.subtitle = element_markdown(size = 10),
        axis.title = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# plot using ggridges
# bill length
p2 <- d %>% 
  mutate(species_num = as.numeric(species)) %>% 
  ggplot(aes(x = bill_length_mm, y = species_num, group = species_num, colour = species, fill = species)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4")) +
  scale_colour_manual(values = c("darkorange","darkorchid","cyan4")) +
  labs(x = "Bill length (mm)",
       y = "") +
  scale_y_continuous(breaks = c(1, 2, 3), labels = c("<span style='color:darkorange'>Adelie</span>", "<span style='color:darkorchid'>Chinstrap</span>", "<span style='color:cyan4'>Gentoo</span>")) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 10),
        axis.ticks.y = element_blank(),
        axis.text.y = element_markdown(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# bill depth
p3 <- d %>% 
  mutate(species_num = as.numeric(species)) %>% 
  ggplot(aes(x = bill_depth_mm, y = species_num, group = species_num, colour = species, fill = species)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4")) +
  scale_colour_manual(values = c("darkorange","darkorchid","cyan4")) +
  labs(x = "Bill depth (mm)",
       y = "", 
       caption = "*Source: palmerpenguins package*") +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(legend.position = "none",
        plot.caption = element_markdown(size = 8),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# combine plots into one using patchwork
# not quite happy about position of the y-axis label position for the scatterplot
p1/(p2+p3) +
  plot_layout(heights = c(3,2)) 

# save plot
ggsave("Day8_Animals/Day8.jpeg") 
