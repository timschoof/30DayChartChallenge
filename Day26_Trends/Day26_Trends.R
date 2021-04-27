# 30DayChartChallenge Day 26 - Trends

# load packages
library(tidyverse)
library(ggtext)
library(showtext)

# load data
departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

# load font
font_add_google("Source Code Pro", "source")
showtext_auto()

# prep data for plotting
d <- departures %>% 
  filter(departure_code %in% c(3, 4, 5,6)) %>% 
  mutate(departure_code = as.factor(departure_code)) %>% 
  group_by(departure_code, fyear) %>% 
  count() 

# plot data
d %>% 
  ggplot(aes(y = departure_code, x = n, colour = departure_code)) +
  geom_boxplot(fill = "#fff9f5") +
  scale_colour_manual(values = c("#0d7680", "#802804", "#bda44d", "darkgray")) +
  labs(title = "CEO departures 2000 - 2018",
       subtitle = "Annual number of CEO departures due to <span style='color:darkgray'>**new career <br> opportunities**</span>, <span style='color:#bda44d'>**retirement**</span>, <span style='color:#802804'>**legal reasons**</span>, and <span style='color:#0d7680'>**job <br> performance**</span>",
       x = "",
       y = "",
       caption = "Source: Gentry et al | DataIsPlural") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        text = element_text(family = "source"),
        axis.text.y = element_blank(),
        plot.title = element_text(),
        plot.subtitle = element_markdown(lineheight = 0.5),
        plot.caption = element_text(face = "italic"),
        plot.background = element_rect(fill = "#fff9f5"),
        panel.background = element_rect(fill = "#fff9f5"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# save plot
ggsave("Day26_Trends/Day26.jpeg")
