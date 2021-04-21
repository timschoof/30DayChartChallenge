# 30DayChartChallenge Day 20: Upwards

# load packages
library(tidyverse)
library(ggthemes)

# load data
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

# compute percentages
d <- movies %>%
  mutate(year_group = if_else(year < 1975, "1970- \n '74",
                              if_else(year < 1980, "1975- \n '79",
                                      if_else(year < 1985, "1980- \n '84",
                                              if_else(year < 1990, "1985- \n '89",
                                                      if_else(year < 1995, "1990- \n '94", if_else(year < 2000, "1995- \n '99",
                                                                                               if_else(year < 2005, "2000- \n '04",
                                                                                                       if_else(year < 2010, "2005- \n '09", "2010- \n '13"))))))))) %>% 
  group_by(year_group, clean_test) %>%
  summarise(count_test = length(clean_test)) %>% 
  mutate(total= sum(count_test)) %>%
  group_by(clean_test, .add=TRUE) %>%
  mutate(percent=100*count_test/total) %>% 
  select(-count_test, -total)

# order factors old school style
d$clean_test <- factor(d$clean_test, levels=c('nowomen', 'notalk','men','dubious', 'ok'))

# prep for claus wilke's second axis for labels trick
# https://datavizpyr.com/direct-labeling-with-secondary-axis-trick-ggplot2-r/
d_legend <- tibble(clean_test = c(" Passes \n Bechdel \n Test", " Dubious", " Women only \n talk about men", " Women don't \n talk to each \n other", " Fewer than \n two women"),
                   last = c(22.5,50, 60, 79, 96.5))
  

# plot data
d %>% 
  ggplot(aes(x = year_group, y = percent, fill = clean_test)) +
  geom_bar(stat = "identity", colour = "white") +
  scale_fill_manual(values = c("#ff2600", "#ff937f", "#ffcac0", "#6ab2d5","#008fd5")) +
  scale_x_discrete(breaks = c("1970- \n '74", "1980- \n '84","1990- \n '94","2000- \n '04","2010- \n '13")) +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0,0),
                     sec.axis = dup_axis(breaks = d_legend$last,
                                         labels = d_legend$clean_test,
                                         name = NULL)) +
  labs(title = "The Bechdel Test Over Time",
       subtitle = "How women are represented in movies",
       caption = "Source: bechdeltest.com | fivethirtyeight.com") +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        axis.text = element_text(size = 8),
        plot.caption = element_text(size = 8))

ggsave("Day20_Upwards/Day20.jpeg")
