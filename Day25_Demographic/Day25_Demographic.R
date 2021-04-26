# 30DayChartChallenge Day 25 - Demographic

# load packages
library(tidyverse)

# load data 
households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')

# prep data for plotting
d <- households %>% 
  mutate(NumberOfHouseholds = NumberOfHouseholds/10^6,
         Population = Population/10^6)

# plot data
d %>% 
  filter(!County == "Kenya") %>% 
  ggplot(aes(x = Population, y = NumberOfHouseholds, colour = AverageHouseholdSize)) +
  geom_point() +
  geom_smooth(method = "gam", colour = "#555555") +
  scale_colour_viridis_c(name = "Average household size") +
  labs(y = "Households (in millions)",
       x = "Population (in millions)",
       title = "Households in Kenya's Counties",
       subtitle = "The relationship between the total population and \n number of households in Kenya's counties in 2019",
       caption = "Data: {rKenyaCensus} package") +
  theme_minimal() +
  theme(legend.position = "bottom")

# save plot
ggsave("Day25_Demographic/Day25.jpeg")
