# 30DayChartChallenge Day 9 - Statistics
# Comparing rainy days in Seattle and London

# packages
library(tidyverse)
library(ggdist)
library(distributional)
library(ggtext)

# load Seattle rain data 
# source: https://www.kaggle.com/rtatman/did-it-rain-in-seattle-19482017
kaggle_data <- read.csv("Day9_Statistics/seattleWeather_1948-2017.csv", header = TRUE)

# add month column
# lm() gave some weird labels with lubridate::month. Didn't want to spend too much time on this, so expect some silly hacks
seattle <- kaggle_data %>% 
  mutate(DATE = lubridate::ymd(DATE),
         month = as.character(lubridate::month(DATE, label = TRUE)))

# Create tibble with London rain data
# Make sure London precipitation data is in the same units as Seattle data
# convert mm to inches, and monthly average to daily average
# source: https://www.metoffice.gov.uk/research/climate/maps-and-data/uk-climate-averages/gcpsvg3nc
london <- tibble(month = c("Dec", "Nov", "Oct","Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan"), 
                 precipitation = c(55.2, 59, 68.5, 49.1, 49.5, 44.5, 45.1, 49.4, 43.7, 41.6, 40.9, 55.2)) %>% 
  mutate(precipitation = precipitation * 0.039370,
         day_month = c(31,28,31,30,31,30,31,31,30,31,30,31),
         precipitation = precipitation/day_month) %>% 
  select(-day_month)

# run simple linear regression - this is the "Statistics" part for Day 9 ;-)
rain_model <- lm(PRCP ~ month, data = seattle)

# prep the data for plotting
# following https://mjskay.github.io/ggdist/articles/freq-uncertainty-vis.html
d <- seattle %>%
  modelr::data_grid(month) %>%
  broom::augment(rain_model, newdata = ., se_fit = TRUE) %>%
  # add London data
  left_join(london) %>% 
  rename("london" = "precipitation") %>% 
  mutate(month = fct_relevel(as.factor(month), "Dec", "Nov", "Oct","Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan"))

# plot
# Seattle-Tacoma International Airport: 1948 - 2017
# Heathrow: 1981-2010
d %>% 
  ggplot(aes(y = month)) + 
  stat_dist_dots(
    aes(dist = dist_student_t(df = df.residual(rain_model), mu = .fitted, sigma = .se.fit), colour = stat(x <0.062)),
    quantiles = 80,
    side = "bottom") +
  geom_point(aes(london), alpha = 0.3) +
  labs(title = "It rains a lot more in Seattle than in London!",
       subtitle = "Distribution of monthly precipitation in Seattle compared <br> to the average monthly precipitation in **<span style='color:darkgray'>London</span>**. It rains <br> **<span style='color:#ADD8E6'>less</span>** in Seattle in summer, but a lot **<span style='color:#50A6C2'>more</span>** the rest of the <br> year. Who knew?!",
       caption = "*Data: kaggle/NOAA, Met Office*",
       x = "Precipitation (inches)",
       y = "") +
  scale_colour_manual(values = c("#50A6C2", "#ADD8E6")) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(legend.position = "none",
        plot.subtitle = element_markdown(),
        plot.caption = element_markdown(size = 8),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# save plot
ggsave("Day9_Statistics/Day9.jpeg") 
