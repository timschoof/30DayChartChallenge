# 30DayChartChallenge Day 21 - Downwards

# load packages
library(tidyverse)
library(lubridate)
library(showtext)
library(ggtext)

# load data
# retrieved from https://www.tsa.gov/coronavirus/passenger-throughput on 4/21/21
data <- read.csv("Day21_Downwards/TSA_21042021.csv", header = TRUE)

# load font
font_add_google("Source Code Pro", "source")
showtext_auto()

# moving average function
ma <- function(x, n = 7){
  stats::filter(x, rep(1 / n, n), sides = 2)}

# restructure
d <- data %>%  
  mutate(Date = lubridate::mdy(Date), 
         day = format(Date, format="%m-%d")) %>% 
  select(-Date) %>% 
  pivot_longer(-day, names_to = "year", values_to = "travelers") %>% 
  mutate(year = str_extract(year, "2019|2020|2021"),
         travelers = as.numeric(str_remove_all(travelers, ","))) %>% 
  group_by(year) %>% 
  arrange(day) %>% 
  mutate(avg_travelers = ma(travelers))


# plot data
d %>% 
  ggplot(aes(x = day, y = avg_travelers, colour = year, group = year)) +
  geom_line() +
  scale_y_continuous(breaks = c(0e+00, 1e+06, 2e+06), labels = c(0, 1, 2)) +
  scale_x_discrete(breaks = c("01-15", "02-15", "03-15", "04-15", "05-15", "06-15", 
                              "07-15", "08-15", "09-15", "10-15", "11-15", "12-15"),
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov", "Dec")) +
  scale_colour_manual(values = c("#0d7680", "#802804", "#bda44d")) +
  labs(title = "The effects of COVID-19 on air travel", 
       subtitle = "7-day rolling average of airline passengers (in millions) <br> in <span style='color:#0d7680'>**2019**</span>, <span style='color:#802804'>**2020**</span>, and <span style='color:#bda44d'>**2021**</span>",
       caption = "Source: Transportation Security Administration | Graphic by: Tim Schoof",
       x = "",
       y = "") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        text = element_text(family = "source", size = 24),
        plot.title = element_text(size = 28),
        plot.subtitle = element_markdown(size = 19, lineheight = 0.5),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.background = element_rect(fill = "#fffff0"),
        panel.background = element_rect(fill = "#fffff0"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# save plot
ggsave("Day21_Downwards/Day21.jpeg")
