# 30DayChartChallenge Day 17 - Pop Culture

# load packages
library(tidyverse)
library(showtext)
library(ggtext)

# load data
# https://www.kaggle.com/fmejia21/demographics-of-academy-awards-oscars-winners?select=Oscars-demographics-DFE.csv
oscar_data <- read.csv("Day17_PopCulture/Oscars-demographics-DFE.csv", header = TRUE)

# load font
font_add_google("Raleway", "raleway")
showtext_auto()

# prep data
d <- oscar_data %>% 
  select(year_of_award, date_of_birth, award, person) %>% 
  filter(award %in% c("Best Actress", "Best Actor")) %>% 
  mutate(sex = ifelse(award == "Best Actress", "Female", "Male"),
         dob = lubridate::year(lubridate::dmy(date_of_birth)),
         # fix dob coded in the 21st century when in fact they were in the 20th century
         dob = ifelse(year_of_award < dob, dob - 100, dob), 
         age = year_of_award - dob)

# plot data
d %>% 
  ggplot(aes(x = year_of_award, y = age, colour = as.factor(sex))) +
  geom_point() +
  labs(title = "And the Oscar goes to...",
       subtitle = "<span style='color:#3c3030'>**Older men**</span> and <span style='color:#dfca4e'>**younger women**</span> <br> in the category Best Actor/Actress, 1928-2014",
       x = "",
       y = "Age at award",
       caption = "Source: kaggle") +
  scale_colour_manual(values = c("#dfca4e", "#3c3030")) +
  xlim(1925, 2018) + ylim(20,85) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "raleway", colour = "darkgray"),
        axis.text.x = element_text(family = "raleway", colour = "darkgray"),
        axis.text.y = element_text(family = "raleway", colour = "darkgray"),
        plot.title = element_text(size = 36),
        plot.subtitle = element_markdown(size = 26, lineheight = 0.3),
        plot.caption = element_text(size = 20, face = "italic"),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 28),
        panel.grid = element_blank())

# save plot
ggsave("Day17_PopCulture/Day17.jpeg")
