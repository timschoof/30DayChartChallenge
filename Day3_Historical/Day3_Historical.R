# 30DayChartChallenge - Day 3: historical
# DuBoisChallenge

# load packages
library(dplyr)
library(glue)
library(ggplot2)
library(ggtext)
library(showtext)

# download Jefferies font https://www.1001freefonts.com/jefferies.font
# using custom fonts: https://r-coder.com/custom-fonts-r/
font_add(family = "jefferies", regular = "Jefferies.ttf")
showtext_auto()

# load Tidy Tuesday W.E.B. DuBois data
furniture <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/furniture.csv')

# calculate slope based on year 1899
slope <- (2-2)/ (0-1434975) # (y - yend) / (x - Dollar)

# some cleaning
furniture2 <- furniture %>% 
  rename("Dollars" = 2) %>% 
  mutate(Year = as.factor(Year),
         label = glue("{Year} --- ${Dollars}"),
         y = seq(14, by = -2, length.out = 6),
         yend = y + 10*slope*Dollars)

# create spiral plot
furniture2 %>% 
  ggplot(aes(xend = Dollars, x = 1, y = y, yend = yend, colour = Year)) +
  geom_richtext(aes(x = 1, y = y, label = label), 
                hjust = 1.1,
                size = 3,
                fill = NA, label.color = NA,
                label.padding = grid::unit(rep(0, 4), "pt"))+
  geom_segment(size=3) +
  scale_color_manual(values = c("#ebafa5", "#a8abb8", "#d2ae94", "#f3b32f", "#dbc9b8", "#d74051"))+
  coord_polar(clip = "off") +
  ylim(-10, 20)+
  xlim(0,1600000)+ #1434975
  labs(title = "ASSESSED VALUE OF HOUSEHOLD AND KITCHEN \n FURNITURE OWNED BY GEORGIA NEGROES.") +
  theme_void()+
  theme(text=element_text(size=10.5,family="jefferies"),
        panel.background = element_rect(fill = "#e4d2c1", colour = NA),
        plot.background = element_rect(fill = "#e4d2c1", colour = NA),
        legend.position="none")

# save plot
# ggsave("Day3_Historical/Day3.jpeg") 
  