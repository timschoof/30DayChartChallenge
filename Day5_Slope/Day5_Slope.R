# 30DayChartChallenge - Day 5 Slopes
# Projected incidence of hearing loss 2019-2050 according to the WHO

# load packages
library(tidyverse)
library(ggtext)
library(showtext)

# install font
font_add_google("Roboto", "roboto")
showtext_auto()

# data from https://www.who.int/publications/i/item/world-report-on-hearing
HL <- tibble(year = rep(c(2019, 2030, 2040, 2050), times = 6),
             region = c(rep("Eastern Mediterranean", times = 4),
                        rep("European Region", times = 4), 
                        rep("Americas", times = 4), 
                        rep("South-East Asia", times = 4), 
                        rep("Western Pacific", times = 4), 
                        rep("African Region", times = 4)), 
             incidence = c(78, 115, 152, 194, 197, 213, 228, 236, 217, 255, 292, 322, 401, 479, 573, 666, 546, 656, 731, 766, 137, 185, 251, 338)) %>% 
  group_by(region) %>% 
  mutate(ylab_pos = if_else(region == "Americas", max(incidence)-25, max(incidence)))

# select colours from pomological palette
# scales::show_col(ggpomological:::pomological_palette)
select_colours <- c("#c03728", "#919c4c", "#fd8f24", "#f5c04a", "#e68c7c", "#c3c377")

# plot the data
HL %>% 
  ggplot(aes(x = year, y = incidence, colour = region, label = region)) + 
  geom_line(lwd = 1) + 
labs(title = "Projected increase in prevalence of all grades \n of hearing loss in WHO regions", 
     subtitle = "Worldwide, nearly <span style='color:#000000'>1 in 4</span> people is expected to have some <br> degree of hearing loss by 2050. The largest increase will <br> likely be in the <span style='color:#fd8f24'>Eastern Mediterranean</span> and <span style='color:#c03728'>African</span> regions, <br> where numbers are projected to double by 2050. The <br> <span style='color:#c3c377'>Western Pacific</span> and <span style='color:#e68c7c'>South-East Asia</span> regions are expected <br> to see the highest number of people with hearing loss. <br>", 
     caption = "World Report on Hearing, WHO 2021") + 
  xlab("") +
  ylab("") +
  coord_cartesian(xlim = c(2018, 2051), ylim = c(0, 800), clip ="off")+
  geom_richtext(aes(x = 2051, y = ylab_pos), 
                hjust = 0,
                size = 3,
                fill = NA, label.color = NA,
                label.padding = grid::unit(rep(0, 6), "pt")) +
  geom_richtext(aes(x = 2012, y = 850, text.colour = "#808080",
                label = "Number of people in millions"),
                hjust = 0,
                
                size = 3,
                fill = NA, label.color = NA,
                label.padding = grid::unit(rep(0, 6), "pt"))+
  scale_color_manual(values = select_colours) + 
  theme_minimal() + 
  theme(text=element_text(family="roboto"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(colour = "#808080"),
        plot.caption = element_text(hjust = 3, size = 8, face = "italic"),
        legend.position="none",
        plot.margin = unit(c(1,6,1,1), "lines"))
