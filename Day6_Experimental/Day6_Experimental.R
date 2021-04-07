# 30DayChartChallenge - Day 6: Experimental

library(tidyverse)
library(emojifont)
require(gridExtra)

# set seed for random number generator
set.seed(1986)

# generate data
d <- tibble(measure = c(runif(20, min = 0, max = 20),
                        runif(20, min = 10, max = 30),
                        runif(20, min = 15, max = 35),
                        runif(20, min = 0, max = 20),
                        runif(20, min = 0, max = 20),
                        runif(20, min = 0, max = 20)),
            condition = rep(c("A","B","C", "A","B","C"), each = 20),
            study_phase = rep(c("Hypothesis", "Results"), each = 60),
            emoji = rep(c("crossed_fingers", "slightly_frowning_face"), each = 60))

# plot data
# couldn't get facet_grid to work with geom_emoji, so using grid.arrange
plot1 <- d %>% 
  filter(study_phase == "Hypothesis") %>% 
  ggplot(aes(x = condition, y = measure)) +
  #facet_grid(~study_phase) +
  geom_boxplot(width = 0.5) +
  labs(title = "Hypothesis",
       x = "", y = "") +
  geom_emoji(x = 0.65, y = 33, alias = "crossed_fingers", size = 10, color = "black") + 
  ylim(0,35) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")

plot2 <- d %>% 
  filter(study_phase == "Results") %>% 
  ggplot(aes(x = condition, y = measure)) +
  geom_boxplot(width = 0.5) +
  labs(title= "Results",
       x = "", y = "") +
  geom_emoji(x = 0.65, y = 33, alias = "slightly_frowning_face", size = 10, color = "black") + 
  ylim(0,35) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
  
grid.arrange(plot1, plot2, ncol=2)
