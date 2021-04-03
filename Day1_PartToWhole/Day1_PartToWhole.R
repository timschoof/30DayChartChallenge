# 30DayChartChallenge - Day 1: part-to-whole
# Waffle Chart of a Waffle Recipe

# load packages
library(ggplot2)
library(dplyr)
library(forcats)
library(waffle) # devtools::install_github("hrbrmstr/waffle")
library(ggpomological) # devtools::install_github("gadenbuie/ggpomological")

# create data frame based on this recipe:
# https://www.allrecipes.com/recipe/20513/classic-waffles/
d <- tibble(
  Ingredients = c("milk","flour", "eggs", "butter", "sugar","baking powder","salt",    "vanilla extract"), 
  grams = c(363, 240, 100, 80, 25, 19, 6, 4)
)

# create waffle plot
d %>%
  mutate(Ingredients = fct_reorder(Ingredients, grams, min)) %>% 
  ggplot(aes(fill = Ingredients, values = grams)) +
  geom_waffle(n_rows = 20, size = 0.33, colour = "white", flip = TRUE) +
  scale_fill_pomological() +
  labs(
    title = "Waffles",
    x = "",
    y = ""
  ) +
  coord_equal() +
  theme_pomological_fancy() +
  theme(panel.grid = element_blank(), axis.text=element_blank())
