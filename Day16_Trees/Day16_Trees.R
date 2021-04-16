# 30DayChartChallenge Day 16 - Trees
# With more time I would make sure you could actually read all the names, lol

# load packages 
library(rtrek)
library(trekcolors)
library(trekfont)
library(tidyverse)
library(treemapify)
library(showtext)

# load fonts
font <- c("Khan", "StarNext")
path <- system.file(paste0("fonts/", font, ".ttf"), package = "trekfont")
for(i in seq_along(font)) font_add(font[i], path[i])
font_families()
showtext_auto()

# load data
scriptData <- st_transcripts()

# code blatantly copied from: https://leonawicz.github.io/rtrek/articles/ex-episode-analysis.html
pat <- "('s\\s|\\s\\(|\\sV\\.).*"
TNG <- filter(scriptData, format == "episode" & series == "TNG") %>% 
  unnest(text) %>%
  select(season, title, character, line) %>%
  mutate(character = gsub(pat, "", character)) %>%
  group_by(season, title, character) %>%
  summarize(lines = n(), words = length(unlist(strsplit(line, " "))))

# top 8 characters per season by # of spoken words 
TNG_words <- group_by(TNG, season, character) %>% 
  summarize(lines = sum(lines), words = sum(words)) %>% 
  arrange(desc(words)) %>% 
  top_n(8)

# plot
TNG_words %>% 
  ggplot(aes(area = lines, fill = as.factor(season), subgroup = character)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", alpha = 0.5, colour =
                               "black", family = "Khan", min.size = 0) +
  scale_fill_trek("lcars_2369", name = "Season") +
  labs(title = "Star Trek: The Next Generation", 
       subtitle = "The top 8 speaking roles per season by number of lines",
       caption = "Source: {rtrek} package") +
  guides(fill = guide_legend(nrow = 1)) +
  theme(text=element_text(family="Khan", colour = "#1A6384"),
        plot.title = element_text(family = "StarNext", size = 42),
        plot.subtitle = element_text(lineheight = 0.2, size = 26),
        plot.caption = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15))

# save plot
ggsave("Day16_Trees/Day16.jpeg")
