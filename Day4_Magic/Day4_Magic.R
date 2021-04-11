# 30DayChartChallenge - Day 4: magic
# Characterizing the different Harry Potter books based on relative word frequency (using weighted log odds)

# install packages
library(harrypotter) # https://github.com/bradleyboehmke/harrypotter
library(dplyr)
library(forcats)
library(tidytext)
library(tidylo)
library(ggplot2)

# read in all text data
book1 <- harrypotter::philosophers_stone
book2 <- harrypotter::chamber_of_secrets
book3 <- harrypotter::prisoner_of_azkaban
book4 <- harrypotter::goblet_of_fire
book5 <- harrypotter::order_of_the_phoenix
book6 <- harrypotter::half_blood_prince
book7 <- harrypotter::deathly_hallows

# combine into a single tibble
harry<-tibble(chapter_text = c(book1, book2, book3, book4, book5, book6, book7),
       book = as.factor(c(rep("Book 1", each = length(book1)),
                rep("Book 2", each = length(book2)),
                rep("Book 3", each = length(book3)),
                rep("Book 4", each = length(book4)),
                rep("Book 5", each = length(book5)),
                rep("Book 6", each = length(book6)),
                rep("Book 7", each = length(book7)))))

# count most frequent words per book
harry_words <- harry %>% 
  group_by(book) %>% 
  unnest_tokens(word,chapter_text) %>% 
  count(word, sort = TRUE) %>% 
  anti_join(stop_words)

# weighted log odds by book
harry_logodds <- harry_words %>%
  bind_log_odds(book, word, n) %>%
  group_by(book) %>%
  top_n(8, log_odds_weighted) %>%
  ungroup() %>% 
  mutate(highlight = ifelse(word == "harry", "yes", "no" ) )

# plot
harry_logodds %>%
  ggplot(aes(
    x = log_odds_weighted,
    y = reorder_within(word, log_odds_weighted, book),
    fill = book
  )) +
  geom_col(aes(colour = highlight), show.legend = FALSE) +
  facet_wrap(vars(book), scales = "free_y") +
  labs(
    x = "Weighted log odds", y = NULL,
    title = "Characterizing the different Harry Potter books"
  ) +
  scale_y_reordered() +
  scale_fill_manual(values = c("#d1facc", "#95d0ad", "#65aa96", "#119292", "#325960", "#1e3c58", "#543c54")) + 
  scale_colour_manual( values = c( "yes"="red", "no"="gray")) + 
  theme_minimal() + 
  theme(plot.title.position = "plot",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(hjust = 1, size = 10)
    )

# save plot
# ggsave("Day4_Magic/Day4.jpeg") 

