#1.1
library(Lahman)
library(tidyverse)
?Teams
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(win_rate = W/G, E_per_game = E/G) %>%
  ggplot(aes(win_rate, E_per_game)) + 
  geom_point(alpha = 0.5)
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(doubles = X2B/G, triples = X3B/G) %>%
  ggplot(aes(doubles, triples)) + 
  geom_point(alpha = 0.5)

#1.2
library(Lahman)
library(tidyverse)
?Teams
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  summarize(r = cor(R_per_game, AB_per_game)) %>% 
  pull(r)
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(win_rate = W/G, E_per_game = E/G) %>%
  summarize(r = cor(win_rate, E_per_game)) %>% 
  pull(r)
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(doubles = X2B/G, triples = X3B/G) %>%
  summarize(r = cor(doubles, triples)) %>% 
  pull(r)
  