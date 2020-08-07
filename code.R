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

#1.3
library(Lahman)
library(tidyverse)
set.seed(1989, sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

head(female_heights)
mothers_mean <- mean(female_heights$mother)
mothers_mean
mothers_sd <- sd(female_heights$mother)
mothers_sd
daughters_mean <- mean(female_heights$daughter)
daughters_mean
daughters_sd <- sd(female_heights$daughter)
daughters_sd
dm_cor <- cor(female_heights$mother, female_heights$daughter)
dm_cor

dm_slope <- (daughters_sd/mothers_sd)*dm_cor
dm_slope
dm_intercept <- daughters_mean - dm_slope*mothers_mean
dm_intercept

(dm_cor^2)*100

dm_intercept+dm_slope*60