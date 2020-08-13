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


#2.2
library(Lahman)
library(tidyverse)
library(HistData)
data("GaltonFamilies")

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

?Teams
teams <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(R_per_game = R / G, BB_per_game = BB / G, HR_per_game = HR / G)
fit <- lm(R_per_game ~ BB_per_game + HR_per_game, data = teams)
fit

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

set.seed(1989, sample.kind="Rounding") 
library(HistData)
data("GaltonFamilies")
options(digits = 3)    

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

fit <- lm(mother ~ daughter, data = female_heights)
fit

Y_hat <- predict(fit, se.fit = TRUE)
Y_hat$fit[[1]]
female_heights$mother[[1]]

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_02

bat_9901 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb)) %>%
  select(playerID, mean_singles, mean_bb)
bat_9901
bat_9901 %>% filter(mean_singles > 0.2)
bat_9901 %>% filter(mean_bb > 0.2)
bat_990102 <- inner_join(bat_02, bat_9901)
cor(bat_990102$singles, bat_990102$mean_singles)
cor(bat_990102$bb, bat_990102$mean_bb)
bat_990102 %>% ggplot(aes(singles, mean_singles)) +
  geom_point()
bat_990102 %>% ggplot(aes(bb, mean_bb)) +
  geom_point()

fit <- lm(singles ~ mean_singles, data = bat_990102)
fit
fit <- lm(bb ~ mean_bb, data = bat_990102)
fit
