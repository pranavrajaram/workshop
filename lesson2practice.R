library(tidyverse)
library(ggthemes)

file <- "https://raw.githubusercontent.com/pranavrajaram/workshop/main/data/nbaplayerstats2021.csv"

data <- read_csv(file)

data %>%
  ggplot() + 
  geom_histogram(aes(x = PTS), binwidth = 100, color = "purple", fill = "violet") +
  labs(title = "Point Distribution in NBA",
       x = "Points",
       y = "Count") +
  theme_minimal()

data %>%
  ggplot() + 
  geom_point(aes(x = MIN, y = PTS), color = "brown") +
  geom_text(aes(x = MIN, y = PTS, label = Player)) +
  geom_hline(aes(yintercept = mean(PTS)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(MIN)), color = "red", linetype = "dashed") +
  labs(title = "NBA Minutes Played vs Points Scored",
       x = "Mins",
       y = "Points") +
  theme_fivethirtyeight()

data %>%
  group_by(Team) %>%
  summarise(avgRebs = mean(REB)) %>% 
  ggplot() +
  geom_col(aes(x = Team, y = avgRebs, fill = Team)) +
  theme_bw() +
  labs(title = "NBA Rebounds by Team",
       x = "Team",
       y = "Rebs")
