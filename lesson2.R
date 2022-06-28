library(tidyverse)
library(ggthemes)

file <- "https://raw.githubusercontent.com/pranavrajaram/workshop/main/data/passing2021.csv"

data <- read_csv(file)

data %>%
  filter(Att > 100) %>%
  ggplot() +
  geom_histogram(aes(x = Yds), binwidth = 500, color = "mediumvioletred", fill = "lightsteelblue1") +
  labs(title = "Histogram of Passing Yards",
       subtitle = "2021 NFL Season, Binwidth = 500",
       x = "Yards",
       y = "Count") +
  theme_minimal()


data %>%
  ggplot() + 
  geom_col(aes(x = reorder(Tm, Yds), y = Yds, fill = Tm)) +
  coord_flip() +
  labs(title = "Amount of Passing Yards by NFL Team",
       subtitle = "2021 NFL Season",
       x = "Team",
       y = "Passing Yards",
       caption = "By: Pranav Rajaram")



data %>%
  filter(Att > 100) %>%
  ggplot() + 
  geom_point(aes(x = TD, y = Int), color = "black") +
  geom_hline(aes(yintercept = mean(Int)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(TD)), color = "red", linetype = "dashed") +
  geom_text(aes(x = TD, y = Int, label = Player), color = "navy") +
  geom_label(aes(x = 30, y = 12, label = "Lots of TDs, Lots of INTs")) +
  geom_label(aes(x = 30, y = 5, label = "Lots of TDs, Not Many INTs")) +
  geom_label(aes(x = 10, y = 12, label = "Not Many TDs, Lots of INTs")) +
  geom_label(aes(x = 10, y = 4, label = "Not Many TDs, Not Many INTs")) +
  labs(title = "NFL QB Touchdowns vs Interceptions",
       subtitle = "Minimum 100 Passing Attempts",
       x = "Passing Touchdowns",
       y = "Interceptions") +
  theme_fivethirtyeight()






# All the R Colors: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

