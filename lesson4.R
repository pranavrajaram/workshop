library(tidyverse)
library(modelr)
library(ggthemes)
library(ggimage)

data <- read_csv("https://raw.githubusercontent.com/pranavrajaram/workshop/main/data/interceptions.csv")
## Review Exercise #1

data %>%
  ggplot() +
  geom_image(aes(x = Int2020, y = Int2021, image = team_logo_espn), asp = 16/9) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  labs(title = "NFL Team Interceptions in 2020 vs 2021",
       x = "Interceptions in 2020",
       y = "Interceptions in 2021") +
  geom_hline(aes(yintercept = mean(Int2021)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(Int2020)), color = "red", linetype = "dashed") 

ggsave("interceptionplot.png")


## Review Exercise #2

linear_model <- lm(Int2021 ~ Int2020, data = data)
summary(linear_model)

cor(data$Int2020, data$Int2021)

data %>%
  ggplot() +
  geom_point(aes(x = Int2020, y = Int2021)) +
  geom_abline(intercept = 8.25, slope = 0.4575, color = "red")

## Review Exercise #3

data %>%
  add_predictions(model = linear_model, type = "response", var = "pred") %>%
  ggplot() +
  geom_point(aes(x = Int2020, y = Int2021)) +
  geom_abline(intercept = 8.25, slope = 0.4575, color = "red") + 
  geom_segment(aes(x = Int2020, xend = Int2020, y = Int2021, yend = pred), color = "blue")
