library(tidyverse)
library(modelr)

file <- "https://raw.githubusercontent.com/pranavrajaram/workshop/main/data/receiving2021.csv"

data <- read_csv(file)


linear_model <- lm(Rec ~ Tgt, data = data)

summary(linear_model)

data %>%
  ggplot() +
  geom_point(aes(x = Tgt, y = Rec)) +
  geom_abline(intercept = 6.99, slope = 0.613, color = "red")

r <- cor(data$Tgt, data$Rec)

# Model 2
linear_model2 <- lm(TD ~ Yds, data = data)

summary(linear_model2)

data %>%
  ggplot() + 
  geom_point(aes(x = Yds, y = TD)) +
  geom_abline(intercept = -1.08, slope = 0.0074, color = "red")

cor(data$Yds, data$TD)
plot(linear_model2$residuals)


# Model 3
linear_model3 <- lm(Lng ~ `1D`, data = data)

summary(linear_model3)

data %>%
  ggplot() + 
  geom_point(aes(x = `1D`, y = Lng)) +
  geom_abline(intercept = 34.27, slope = 0.355, color = "red")

cor(data$`1D`, data$Lng)




file <- "https://raw.githubusercontent.com/pranavrajaram/workshop/main/data/rushingdata.csv"

data <- read_csv(file)

fit <- lm(Yds2021 ~ Yds2020, data = data)

summary(fit)

data %>%
  ggplot() +
  geom_point(aes(x = Yds2020, y = Yds2021)) +
  geom_abline(intercept = 902.5, slope = 0.5555, color = "red")


data %>%
  add_predictions(model = fit, type = "response", var = "pred") %>%
  ggplot() + 
  geom_segment(aes(x = Yds2020, xend = Yds2020, y = Yds2021, yend = pred), color = "blue") +
  geom_point(aes(x = Yds2020, y = Yds2021)) +
  geom_abline(intercept = 902.5013, slope = 0.5555, color = "red") +
  geom_text(aes(x = Yds2020, y = Yds2021, label = Tm))
  

fit2 <- lm(TD2021 ~ TD2020, data = data)
summary(fit2)

data %>%
  ggplot() +
  geom_point(aes(x = TD2020, y = TD2021)) +
  geom_abline(intercept = 9.844, slope = 0.357, color = "red")


cor(data$TD2020, data$TD2021)

data %>%
  add_predictions(model = fit2, type = "response", var = "pred") %>% 
  ggplot() + 
  geom_point(aes(x = TD2020, y = TD2021)) +
  geom_abline(intercept = 9.844, slope = 0.357, color = "red") +
  geom_segment(aes(x = TD2020, xend = TD2020, y = TD2021, yend = pred), color = "blue")


fit3 <- lm(Att2021 ~ Att2020, data = data)
summary(fit3)

data %>%
  ggplot() + 
  geom_point(aes(x = Att2020, y = Att2021)) +
  geom_abline(intercept = 221.18, slope = 0.5377)

cor(data$Att2020, data$Att2021)

data %>%
  add_predictions(model = fit3, type = "response", var = "pred") %>% 
  ggplot() + 
  geom_point(aes(x = Att2020, y = Att2021)) +
  geom_abline(intercept = 221.18, slope = 0.5377) +
  geom_segment(aes(x = Att2020, xend = Att2020, y = Att2021, yend = pred), color = "blue") +
  geom_text(aes(x = Att2020, y = Att2021, label = Tm))

