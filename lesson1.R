# load in the tidyverse package
library(tidyverse)

# load in the data
file <- "https://raw.githubusercontent.com/pranavrajaram/workshop/main/data/passing2021.csv"

data <- read_csv(file)

# view data
data %>% View() 

# filter the data with passing yards > 1000
data %>%
  filter(Yds > 1000) %>% View()

# arrange and then apply the same filter
data %>%
  arrange(desc(TD)) %>% 
  filter(Yds > 1000) %>% View()

# select certain columns
data %>%
  select(Player, Yds, TD, Int) %>% View()

# filter data with positive interceptions, create a new column of touchdowns/interceptions, arrange data by that column
data %>%
  filter(Int > 0) %>%
  mutate(ratio = TD/Int) %>% 
  arrange(desc(ratio)) %>% View()

# groups data by team and then finds total yards 
data %>%
  group_by(Tm) %>%
  summarize(totalYards = sum(Yds)) %>% View()


# Answers to Exercises:

data %>%
  filter(Att > 300) %>% View()

data %>%
  mutate(ypg = Yds/G) %>% View()

data %>%
  group_by(Tm) %>%
  summarize(mean_int = mean(Int)) %>% View()

data %>%
  select(Player, Tm, Rate, Lng) %>% View()



