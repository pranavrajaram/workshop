library(nflfastR)
library(tidyverse)

pbp <- load_pbp(2021)

best_rz_receivers <- pbp %>%
  filter(yardline_100 <= 20) %>% 
  group_by(receiver_player_name) %>%
  filter(!is.na(touchdown)) %>%
  summarise(targets = n(),
            totalTD = sum(touchdown)) %>%
  filter(!is.na(receiver_player_name)) %>%
  arrange(desc(totalTD)) %>%
  head(n = 10)

tcl <- teams_colors_logos

best_rz_teams <- pbp %>%
  filter(yardline_100 <= 20) %>% 
  group_by(posteam) %>%
  filter(!is.na(touchdown)) %>%
  summarise(plays = n(),
            totalTD = sum(touchdown)) %>%
  arrange(desc(totalTD))

best_rz_teams <- best_rz_teams %>%
  left_join(tcl, by = c("posteam" = "team_abbr"))

best_rz_teams %>%
  ggplot(aes(x = plays,
            y = totalTD)) +
  geom_point(shape = 21, color = best_rz_teams$team_color, fill = best_rz_teams$team_color2, size = 3)

roster <- fast_scraper_roster(2021)

