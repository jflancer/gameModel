# Adjusting predictions by team rest ***not in use
# Back to back results
# Fitting multiplier by different types of games
new_df <- team_data %>%
  mutate(season = gameId %/%1000000) %>%
  ungroup() %>%
  select(season,teamAbbrev, gameDate, wins, gameLocationCode, gameId) %>%
  group_by(teamAbbrev,season) %>%
  arrange(gameDate) %>%
  mutate(lastG = as.numeric(round(difftime(gameDate,lag(gameDate), units = "days")))
         ) %>%
  ungroup()
away_df <- filter(new_df, gameLocationCode == "R")
back2back <- new_df %>%
  filter(gameLocationCode == "H") %>%
  left_join(away_df,
            by = c('season','gameDate','gameId'),
            suffix = c('_H','_A')) %>%
  select(lastG_H,lastG_A, wins_H) %>%
  filter(lastG_H <7, lastG_A < 7)

xb2b <- glm(wins_H~lastG_H+lastG_A, data = back2back, family = 'binomial')
back2back$pred <- predict(xb2b, back2back, type = 'response')
b2b_results <- back2back %>%
  group_by(lastG_H,lastG_A) %>%
  summarise(winP = sum(wins_H)/n(),
            xwP = mean(pred),
            count = n()
            )
home_rest <- expand.grid(lastG_H = 1:6, lastG_A = 1:6)
home_rest$xwP <- predict(xb2b, home_rest, type = "response")