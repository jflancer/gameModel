
# RUNNING THROUGH PAST SEASONS 15-25 game prior tends to perform best
# will settle on using 20 games for now 

priors <- c(1,5,10,15,20,25,50,100,1000)
nsim <- 1000
szn <- 2018
logslost <- c()

for(prior_strength in priors){
  #Generate prior for each season 
  prior_data <- league_rates %>%
    #Using previous season averagee scoring rate as prior
    mutate(Season = as.numeric(substr(Season,1,4))+1,
           priorG = G * prior_strength
    ) %>%
    rename(season = Season) %>%
    select(season, priorG)
  
  #Create posterior distribution
  posterior_data <- goal_data %>%
    left_join(prior_data, by = 'season') %>%
    mutate(alphaGF = priorG + cGF, 
           alphaGA = priorG + cGA,
           beta = prior_strength + GP
    )
  
  away_team <- posterior_data %>%
    ungroup() %>%
    filter(home_or_away == "AWAY") %>%
    select(-home_or_away)
  
  all_data <- posterior_data %>%
    ungroup() %>%
    filter(home_or_away == "HOME") %>%
    left_join(away_team, by = c('season','gameId','gameDate'), suffix = c('_H','_A')) %>%
    select(season, gameDate, gameId, team_H, team_A, GF_H, GF_A, GP_H, GP_A,
           alphaGF_H, alphaGA_H, beta_H,
           alphaGF_A, alphaGA_A, beta_A
    ) %>%
    mutate(alpha_HomeScoring = alphaGF_H + alphaGA_A,
           alpha_AwayScoring = alphaGF_A + alphaGA_H,
           Beta = beta_H + beta_A,
           isWin = (GF_H>GF_A)*1, #NOTE SHOOTOUTS COUNT AS A 0 / LOSS
           gameDate = as.Date(as.character(gameDate), format = "%Y%m%d")
    ) %>%
    select(-GP_H,-GP_A)
  
  game_dates <- all_data %>%
    select(season,team_H,gameDate) %>%
    rename('team'='team_H') %>%
    bind_rows(select(all_data,season,team_A,gameDate)%>%rename('team'='team_A')) %>%
    group_by(season,team) %>%
    arrange(gameDate) %>%
    mutate(lastG = gameDate-lag(gameDate),
           lastG = ifelse(is.na(lastG), 20, lastG)
    )
  
  all_data <- all_data %>%
    left_join(game_dates, by = c('season','gameDate','team_H'='team')) %>%
    left_join(game_dates, by = c('season','gameDate','team_A'='team'), suffix = c('_H','_A'))
  
  game_data <- filter(all_data, season==szn)
  
  #Use predictive posterior for distribution of predicted scoring rate in a games
  game_sims <- list()
  talent_sims <- list()
  game_data$hAdvtg <- predict(xb2b, game_data, type = 'response')
  game_data$hAdvtg <- ifelse(game_data$lastG_H >4 | game_data$lastG_A > 4, 0.55, game_data$hAdvtg)
  game_data$xHome <- NA
  game_data$xAway <- NA
  game_data$xOT <- NA
  game_data$wProb <- NA
  game_data$lloss <- NA
  
  for(i in 1:nrow(game_data)){
    talent_sims[[i]] <- list(HomeGF = rgamma(nsim, game_data$alphaGF_H[i], game_data$beta_H[i]),
                             HomeGA = rgamma(nsim, game_data$alphaGA_H[i], game_data$beta_H[i]),
                             AwayGF = rgamma(nsim, game_data$alphaGF_A[i], game_data$beta_A[i]),
                             AwayGA = rgamma(nsim, game_data$alphaGA_A[i], game_data$beta_A[i])
    )
    
    HomeLambdas <- mean(rgamma(nsim, game_data$alpha_HomeScoring[i], game_data$Beta[i]))
    AwayLambdas <- mean(rgamma(nsim, game_data$alpha_AwayScoring[i], game_data$Beta[i]))
    
    multiplier <- (game_data$hAdvtg[i]-0.5)/0.5
    multiplier <- 0.03
    
    HomeScores <- rpois(nsim, HomeLambdas*(1+multiplier))
    AwayScores <- rpois(nsim, AwayLambdas*(1-multiplier))
    game_sims[[i]] <- list(HomeScores,AwayScores)
    game_data$xHome[i] <- mean(HomeScores)
    game_data$xAway[i] <- mean(AwayScores)
    game_data$xOT[i] <- sum(HomeScores==AwayScores)/nsim
    game_data$wProb[i] <- (sum(HomeScores>AwayScores) + sum(HomeScores==AwayScores)*0.5)/nsim
    game_data$isWin[i] <- ifelse(game_data$GF_H[i]==game_data$GF_A[i], sample(c(0,1),1,prob = c(0.5,0.5)), game_data$isWin[i])
    game_data$lloss[i] <- -1*(game_data$isWin[i]*log(game_data$wProb[i]) + (1-game_data$isWin[i])*log(1-game_data$wProb[i]))
  }
  
  nonshootout <- game_data #%>%
    # filter(GF_H != GF_A)
  
  baseline <- -mean(nonshootout$isWin*log(0.54) + (1-nonshootout$isWin)*log(0.54))
  #log loss
  logloss <- mean(nonshootout$lloss)
  logslost <- c(logslost, logloss)
  print(paste(prior_strength,':',logloss))
}

