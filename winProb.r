library("tidyverse")
allplay <- read_csv("2009-2019_NFL_PBP.csv")

allplay <-
    allplay %>%
    rowid_to_column(.) 


endQuarter <-
    allplay %>%
    filter(quarter_end == 1)

lastPlayQrtr <- allplay[endQuarter$rowid - 1,]

seasonPath <- "../nflscrapR-data/games_data/regular_season/"
seasonFiles <- list.files(path = seasonPath)

seasonList <- list()
for (i in seq_along(seasonFiles)) {
    seasonList[[i]] <- read_csv(paste0(seasonPath, seasonFiles[[i]]))
    seasonList[[i]]$Year <- 2008 + i
}

seasonData <- bind_rows(seasonList)

wd <-
    lastPlayQrtr %>%
    left_join(., seasonData, by = "game_id") %>%
    select(game_id, home_team.x, away_team.x, total_home_score, total_away_score,
           home_wp, away_wp, home_score, away_score, week, qtr, season)

## Now I need to think about how this will work. If I have .4 chance winning prob,
## and I win, what does that say about the model? 

wd <-
    wd %>%
    mutate(home_win = ifelse(home_score >= away_score, 1, 0))

wdap <-
    allplay %>%
    left_join(., seasonData, by = "game_id") %>%
    mutate(home_win = ifelse(home_score >= away_score, 1, 0)) %>%
    select(game_id, home_team.x, away_team.x, home_wp, home_win, season) %>%
    filter(!is.na(home_wp)) %>%
    mutate(home_wp = round(home_wp, digits = 2)) %>%
    group_by(home_wp)

wdapsum <-
    wdap %>%
    summarise(N = n(), HW = sum(home_win)) %>%
    mutate(Perc = HW / N)

allPlot <- ggplot(wdapsum, aes(home_wp, Perc)) +
    geom_point() +
    geom_abline(slope= 1, intercept = 0) 

wdapSsn <-
    allplay %>%
    left_join(., seasonData, by = "game_id") %>%
    mutate(home_win = ifelse(home_score >= away_score, 1, 0)) %>%
    select(game_id, home_team.x, away_team.x, home_wp, home_win, season) %>%
    filter(!is.na(home_wp)) %>%
    mutate(home_wp = round(home_wp, digits = 2)) %>%
    group_by(season, home_wp)

wdapsumSsn <-
    wdapSsn %>%
    summarise(N = n(), HW = sum(home_win)) %>%
    mutate(Perc = HW / N)

seasonPlot <- ggplot(wdapsumSsn, aes(home_wp, Perc)) +
    geom_point() +
    geom_abline(slope= 1, intercept = 0) +
    facet_wrap( ~ season, ncol = 3)

wdapTm <-
    wdapTm %>%
    ungroup() %>%
    group_by(home_team.x, home_wp)

wdapsumTm <-
    wdapTm %>%
    summarise(N = n(), HW = sum(home_win)) %>%
    mutate(Perc = HW / N)

teamPlot <- ggplot(wdapsumTm, aes(home_wp, Perc)) +
    geom_point() +
    geom_abline(slope= 1, intercept = 0) +
    facet_wrap( ~ home_team.x, ncol = 3)
