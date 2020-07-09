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

seahawks2015 <-
    wdap %>%
    filter(home_team.x == "SEA" | away_team.x == "SEA") %>%
    filter(season == 2015) %>%
    summarise(N = n(), HW = sum(home_win)) %>%
    mutate(Perc = HW / N)

build_team_season <- function(df, teamyearlist){
    toi <- list()
    for (i in 1:dim(teamyearlist)[1]){
        team <- teamyearlist[[1]][[i]]
        year <- teamyearlist[[2]][[i]]
        wdf <-
            df %>%
            ungroup() %>%
            filter(home_team.x == team | away_team.x == team) %>%
            filter(season == year) %>%
            mutate(sel_team_wp = round(ifelse(home_team.x == team,
                                              home_wp,
                                              1 - home_wp),
                                       2),
                   sel_team_win = ifelse(home_team.x == team, home_win,
                                  ifelse(home_win == 0, 1, 0))) %>%
            group_by(sel_team_wp) %>%
            summarise(N = n(), TW = sum(sel_team_win)) %>%
            mutate(Perc = TW / N,
                   Team = team,
                   Season = year)
        toi[[i]] <- wdf
    }
    bind_rows(toi)
}


build_plot <- function(df){
    pl <- ggplot(df, aes(sel_team_wp, Perc)) +
        geom_point() +
        geom_abline(slope = 1, intercept = 0)
    return(pl)
}



teams_of_interest <-
    tibble(team = c("DEN",
                    "JAX",
                    "CHI",
                    "NE",
                    "ATL",
                    "NE",
                    "KC",
                    "KC"),
           year = rep(seq(from = 2016, to = 2019, by = 1), 2))
    

bestOffandDef <- build_team_season(wdap, teams_of_interest)


## seahawks2015 <- build_team_season(wdap, "SEA", 2015)
## sh2015plot <- build_plot(seahawks2015)
## pats2016 <- build_team_season(wdap, "NE", 2016)
## pats2016plot <- build_plot(pats2016)
## pats2017 <- build_team_season(wdap, "NE", 2017)
## pats2017plot <- build_plot(pats2017)
## chiefs2018 <- build_team_season(wdap, "KC", 2018)
## kc2018plot <- build_plot(chiefs2018)
## chiefs2019 <- build_team_season(wdap, "KC", 2019)
## kc2019plot <- build_plot(chiefs2019)
## pats2019 <- 

## teams_of_interest <- bind_rows(seahawks2015,
##                                pats2016,
##                                pats2017,
##                                chiefs2018,
##                                chiefs2019)

toi_plot <-
    ggplot(bestOffandDef, aes(sel_team_wp, Perc)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    theme_bw() +
    facet_wrap(~ Season + Team, ncol = 2)
