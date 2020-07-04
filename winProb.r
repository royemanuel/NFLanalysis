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
    mutate(Correct = ifelse(sign(total_home_score - total_away_score) ==
                            sign(home_wp - .5), 1, 0))
