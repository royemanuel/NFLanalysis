library(tidyverse)
setwd("c:/Users/royem/Documents/NFLanalysis")
allData <- read_csv("2009-2019_NFL_PBP.csv")



PIonly <-
    allData %>%
    filter(str_detect(penalty_type, "Pass Interference")) %>%
    mutate(Penalty_team = ifelse(str_detect(penalty_type, "Offensive"),
                                 posteam,
                                 defteam))

offdefPI <-
    PIonly %>%
    group_by(Year, penalty_type) %>%
    summarise(N = n())

simplePIplot <- ggplot(offdefPI,
                       aes(Year, N,
                           color = penalty_type,
                           fill = penalty_type,
                           shape = penalty_type)) +
    geom_line() +
    theme_bw() +
    scale_x_continuous(breaks = seq(from = 2009, to = 2019, by = 1)) +
    geom_point()

ggsave(plot = simplePIplot, path = "Plots/", filename = "PI_by_year.png")

teamPI <-
    PIonly %>%
    group_by(Year, Penalty_team, penalty_type) %>%
    summarise(N = n())

teamPIplot <- ggplot(teamPI,
                       aes(Year, N,
                           color = penalty_type,
                           fill = penalty_type,
                           shape = penalty_type)) +
    geom_line() +
    theme_bw() +
    scale_x_continuous(breaks = seq(from = 2009, to = 2019, by = 1)) +
    geom_point() +
    ylim(0, 20) +
    facet_wrap(~ Penalty_team, ncol = 7) +
    theme(axis.text.x = element_text(angle=90, vjust = -.002))
ggsave(plot = teamPIplot, path = "Plots/", filename = "PI_by_team.png")          
