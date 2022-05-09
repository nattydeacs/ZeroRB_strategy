#https://fantasydata.com/nfl/adp?season=2021&leaguetype=1



#positionTable <- data.frame(table(roster$Position)) 
#positionTable <- positionTable %>% 
 # mutate(is_pos_full = mapply(full_func, positionTable$Var1, positionTable$Freq))


#####################
#importing data
#####################
library(tidyverse)
library(ggridges)


adpFiles <- paste0("standard_scoring_adp/adp_", 2017:2021, ".csv")
adp_data <- lapply(adpFiles, read.csv)
names(adp_data) <- (2017:2021)

pointsFiles <- paste0("standard_points_scored/", 2017:2021, ".csv")
points_data <- lapply(pointsFiles, read.csv)
names(points_data) <- (2017:2021)

###############
#define functions and vars
###############

set.seed(5)

NumberOfSimulations <- 2

teams <- c("team_1", "team_2", "team_3", "team_4", "team_5", "team_6", "team_7",
           "team_8", "team_9", "team_10")
years <- c("2017", "2018","2019","2020","2021")

draftPick <- c(1:160)
round <- rep(c(1:16),each=10)
teamPick <- rep(c(teams, rev(teams)), times =8)

#function to tell if roster is full at position
full_func <- function(position, count){
      if (position == "QB" & count >=1){
        return("Yes")
      }
      if (position == "RB" & count >=4){
        return("Yes")
      }
      if (position == "TE" & count >=1){
        return("Yes")
      }
      if (position == "K" & count >=1){
        return("Yes")
      }
      if (position == "DST" & count >=1){
        return("Yes")
      }
      if (position == "WR" & count >=4){
        return("Yes")
      }
      else{
        return("No")
      }
}

#function to simulate drafts using standard strategy
simulateStandardDraft <- function(year, NumberOfSimulations) {
  draftPick <- c(1:160)
  round <- rep(c(1:16),each=10)
  teamPick <- rep(c(teams, rev(teams)), times =8)
  allSimsinFunc <- c()
  for (sim in 1:NumberOfSimulations) {
    roster <- c()
    draftOrder <- data.frame(draftPick, round, teamPick) %>% 
      add_column(Rank = NA) %>%
      add_column(PlayerID = NA) %>%
      add_column(Name = NA) %>%
      add_column(Position = NA) 
    remainingPlayers <- adp_data[[year]] %>% 
      select(c("Rank", "PlayerID", "Name", "Position"))
    remainingPlayersPosFilter <-remainingPlayers
    positionTable <- c()
    for (val in draftPick) {
      if (val<=100) {
        roster <- filter(draftOrder, teamPick == draftOrder[val, "teamPick"])
        positionTable <- data.frame(table(roster$Position)) 
        positionTable <- positionTable %>% mutate(is_pos_full = mapply(full_func, positionTable$Var1, positionTable$Freq))
        fullPositions <- c(as.vector(positionTable[positionTable$is_pos_full == "Yes", 1]), "K", "DST")
        remainingPlayersPosFilter <- filter(remainingPlayers, !(Position %in% fullPositions))
        pick <- remainingPlayersPosFilter[1+round((sample(0:1, 1))*sqrt(val), 1), ]
        draftOrder[val,c(4:7)] <- pick
        remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
      }
      else if (val>=100 & val<=140) {
        roster <- filter(draftOrder, teamPick == draftOrder[val, "teamPick"])
        pick <- remainingPlayers[1+round((sample(0:1, 1))*sqrt(val), 1), ]
        draftOrder[val,c(4:7)] <- pick
        remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
      }
      else if (val>=141 & val<=150) {
        roster <- filter(draftOrder, teamPick == draftOrder[val, "teamPick"])
        if(!("DST" %in% roster$Position)) {
          DST <- filter(remainingPlayers, Position == "DST")
          pick <- DST[1, ]
          draftOrder[val,c(4:7)] <- pick
          remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
        }
        else{
          pick <- remainingPlayers[1+sample(0:2, 1), ]
          draftOrder[val,c(4:7)] <- pick
          remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
        }
      }
      else if(val>150){
        roster <- filter(draftOrder, teamPick == draftOrder[val, "teamPick"])
        if(!("K" %in% roster$Position)) {
          K <- filter(remainingPlayers, Position == "K")
          pick <- K[1, ]
          draftOrder[val,c(4:7)] <- pick
          remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
        }
        else{
          pick <- remainingPlayers[1+sample(0:2, 1), ]
          draftOrder[val,c(4:7)] <- pick
          remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
        }  
      }
    }
    allSimsinFunc <- rbind(allSimsinFunc, draftOrder %>% mutate(simulation = sim))
  }
  returnme <- allSimsinFunc %>%
    mutate(yr = year) %>%
    mutate(Strategy = "Standard") %>%
    left_join(points_data[[year]], by = c("PlayerID" = "PlayerID"))
  return(returnme)
}

#function to simulate drafts 0RB standard strategy
simulate0RBDraft <- function(year, NumberOfSimulations) {
  ZeroTeam <- "team_1"
  allSimsallPicks0RB <- c()
  draftPick <- c(1:160)
  round <- rep(c(1:16),each=10)
  teamPick <- rep(c(teams, rev(teams)), times =8)
  for (team in teams){
    ZeroTeam <- team
    allSims0RB <- c()
    for (sim in 1:NumberOfSimulations) {
      roster <- c()
      draftOrder <- data.frame(draftPick, round, teamPick) %>% 
        add_column(Rank = NA) %>%
        add_column(PlayerID = NA) %>%
        add_column(Name = NA) %>%
        add_column(Position = NA) 
      remainingPlayers <- adp_data[[year]] %>% select(c("Rank", "PlayerID", "Name", "Position"))
      remainingPlayersPosFilter <-remainingPlayers
      positionTable <- c()
      for (val in draftPick) {
        if (draftOrder[val,"teamPick"] != ZeroTeam) {
          if (val<=100) {
            roster <- filter(draftOrder, teamPick == draftOrder[val, "teamPick"])
            positionTable <- data.frame(table(roster$Position)) 
            positionTable <- positionTable %>%  mutate(is_pos_full = mapply(full_func, positionTable$Var1, positionTable$Freq))
            fullPositions <- c(as.vector(positionTable[positionTable$is_pos_full == "Yes", 1]), "K", "DST")
            remainingPlayersPosFilter <- filter(remainingPlayers, !(Position %in% fullPositions))
            pick <- remainingPlayersPosFilter[1+round((sample(0:1, 1))*sqrt(val), 1), ]
            draftOrder[val,c(4:7)] <- pick
            remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
          }
          else if (val>=100 & val<=140) {
            roster <- filter(draftOrder, teamPick == draftOrder[val, "teamPick"])
            pick <- remainingPlayers[1+round((sample(0:1, 1))*sqrt(val), 1), ]
            draftOrder[val,c(4:7)] <- pick
            remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
          }
          else if (val>=141 & val<=150) {
            roster <- filter(draftOrder, teamPick == draftOrder[val, "teamPick"])
            if(!("DST" %in% roster$Position)) {
              DST <- filter(remainingPlayers, Position == "DST")
              pick <- DST[1, ]
              draftOrder[val,c(4:7)] <- pick
              remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
            }
            else{
              pick <- remainingPlayers[1+sample(0:2, 1), ]
              draftOrder[val,c(4:7)] <- pick
              remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
            }
          }
          else if(val>150){
            roster <- filter(draftOrder, teamPick == draftOrder[val, "teamPick"])
            if(!("K" %in% roster$Position)) {
              K <- filter(remainingPlayers, Position == "K")
              pick <- K[1, ]
              draftOrder[val,c(4:7)] <- pick
              remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
            }
            else{
              pick <- remainingPlayers[1+sample(0:2, 1), ]
              draftOrder[val,c(4:7)] <- pick
              remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
            }  
          }
        }
        else if (draftOrder[val,"teamPick"] == ZeroTeam){
          if (val<=40) {
            remainingPlayersSanRB<- filter(remainingPlayers, Position !="RB")
            pick <- remainingPlayersSanRB[1+round((sample(0:1, 1))*sqrt(val), 1), ]
            draftOrder[val,c(4:7)] <- pick
            remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
          }
          else if (val>= 41 & val<=100) {
            roster <- filter(draftOrder, teamPick == draftOrder[val, "teamPick"])
            positionTable <- data.frame(table(roster$Position)) 
            positionTable <- positionTable %>%  mutate(is_pos_full = mapply(full_func, positionTable$Var1, positionTable$Freq))
            fullPositions <- c(as.vector(positionTable[positionTable$is_pos_full == "Yes", 1]), "K", "DST")
            remainingPlayersPosFilter <- filter(remainingPlayers, !(Position %in% fullPositions))
            pick <- remainingPlayersPosFilter[1+round((sample(0:1, 1))*sqrt(val), 1), ]
            draftOrder[val,c(4:7)] <- pick
            remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
          }
          else if (val>=100 & val<=140) {
            roster <- filter(draftOrder, teamPick == draftOrder[val, "teamPick"])
            pick <- remainingPlayers[1+round((sample(0:1, 1))*sqrt(val), 1), ]
            draftOrder[val,c(4:7)] <- pick
            remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
          }
          else if (val>=141 & val<=150) {
            roster <- filter(draftOrder, teamPick == draftOrder[val, "teamPick"])
            if(!("DST" %in% roster$Position)) {
              DST <- filter(remainingPlayers, Position == "DST")
              pick <- DST[1, ]
              draftOrder[val,c(4:7)] <- pick
              remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
            }
            else{
              pick <- remainingPlayers[1+sample(0:2, 1), ]
              draftOrder[val,c(4:7)] <- pick
              remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
            }
          }
          else if(val>150){
            roster <- filter(draftOrder, teamPick == draftOrder[val, "teamPick"])
            if(!("K" %in% roster$Position)) {
              K <- filter(remainingPlayers, Position == "K")
              pick <- K[1, ]
              draftOrder[val,c(4:7)] <- pick
              remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
            }
            else{
              pick <- remainingPlayers[1+sample(0:2, 1), ]
              draftOrder[val,c(4:7)] <- pick
              remainingPlayers <- filter(remainingPlayers, PlayerID != pick$PlayerID)
            }
          }
        }
      }
      allSims0RB <- rbind(allSims0RB, draftOrder %>% 
                            mutate(simulation = sim) %>%
                            mutate(ZeroTeam))
    }
    allSimsallPicks0RB <- rbind(allSimsallPicks0RB, allSims0RB)
  }
  returnValue <- allSimsallPicks0RB %>%
    mutate(yr = year) %>%
    mutate(Strategy = if_else(teamPick == ZeroTeam, "Zero RB", "Standard")) %>%
    filter(Strategy == "Zero RB") %>%
    left_join(points_data[[year]], by = c("PlayerID" = "PlayerID")) %>%
    select(-ZeroTeam)
  return(returnValue)
}


###############
#simulate drafts 
###############

standard <- c()
zero <- c()
simulations <- 10000
for(year in years){
  standard <- rbind(standard, simulateStandardDraft(year, simulations))
  zero <- rbind(zero, simulate0RBDraft(year, simulations))
}
data <- rbind(standard, zero)

###############
#clean up and aggregation
###############

dataClean <- data %>%
  select(-c(Name.y, Position.y)) %>%
  rename(name = Name.x) %>%
  rename(rank_pre = Rank.x) %>%
  rename(position = Position.x) %>%
  rename(rank_post = Rank.y)

dataAggregated <- dataClean %>%
  group_by(teamPick, Strategy, simulation, yr) %>%
  summarise(totalPoints = sum(FantasyPoints, na.rm = TRUE))



#ggplot(allSimsClean, aes(x=totalPoints, y = teamPick))+
 # geom_density_ridges(scale = 0.9) #+
 # facet_wrap(~teamPick, nrow=3, ncol=4)







