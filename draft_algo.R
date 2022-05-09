#https://fantasydata.com/nfl/adp?season=2021&leaguetype=1



#####################
#importing data
#####################
library(tidyverse)
library(ggridges)
twentytwenty <- read.csv("standard_scoring_adp/adp_2021.csv")
twentytwentyPoints <- read.csv("standard_points_scored/2021.csv")

###############
#simulate drafts with standard strategy
###############
set.seed(5)

NumberOfSimulations <- 1000

teams <- c("team_1", "team_2", "team_3", "team_4", "team_5", "team_6", "team_7",
           "team_8", "team_9", "team_10")
draftPick <- c(1:160)
round <- rep(c(1:16),each=10)
teamPick <- rep(c(teams, rev(teams)), times =8)
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

allSims <- c()
for (sim in 1:NumberOfSimulations) {
  roster <- c()
  draftOrder <- data.frame(draftPick, round, teamPick) %>% 
    add_column(Rank = NA) %>%
    add_column(PlayerID = NA) %>%
    add_column(Name = NA) %>%
    add_column(Position = NA) 
  remainingPlayers <- twentytwenty %>% select(c("Rank", "PlayerID", "Name", "Position"))
  remainingPlayersPosFilter <-remainingPlayers
  positionTable <- c()
  for (val in draftPick) {
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
  allSims <- rbind(allSims, draftOrder)
}

allSimsClean <- allSims %>% 
  mutate(simulation = rep(1:NumberOfSimulations,each=160)) %>%
  left_join(twentytwentyPoints, by = c("PlayerID" = "PlayerID"))%>%
  group_by(teamPick, simulation) %>%
  summarise(totalPoints = sum(FantasyPoints, na.rm = TRUE)) %>%
  mutate(Strategy = "Standard")


#ggplot(allSimsClean, aes(x=totalPoints, y = teamPick))+
 # geom_density_ridges(scale = 0.9) #+
 # facet_wrap(~teamPick, nrow=3, ncol=4)

###############
#simulate drafts with no RB strategy
###############
ZeroTeam <- "team_1"

allSimsallPicks0RB <- c()
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
    remainingPlayers <- twentytwenty %>% select(c("Rank", "PlayerID", "Name", "Position"))
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
    allSims0RB <- rbind(allSims0RB, draftOrder)
  }
allSimsallPicks0RB <- rbind(allSimsallPicks0RB, allSims0RB)
}

SimColoneRound <- rep(1:NumberOfSimulations,each=160)
SimColtenRounds <- rep(SimColoneRound, times =10)
ZeroTeam <- rep(teams, each = NumberOfSimulations*160)


allSims0RBClean <- allSimsallPicks0RB %>% 
  mutate(simulation = SimColtenRounds) %>%
  mutate(ZeroRBteam = ZeroTeam)  %>%
  mutate(Strategy = ifelse(teamPick == ZeroRBteam, "Zero RB", "Standard")) %>%
  filter(Strategy == "Zero RB") %>%
  left_join(twentytwentyPoints, by = c("PlayerID" = "PlayerID"))%>%
  group_by(teamPick, simulation) %>%
  summarise(totalPoints = sum(FantasyPoints, na.rm = TRUE)) %>%
  mutate(Strategy = "Zero RB")

ConsolidatedData <- rbind(allSims0RBClean, allSimsClean)

ggplot() +
  geom_density(data = ConsolidatedData, aes(x = totalPoints, group = Strategy, fill = Strategy), alpha=.5) +
  facet_grid(teamPick~.) 

ggplot(ConsolidatedData, aes(x = totalPoints, y = teamPick, group = teamPick)) +
  geom_density_ridges()

ggplot(ConsolidatedData, aes(x = totalPoints, fill = Strategy)) +
  geom_histogram(alpha=0.3, position = 'identity') 


ggplot(ConsolidatedData, aes(x = Strategy, y = totalPoints, fill = Strategy)) +
  geom_boxplot() +
  facet_wrap(.~teamPick, nrow = 4, ncol = 3) 

  

write.csv(ConsolidatedData, "zeroRBdata.csv")

write.csv(allSims0RBClean, "allSims0RBClean.csv")
write.csv(allSimsClean, "allSimsClean")


