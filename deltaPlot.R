###############
#load packages and data
###############

library(tidyverse)
library(ggpubr)
dataClean <- read.csv("0518sims.csv")

###############
#clean up and aggregation
###############

#dataClean <- data %>%
 # select(-c(Name.y, Position.y)) %>%
  #rename(name = Name.x) %>%
  #ename(rank_pre = Rank.x) %>%
  #rename(position = Position.x) %>%
  #rename(rank_post = Rank.y)

dataAggregated <- dataClean %>%
  group_by(rosterID, Strategy, pickBin) %>%
  summarise(totalPoints = sum(FantasyPoints, na.rm = TRUE))


dataClean %>% 
  group_by(rosterID, pickBin, Strategy) %>%
  summarise(total = sum(FantasyPoints, na.rm = TRUE)) %>%
  group_by(Strategy, pickBin) %>%
  summarise(mean(total))
##############
#tree graphs
##############
deltas <- dataAggregated %>%
  group_by(pickBin) %>%
  summarise(ZeroRBPoints = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$estimate[1],
            StandardPoints = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$estimate[2],
            deltaLower = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$conf.int[1],
            deltaUpper = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$conf.int[2],
            pval = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$p.value) %>%
 mutate(delta = ZeroRBPoints - StandardPoints) 


detlaPlot <- ggplot(deltas, aes(x= delta, y = pickBin, xmin = deltaLower, xmax = deltaUpper, label = round(delta, 1)))+ 
  geom_point(aes(size = 2)) + 
  geom_errorbarh(height=.1) +
  #scale_y_continuous(breaks=1:nrow(deltas), labels=deltas$pickBin) +
  labs(title="Difference in Average Points of Zero RB Strategy Compared to Standard Strategy ", 
       x="Average Points of Zero RB Rosters less Average Points of Standard Rosters", y = "Draft Position") +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        title = element_text(size = 16),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_text(hjust=-0.5, vjust=-0.5)
detlaPlot  








################
deltasbyYear <- dataAggregated %>%
  group_by(teamPick, yr) %>%
  summarise(ZeroRBPoints = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$estimate[1],
            StandardPoints = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$estimate[2],
            deltaLower = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$conf.int[1],
            deltaUpper = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$conf.int[2],
            pval = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$p.value) %>%
  mutate(teamPick = as.integer(sub(".*_", "", teamPick))) %>%
  mutate(delta = ZeroRBPoints - StandardPoints) %>%
  arrange(desc(teamPick)) 


detlaPlotYear <- ggplot(deltasbyYear, aes(x= delta, y = teamPick, xmin = deltaLower, xmax = deltaUpper, label = round(delta, 1)))+ 
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(deltas), labels=deltas$teamPick) +
  labs(title="Difference in Average Points of Zero RB Strategy Compared to Standard Strategy ", 
       x="Average Points of Zero RB Rosters less Average Points of Standard Rosters", y = "Draft Position") +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        title = element_text(size = 11),
        legend.position = "none") +
  geom_text(hjust=0.5, vjust=-0.9) +
  facet_grid(.~yr)

detlaPlotYear






