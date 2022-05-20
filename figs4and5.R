###############
#load packages and data
###############

library(tidyverse)
library(ggpubr)
dataClean <- read.csv("0518sims.csv")

###############
#clean up and aggregation
###############

dataAggregated <- dataClean %>%
  group_by(rosterID, Strategy, pickBin) %>%
  summarise(totalPoints = sum(FantasyPoints, na.rm = TRUE))

dataAggregatedyr <- dataClean %>%
  group_by(rosterID, Strategy, pickBin, yr) %>%
  summarise(totalPoints = sum(FantasyPoints, na.rm = TRUE))

##############
#tree graph
##############
dataAggregated <- dataClean %>%
  group_by(rosterID, Strategy, pickBin) %>%
  summarise(totalPoints = sum(FantasyPoints, na.rm = TRUE))

deltas <- dataAggregated %>%
  group_by(pickBin) %>%
  summarise(ZeroRBPoints = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$estimate[1],
            StandardPoints = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$estimate[2],
            deltaLower = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$conf.int[1],
            deltaUpper = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$conf.int[2],
            pval = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$p.value) %>%
  mutate(delta = ZeroRBPoints - StandardPoints) %>%
  mutate(pickBin = factor(pickBin, levels = c("Late (Picks 8-10)", "Mid (Picks 4-7)", "Early (Picks 1-3)")))


fig4 <- ggplot(deltas, aes(x= delta, y = pickBin, xmin = deltaLower, xmax = deltaUpper, label = round(delta, 1)))+ 
  geom_point(aes(size = 2)) + 
  geom_errorbarh(height=.1) +
  #scale_y_continuous(breaks=1:nrow(deltas), labels=deltas$pickBin) +
  labs(title="fig. 4 'Zero RB' rosters perform worse than 'Standard' rosters by a statistically significant margin accross draft positions", 
       x="Average Points of Zero RB Rosters less Average Points of Standard Rosters", y = "Draft Position") +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        title = element_text(size = 12),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_text(hjust=-0.5, vjust=-0.5)
fig4  

##############
#tree graph by year
##############
deltasbyYear <- dataAggregatedyr %>%
  group_by(pickBin, yr) %>%
  summarise(ZeroRBPoints = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$estimate[1],
            StandardPoints = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$estimate[2],
            deltaLower = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$conf.int[1],
            deltaUpper = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$conf.int[2],
            pval = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$p.value) %>%
  mutate(delta = ZeroRBPoints - StandardPoints) %>%
  mutate(pickBin = factor(pickBin, levels = c("Late (Picks 8-10)", "Mid (Picks 4-7)","Early (Picks 1-3)")))

fig5 <- ggplot(deltasbyYear, aes(x= delta, y = pickBin, xmin = deltaLower, xmax = deltaUpper, label = round(delta, 1)))+ 
  geom_point() + 
  geom_errorbarh(height=.1) +
  labs(title="fig. 5 The 'Zero RB' Strategy worked quite well in 2018, and for late draft positions in 2020", 
       x="Average Points of Zero RB Rosters less Average Points of Standard Rosters", y = "Draft Position") +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        title = element_text(size = 11),
        legend.position = "none") +
  geom_text(hjust=0.5, vjust=-0.9) +
  facet_grid(.~yr) 

fig5






