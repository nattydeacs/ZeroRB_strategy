###############
#load packages and data
###############

library(tidyverse)
library(ggpubr)

dataClean <- read.csv("0518sims.csv")
NDColors <- c("#B0C0BF", "#332A21", "#64A1B4",
              "#AE8988", "#C36733", "#DD7764", 
              "#602A10")

###############
#clean up 
###############
knownTotals<- dataClean %>%
  group_by(Strategy, pickBin) %>%
  summarise(totalPoints = sum(FantasyPoints, na.rm = TRUE),
            numRoster = length(unique(rosterID))) %>%
  mutate(averagePoints = totalPoints/numRoster)

###############
#bar data overall
###############

barDataTotal <- dataClean %>%
  group_by(Strategy, pickBin, position) %>%
  summarise(totalPoints = sum(FantasyPoints, na.rm = TRUE),
            numRoster = length(unique(rosterID))) %>%
  mutate(averageRosterPoints = totalPoints/numRoster) 
  
summaryNumbers <- barDataTotal %>%
  group_by(Strategy, pickBin) %>%
  summarise(averageRosterPoints= sum(averageRosterPoints))

barPlot <- ggplot() +
  geom_bar(data=barDataTotal, aes(x = Strategy, y = averageRosterPoints, fill = position), position = "stack", stat = "identity") + 
  geom_text(data=barDataTotal, position = position_stack(vjust = 0.5), 
            aes(x = Strategy, y = averageRosterPoints, group = position, label = round(barDataTotal$averageRosterPoints, 0)), color = "white") +
  geom_text(data=summaryNumbers, aes(x = Strategy, y = averageRosterPoints, 
                                     label=round(averageRosterPoints,0)), vjust=-0.5, size = 4, fontface = "bold") +
  facet_grid(.~factor(pickBin, levels = c("Early (Picks 1-3)", "Mid (Picks 4-7)",
                                          "Late (Picks 8-10)")))+
  ggtitle("fig. 3 'Zero RB' teams do score better at the WR, QB, and TE")+
  ylab("Average Points Scored")+
  xlab("Roster Type") +
  theme_bw() +
  scale_fill_manual(values = NDColors)

barPlot


###############
#bar plot delta
###############

deltaBars <- barDataTotal %>%
  select(pickBin, position, averageRosterPoints, Strategy) %>%
  spread(Strategy, averageRosterPoints) %>%
  mutate(delta = `Zero RB` - Standard) 

totals <- deltaBars %>%
  group_by(pickBin) %>%
  summarise(delta = sum(delta)) %>%
  mutate(position = "ALL POSITIONS") %>%
  mutate(Standard = NA)  %>%
  mutate(`Zero RB` = NA) 

deltaBars <- rbind(deltaBars, totals) %>%
  mutate(position = factor(position, levels= c("ALL POSITIONS", "QB", "RB", "WR",
                                               "TE", "K", "DST")))
  
delataBarPlot <- ggplot(deltaBars, aes(x=position, y = delta, fill = delta < 0, label = round(delta))) +
  geom_bar(stat="Identity") +
  scale_fill_manual("", values = c("#64A1B4", "#DD7764"))+
  facet_grid(.~factor(pickBin, levels = c("Early (Picks 1-3)", "Mid (Picks 4-7)",
            "Late (Picks 8-10)"))) +
  theme(legend.position = "none",) +
  geom_text(size = 5, vjust = ifelse(deltaBars$delta > 0, -0.5, 1.5)) +
  ggtitle("...but gains at these positions fail to offset the loss of a premium RB") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "none")+
  ylab("Zero RB Average Points less Standard Strategy Average Points")+
  xlab("") 
delataBarPlot

##############
#tree graphs
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


detlaPlot <- ggplot(deltas, aes(x= delta, y = pickBin, xmin = deltaLower, xmax = deltaUpper, label = round(delta, 1)))+ 
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
detlaPlot  
##############
#arrange
##############

finalBar <- ggarrange(barPlot, delataBarPlot)
finalBar

finalFinalBar <- ggarrange(finalBar,barPlot , nrow = 1)
finalFinalBar


