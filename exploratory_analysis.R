###############
#load packages and data
###############

library("tidyverse")
data <- read.csv("0514sims.csv")

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



##############
#delta by draft pick
##############
deltas <- dataAggregated %>%
  group_by(teamPick) %>%
  summarise(ZeroRBPoints = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$estimate[1],
            StandardPoints = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$estimate[2],
            deltaLower = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$conf.int[1],
            deltaUpper = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$conf.int[2],
            pval = t.test(totalPoints[Strategy == "Zero RB"], totalPoints[Strategy == "Standard"])$p.value) %>%
  mutate(teamPick = as.integer(sub(".*_", "", teamPick))) %>%
  mutate(delta = ZeroRBPoints - StandardPoints) %>%
  arrange(desc(teamPick))
  

detlaPlot <- ggplot(deltas, aes(x= delta, y = teamPick, xmin = deltaLower, xmax = deltaUpper))+ 
  geom_point(aes(size = 3, color = "red")) + 
  geom_errorbarh(height=.1, aes(size = .3, color = "blue")) +
  scale_y_continuous(breaks=1:nrow(deltas), labels=deltas$teamPick) +
  labs(title="Difference in Average Points of Zero RB Strategy Compared to Standard Strategy ", 
       x="Average Points of Zero RB Rosters less Average Points of Standard Rosters", y = "Draft Position") +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        title = element_text(size = 16))
detlaPlot  

##############




ggplot(dataAggregated, aes(x=Strategy, y= totalPoints, fill = Strategy))+
  geom_boxplot() +
  facet_grid(teamPick~.)

ggplot(dataAggregated, aes(x = totalPoints, fill = Strategy, color = Strategy))+
  geom_density(alpha= 0.3) +
  facet_grid(teamPick~.)


ggplot(dataAggregated, aes(x=Strategy, y= totalPoints, fill = Strategy))+
  geom_violin() 


#ggplot(allSimsClean, aes(x=totalPoints, y = teamPick))+
# geom_density_ridges(scale = 0.9) #+
# facet_wrap(~teamPick, nrow=3, ncol=4)
