###############
#load packages and data
###############

library("tidyverse")
library(ggpubr)
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
#delta by  pick
##############

###############
#zero vs standard line (probably will not use)
###############
deltasPick <- dataClean %>%
  group_by(draftPick) %>%
  summarise(ZeroRBPoints = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$estimate[1],
            StandardPoints = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$estimate[2],
            deltaLower = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$conf.int[1],
            deltaUpper = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$conf.int[2],
            pval = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$p.value) %>%
  mutate(delta = ZeroRBPoints - StandardPoints) %>%
  arrange(desc(draftPick))


detlaPickPlot <- ggplot(deltasPick, aes(x = draftPick))+ 
  geom_line(aes(y=ZeroRBPoints, color = "Zero RB"))+
  geom_line(aes(y=StandardPoints, color = "Standard")) +
  labs(title="Average Points Scored by Player at Pick", 
       x="Pick #", y = "Draft Position") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        title = element_text(size = 16),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_colour_manual("", 
                      breaks = c("Zero RB", "Standard"),
                      values = c("forestgreen", "brown3"))
detlaPickPlot 

###############
#zero vs standard delta
###############

deltaBar <- ggplot(deltasPick, aes(x = draftPick, y = delta, fill = delta < 0))
deltBarPlot <- deltaBar + geom_bar(stat = 'identity') +
  scale_fill_manual("", values = c("forestgreen", "brown3"))+
  theme(legend.position = "None")
deltBarEx

