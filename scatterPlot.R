###############
#load packages and data
###############

library("tidyverse")
dataClean <- read.csv("0518sims.csv")

###############
#clean up and aggregation
###############


dataAggregated <- dataClean %>%
  group_by(teamPick, Strategy, simulation, yr) %>%
  summarise(totalPoints = sum(FantasyPoints, na.rm = TRUE))

deltasPick <- dataClean %>%
  group_by(draftPick) %>%
  summarise(ZeroRBPoints = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$estimate[1],
            StandardPoints = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$estimate[2],
            deltaLower = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$conf.int[1],
            deltaUpper = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$conf.int[2],
            pval = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$p.value) %>%
  mutate(delta = ZeroRBPoints - StandardPoints) %>%
  arrange(desc(draftPick))

##############
#delta scatter
##############

mostcommonPostion0RB <- dataClean %>% 
  filter(Strategy == "Zero RB") %>%
  count(draftPick, position) %>%
  spread(position, n) %>%
  mutate_all(~coalesce(.,0)) 

ZeroScatterdata <- mostcommonPostion0RB%>%
  mutate(mostFreq = names(mostcommonPostion0RB)[max.col(mostcommonPostion0RB)]) %>%
  inner_join(deltasPick, by = c("draftPick" = "draftPick")) %>%
  mutate(strategy = "zero") %>%
  rename(points = ZeroRBPoints) %>%
  select(-StandardPoints) 

ZeroRB_Rosters <- "blue"
Standard_Rosters <- "red"


comparisionchart <- ggplot()+
  geom_smooth(data = StandardScatterdata, aes(x=draftPick, y = points), col = "red") +
  geom_point(data = StandardScatterdata, aes(x=draftPick, y = points, col = "Standard_Rosters"), alpha = .4) +
  geom_smooth(data = ZeroScatterdata, aes(x=draftPick, y = points), col = "cyan4") +
  geom_point(data = ZeroScatterdata, aes(x=draftPick, y = points, col = "ZeroRB_Rosters"), alpha = .4) +
  theme(legend.title=element_blank(), legend.position = "top") +
  ggtitle("") +
  ylab("Average Points at Draft Pick")+
  xlab("Draft Pick") +
  ggtitle("Standard rosters perform better in rounds 5-8") 

##############
#bar differential
##############
deltasPick <- dataClean %>%
  group_by(draftPick) %>%
  summarise(ZeroRBPoints = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$estimate[1],
            StandardPoints = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$estimate[2],
            deltaLower = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$conf.int[1],
            deltaUpper = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$conf.int[2],
            pval = t.test(FantasyPoints[Strategy == "Zero RB"], FantasyPoints[Strategy == "Standard"])$p.value) %>%
  mutate(delta = ZeroRBPoints - StandardPoints) %>%
  arrange(desc(draftPick))

barDelta <- ggplot()+
  geom_bar(data = deltasPick, aes(x = draftPick, y = delta, fill = delta < 0, alpha = .05), show.legend = F, stat = "identity")+
  scale_fill_manual("", values = c("forestgreen", "violetred3")) +
  geom_smooth(data = deltasPick, aes(x = draftPick, y = delta), col = "black") +
  ylab("Difference in Average Points (Zero RB minus Standard Rosters)")+
  xlab("Draft Pick")


##############
#pos freq 
##############
freqData <- dataClean %>%
  group_by(round, Strategy, position) %>%
  summarise(count = n()) %>%
  mutate(countAsPercent = count/500)


freqDataBar <- ggplot(freqData, aes(x= round, y = countAsPercent, fill = position))+
  geom_bar(stat = "identity") +
  facet_grid(Strategy~.) +
  scale_x_continuous(breaks = seq(1, 16, by = 1)) +
  scale_y_continuous(name="Frequency of Selection", labels = scales::percent) +
  xlab("Round of Draft") +
  ggtitle("Zero RB rosters are forced to pick up sub-standard running backs in these rounds to fill their rosters") +
  theme(plot.background = element_rect(color = "black", size = .5))


##############
#arrange 
##############

leftside <- ggarrange(comparisionchart, barDelta, ncol = 1)  

rightside <- ggarrange(leftside, freqDataBar)
rightside  

