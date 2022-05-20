###############
#load packages and data
###############

library("tidyverse")
dataClean <- read.csv("0518sims.csv")

###############
#clean up and aggregation
###############

NDColors <- c("#B0C0BF", "#332A21", "#64A1B4",
              "#AE8988", "#C36733", "#DD7764", 
              "#602A10")

ggplot(dataAggregated, aes(y = totalPoints, x = yr)) +
  geom_col(fill = "#2B50AA")

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

ZeroRB_Rosters <- "#64A1B4"
Standard_Rosters <- "#DD7764"


comparisionchart <- ggplot()+
  geom_smooth(data = StandardScatterdata, aes(x=draftPick, y = points), col = "#DD7764", se = FALSE) +
  geom_point(data = StandardScatterdata, aes(x=draftPick, y = points, col = "Standard_Rosters"), alpha = .4) +
  geom_smooth(data = ZeroScatterdata, aes(x=draftPick, y = points), col = "#64A1B4", se = FALSE) +
  geom_point(data = ZeroScatterdata, aes(x=draftPick, y = points, col = "ZeroRB_Rosters"), alpha = .4) +
  ggtitle("") +
  ylab("Average Points at Draft Pick")+
  xlab("Draft Pick") +
  ggtitle("fig. 2 'Standard' rosters substantially outperform 'Zero RB' rosters in mid rounds") +
  theme_light() +
  scale_x_continuous(breaks = seq(0, 160, by = 10)) +
  theme(legend.title=element_blank(), legend.position = "top") 


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
  geom_bar(data = deltasPick, aes(x = draftPick, y = delta, fill = delta < 0), show.legend = F, stat = "identity")+
  scale_fill_manual("", values = c("#64A1B4", "#DD7764")) +
  ylab("Difference in Average Points (Zero RB minus Standard Rosters)")+
  xlab("Draft Pick") +
  scale_x_continuous(breaks = seq(0, 160, by = 10)) +
  theme_light()


##############
#pos freq 
##############
freqData <- dataClean %>%
  group_by(round, Strategy, position) %>%
  summarise(count = n()) %>%
  mutate(countAsPercent = count/25000)


freqDataBar <- ggplot(freqData, aes(x= round, y = countAsPercent, fill = position))+
  geom_bar(stat = "identity") +
  facet_grid(Strategy~.) +
  scale_x_continuous(breaks = seq(1, 16, by = 1)) +
  scale_y_continuous(name="Frequency of Selection", labels = scales::percent) +
  xlab("Round of Draft") +
  ggtitle("fig. 1 'Standard' Strategy rosters load up on RBs in the first four rounds",
          subtitle = "'Zero RB' teams also draft more WR, QBs and TEs") +
  theme(plot.background = element_rect(color = "black", size = .5)) +
  scale_fill_manual(values = NDColors) +
  theme_light()

freqDataBar

##############
#arrange 
##############

leftside <- ggarrange(comparisionchart, barDelta, ncol = 1)  
leftside

rightside <- ggarrange(leftside, freqDataBar)
rightside  

