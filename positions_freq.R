###############
#load packages and data
###############

library("tidyverse")
dataClean <- read.csv("0518sims.csv")

###############
#clean up and aggregation
###############

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
  ggtitle("Teams using the Zero RB are forced to draft RBs in rounds 5-8 to fill their roster",
         subtitle = "As a result, they underperform Standard Strategy teams in these rounds; however they perform similarely or better in other rounds") 

