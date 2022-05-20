###############
#load packages and data
###############

library("tidyverse")
dataClean <- read.csv("0518sims.csv")



###############
#celan data and data
###############

#other years
others <- filter(dataClean, yr != 2018) %>%
  select(PlayerID, name, position, rank_pre, rank_post, FantasyPoints) %>%
  rename(average_draft_position = rank_pre) %>%
  rename(postseason_rank = rank_post) %>%
  filter() %>%
  arrange(average_draft_position)

othersO <- others[!duplicated(others$PlayerID), ] %>%
  filter(average_draft_position <= 40) %>%
  group_by(position) %>%
  summarise(mean(FantasyPoints)) %>%
  mutate(year = "2017, 2019-2021")

#other 2018

two8 <- filter(dataClean, yr == 2018) %>%
  select(PlayerID, name, position, rank_pre, rank_post, FantasyPoints) %>%
  rename(average_draft_position = rank_pre) %>%
  rename(postseason_rank = rank_post) %>%
  filter() %>%
  arrange(average_draft_position)

two8t <- two8[!duplicated(two8$PlayerID), ] %>%
  filter(average_draft_position <= 40) %>%
  mutate(FantasyPoints = if_else(is.na(FantasyPoints), 0, FantasyPoints)) %>%
  group_by(position) %>%
  summarise(mean(FantasyPoints)) %>%
  mutate(year = "2018")

data <- rbind(two8t, othersO)

###############
#plot data and data
###############

barplot <- ggplot(data, aes(x=position, y = `mean(FantasyPoints)`, fill = year)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(values = c("#64A1B4", "#DD7764")) +
  ylab("Average Fantasy Points (Top 40 players by adp)") +
  ggtitle("fig 6. 2018 was a banner year for WRs in the top 40 of ADP and an above average year for QBs and TEs",
          subtitle = "RB performance was slightly below average") +
  xlab("")
  theme_light()

barplot


