library(ggplot2)
library(scales)
library(data.table)

load("data/athletes.RData")
load("data/leaderboard.RData")

### Histograms by WOD
ggplot(leaderboard[wod=="wod1",], aes(score)) +
  geom_histogram(aes(y=..density..),binwidth=1, position="dodge")+
  scale_x_continuous(limits = c(0,475),
                     breaks = seq(0,475,25))+
  scale_y_continuous(labels=percent) +
  labs(x="14.1 Score",y="% of Athletes") +
  theme_bw(base_size=18)


ggplot(leaderboard[wod=="wod2",], aes(score)) +
  geom_histogram(aes(y=..density..),binwidth=1, position="dodge")+
  scale_x_continuous(limits = c(0,425),
                     breaks = seq(0,425,25))+
  scale_y_continuous(labels=percent) +
  labs(x="14.2 Score",y="% of Athletes") +
  theme_bw(base_size=18)

ggplot(leaderboard[wod=="wod3",], aes(score)) +
  geom_histogram(aes(y=..density..),binwidth=1, position="dodge")+
  scale_x_continuous(limits = c(0,200),
                     breaks = seq(0,200,10))+
  scale_y_continuous(labels=percent) +
  labs(x="14.3 Score",y="% of Athletes") +
  theme_bw(base_size=18)


ggplot(leaderboard[wod=="wod4",], aes(score)) +
  geom_histogram(aes(y=..density..),binwidth=1, position="dodge")+
  scale_x_continuous(limits = c(60,280),
                     breaks = seq(0,280,10))+
  scale_y_continuous(labels=percent) +
  labs(x="14.4 Score",y="% of Athletes") +
  theme_bw(base_size=18)
