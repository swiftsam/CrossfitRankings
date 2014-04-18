library(ggplot2)
library(scales)
library(data.table)

load("data/athletes.RData")
load("data/leaderboard.RData")

leaderboard <- merge(leaderboard, athletes[,list(id, gender)], by="id", all.x=TRUE)

### Histograms by WOD ###

# 14.1 overall
ggplot(leaderboard[wod=="wod1",], aes(score)) +
  geom_histogram(binwidth=1)+
  scale_x_continuous(limits = c(0,475),
                     breaks = seq(0,475,25))+
  labs(x="14.1 Score",y="# of Athletes") +
  theme_bw(base_size=18)

# 14.1 by gender
ggplot(leaderboard[wod=="wod1" & !is.na(gender),], aes(score)) +
  geom_histogram(aes(y=..density..), binwidth=1)+
  scale_x_continuous(limits = c(0,475),
                     breaks = seq(0,475,25))+
  labs(x="14.1 Score",y="# of Athletes") +
  facet_grid(gender~.) +
  theme_bw(base_size=18)

# 14.2 overall
ggplot(leaderboard[wod=="wod2",], aes(score)) +
  geom_histogram(binwidth=1)+
  scale_x_continuous(limits = c(0,425),
                     breaks = seq(0,425,25))+
  labs(x="14.2 Score",y="# of Athletes") +
  theme_bw(base_size=18)

# 14.2 by gender
ggplot(leaderboard[wod=="wod2" & !is.na(gender),], aes(score)) +
  geom_histogram(aes(y=..density..), binwidth=1)+
  scale_x_continuous(limits = c(0,425),
                     breaks = seq(0,425,25))+
  labs(x="14.2 Score",y="% of Athletes") +
  facet_grid(gender~.) +
  theme_bw(base_size=18)

# 14.3 overall
ggplot(leaderboard[wod=="wod3",], aes(score)) +
  geom_histogram(binwidth=1)+
  scale_x_continuous(limits = c(0,200),
                     breaks = seq(0,200,10))+
  labs(x="14.3 Score",y="# of Athletes") +
  theme_bw(base_size=18)

# 14.3 by gender
ggplot(leaderboard[wod=="wod3" & !is.na(gender),], aes(score)) +
  geom_histogram(aes(y=..density..), binwidth=1)+
  scale_x_continuous(limits = c(0,200),
                     breaks = seq(0,200,10))+
  labs(x="14.3 Score",y="% of Athletes") +
  facet_grid(gender~.) +
  theme_bw(base_size=18)

# 14.4 overall
ggplot(leaderboard[wod=="wod4",], aes(score)) +
  geom_histogram(binwidth=1)+
  scale_x_continuous(limits = c(60,280),
                     breaks = seq(0,280,10))+
  labs(x="14.4 Score",y="# of Athletes") +
  theme_bw(base_size=18)

# 14.4 by gender
ggplot(leaderboard[wod=="wod4" & !is.na(gender),], aes(score)) +
  geom_histogram(aes(y=..density..), binwidth=1)+
  scale_x_continuous(limits = c(60,280),
                     breaks = seq(0,280,10))+
  labs(x="14.4 Score",y="% of Athletes") +
  facet_grid(gender~.) +
  theme_bw(base_size=18)

# 14.5 overall
ggplot(leaderboard[wod=="wod5",], aes(score)) +
  geom_histogram(binwidth=1)+
  scale_x_continuous(limits = c(420,3600),
                     breaks = seq(420,3600, 120),
                     labels = seq(7,60,2))+
  labs(x="14.5 time (minutes)",y="# of Athletes") +
  theme_bw(base_size=18)

# 14.5 by gender
ggplot(leaderboard[wod=="wod5" & !is.na(gender),], aes(score)) +
  geom_histogram(aes(y=..density..), binwidth=1)+
  scale_x_continuous(limits = c(420,3600),
                     breaks = seq(420,3600, 120),
                     labels = seq(7,60,2))+
  labs(x="14.5 Score",y="% of Athletes") +
  facet_grid(gender~.) +
  theme_bw(base_size=18)

