####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Analysis of scores from 15.1
###
### Notes:
###  * uses data scraped by scrape_athletes.R and scrape_leaderboard.R
###  * used to create this post:
###    swift.pw/data/wod-data-crossfit-open-15-1
###
### Primary Creator(s): Sam Swift (samswift@gmail.com)
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(ggplot2)
library(scales)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Load and Clean Data ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# binary data files saved from database using export_data.R
load("data/leaderboard.15.RData")

# create factors
leaderboard[, gender := factor(division, levels=2:1, labels=c("Women","Men"), ordered=TRUE)]
leaderboard[, wod    := factor(stage, levels=c(1,1.1), labels = c("15.1", "15.1A"), ordered = TRUE)]
leaderboard[, scaled := factor(scaled, levels=c(0,1), labels = c("Rx", "Scaled"), ordered = TRUE)]
leaderboard[, participated := factor(is.na(score), levels=c(F,T), labels=c("WOD'd", "No Show"), ordered=TRUE)]

setkeyv(leaderboard, c("wod", "participated", "scaled", "athlete_id"))
leaderboard <- unique(leaderboard)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Descriptive Stats about Participation ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

n.athletes <- leaderboard[, length(unique(athlete_id))]
n.men      <- leaderboard[gender=="Men", length(unique(athlete_id))]
n.women    <- leaderboard[gender=="Women", length(unique(athlete_id))]
n.athletes
n.women/n.athletes

# drop outs
leaderboard[, list(length(unique(athlete_id)) / n.athletes), by=participated]
leaderboard[gender == "Men", list(length(unique(athlete_id)) / n.men), by=participated]
leaderboard[gender == "Women", list(length(unique(athlete_id)) / n.women), by=participated]

# plot of participation numbers by gender
ggplot(leaderboard[wod == "15.1",
                   list( N = length(unique(athlete_id))), 
                   by=list(gender, participated)], 
       aes(gender, N, fill=participated)) + 
  geom_bar( stat="identity", position="stack") + 
  geom_text(aes(label=N),position = "stack", hjust=1.2) + 
  scale_fill_manual(values=c("#a6cee3","#b2df8a")) + 
  coord_flip() + 
  labs(y="Number of Athletes",
       x="",
       fill="",
       title="Crossfit Open 15.1 participation") + 
  theme_bw(base_size=18) + 
  theme(legend.position="right")
ggsave(filename="~/Desktop/crossfit_15.1_athlete_count.png",width=10, height=4)

# plot Scaled vs RX
leaderboard[participated == "WOD'd", list(length(unique(athlete_id)) / n.athletes), by=scaled]

ggplot(leaderboard[!is.na(score) & wod == "15.1", 
                   .N, by=list(scaled, gender, wod)],
       aes(gender, N, fill=scaled)) + 
  geom_bar( stat="identity", position = "stack") + 
  geom_text(aes(label=N),position = "stack", hjust=1.2) + 
  scale_fill_manual(values=c("#a6cee3","#b2df8a")) + 
  scale_y_continuous(breaks=seq(0,150000, 25000),labels=comma)+
  coord_flip() + 
  labs(y="Number of Athletes",
       x="",
       fill="",
       title="Crossfit Open 15.1 Rx vs Scaled") + 
  theme_bw(base_size=18) + 
  theme(legend.position="right")
ggsave(filename="~/Desktop/crossfit_15.1_rx_scaled.png",width=10, height=4)


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Histogram of 15.1 scores by gender ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# define break points by WOD rep scheme
rounds.151 <- data.frame("x_min" = c(1,61,121,181,241),
                         "x_max" = c(30,90,150,210,250)+1)
moves.151 <- data.frame(xint = c(15,25,30,
                                 45,55,60,
                                 75,85,90,
                                 105,115,120,
                                 135,145,150,
                                 165,175,180,
                                 195,205,210,
                                 225,235,240)+1)
ggplot(leaderboard[wod == "15.1" & scaled == "Rx"]) + 
  geom_rect(data=rounds.151, aes(xmin=x_min, xmax=x_max), 
            ymin=0, ymax=4000, alpha=0.2, fill="grey60") +
  geom_vline(data=moves.151, aes(xintercept = xint), color="lightgrey") + 
  geom_histogram(aes(score, fill = gender), binwidth=1, origin=-1) + 
  scale_x_continuous(breaks = seq(0,240,30)+1,
                     labels = seq(0,240,30),
                     limits=c(0,250)) + 
  labs(x="15.1 Score",y="# of Athletes", fill="", title="Crossfit Open 15.1 Scores (Rx)") +
  facet_grid(gender~.,  scales="free_y") + 
  theme_bw(base_size=18)
ggsave(filename="~/Desktop/crossfit_15.1_wod.png",width=10, height=6)


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Histogram of 15.1A scores by gender ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(leaderboard[wod == "15.1A" & scaled == "Rx"],
       aes(score, fill = gender)) + 
  geom_histogram(binwidth=5) + 
  scale_x_continuous(breaks = seq(0, 400, 25), 
                     limits=c(0,400)) + 
  labs(x="15.1A Score",y="# of Athletes", fill="", title="Crossfit Open 15.1A Scores (Rx)") +
  facet_grid(gender~.,  scales="free_y") + 
  theme_bw(base_size = 18)
ggsave(filename="~/Desktop/crossfit_15.1A_wod.png",width=10, height=6)

# compare scores on 15.1 and 15.1A
score.wide <- dcast.data.table(leaderboard[scaled == "Rx" & 
                                                  !is.na(score)],
                                    athlete_id + gender ~ wod,
                                    value.var = "score")
setnames(score.wide, c("15.1","15.1A"), c("W15_1","W15_1A"))
ggplot(score.wide[!is.na(W15_1) & !is.na(W15_1A)],
       aes(x = W15_1, y=W15_1A, color=gender)) + 
  geom_point(shape=1, alpha=.5) + 
  labs(x="15.1 Score",y="15.1A Score", fill="", title="Crossfit Open 15.1 vs 15.1A Scores") +
  scale_x_continuous(breaks = seq(0, 250, 25), 
                     limits=c(0,250)) + 
  facet_grid(gender~., scales="free") +
  theme_bw(base_size = 18)
ggsave(filename="~/Desktop/crossfit_15.1v1A_scatter.png",width=8, height=7)








