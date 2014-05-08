library(ggplot2)
library(scales)
library(data.table)

load("data/athletes.RData")
load("data/leaderboard.RData")

leaderboard <- merge(leaderboard, athletes[,list(id, gender)], by="id", all.x=TRUE)

### Histograms by WOD ###

# 14.1 overall
wod1 <- data.frame("x_min" = 0:10 * 45 + 1,
                   "x_max" = 0:10 * 45 + 30)
# ggplot(leaderboard[wod=="wod1",], aes(score)) +
#   geom_histogram(binwidth=1)+
#   scale_x_continuous(limits = c(0,475),
#                      breaks = seq(0,475,25))+
#   labs(x="14.1 Score",y="# of Athletes") +
#   theme_bw(base_size=14)

# 14.1 by gender
ggplot(leaderboard[wod=="wod1" & !is.na(gender) & !is.na(score)]) +
  geom_rect(data=wod1, aes(xmin=x_min, xmax=x_max+1), 
            ymin=0, ymax=2000, alpha=0.2, fill="grey60") +
  geom_histogram(aes(x=score, fill=gender), binwidth=1)+
  scale_x_continuous(limits = c(0,475),
                     breaks = seq(0,475,25))+
  labs(x="14.1 Score",y="# of Athletes") +
  facet_grid(gender~., scales="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none")
ggsave(filename="crossfit_14.1_hist_gender.png",width=10, height=6)

# 14.2 overall
rep.ct   <- 1
reps.rd  <- 10
mins     <- c()
maxs     <- c()

for(i in 1:20){
  mins    <- c(mins, rep.ct)
  maxs    <- c(maxs, rep.ct + reps.rd -1)
  rep.ct  <- rep.ct + (reps.rd * 2)
  mins    <- c(mins, rep.ct)
  maxs    <- c(maxs, rep.ct + reps.rd -1)
  rep.ct  <- rep.ct + (reps.rd * 2)
  reps.rd <- reps.rd + 2
}

wod2 <- data.frame("x_min" = mins,
                   "x_max" = maxs)
# ggplot(leaderboard[wod=="wod2",], aes(score)) +
#   geom_histogram(binwidth=1)+
#   scale_x_continuous(limits = c(0,425),
#                      breaks = seq(0,425,25))+
#   labs(x="14.2 Score",y="# of Athletes") +
#   theme_bw(base_size=14)

# 14.2 by gender
ggplot(leaderboard[wod=="wod2" & !is.na(gender),]) +
  geom_rect(data=wod2, aes(xmin=x_min, xmax=x_max+1), 
            ymin=0, ymax=10000, alpha=0.2, fill="grey60") +
  geom_histogram(aes(x=score, fill=gender), binwidth=1)+
  scale_x_continuous(limits = c(0,300),
                     breaks = seq(0,300,25))+
  labs(x="14.2 Score",y="# of Athletes") +
  facet_grid(gender~.,scale="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none")
ggsave(filename="crossfit_14.2_hist_gender.png",width=10, height=6)

# 14.3 overall
rep.ct   <- 1
reps.rd  <- 10
mins     <- c()
maxs     <- c()

for(i in 1:20){
  mins    <- c(mins, rep.ct)
  maxs    <- c(maxs, rep.ct + reps.rd -1)
  rep.ct  <- rep.ct + reps.rd + 15
  reps.rd <- reps.rd + 5
}

wod3 <- data.frame("x_min" = mins,
                   "x_max" = maxs)
# ggplot(leaderboard[wod=="wod3",], aes(score)) +
#   geom_histogram(binwidth=1)+
#   scale_x_continuous(limits = c(0,200),
#                      breaks = seq(0,200,10))+
#   labs(x="14.3 Score",y="# of Athletes") +
#   theme_bw(base_size=14)

# 14.3 by gender
ggplot(leaderboard[wod=="wod3" & !is.na(gender),]) +
  geom_rect(data=wod3, aes(xmin=x_min, xmax=x_max+1), 
            ymin=0, ymax=10000, alpha=0.2, fill="grey60") +
  geom_histogram(aes(x=score, fill=gender), binwidth=1)+
  scale_x_continuous(limits = c(0,200),
                     breaks = seq(0,200,10))+
  labs(x="14.3 Score",y="# of Athletes") +
  facet_grid(gender~., scales="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none")
ggsave(filename="crossfit_14.3_hist_gender.png",width=10, height=6)

# 14.4 overall
wod4 <- data.frame("x_min" = c(61,151,201),
                   "x_max" = c(110,180,260))
# 
# ggplot(leaderboard[wod=="wod4",], aes(score)) +
#   geom_histogram(binwidth=1)+
#   scale_x_continuous(limits = c(60,280),
#                      breaks = seq(0,280,10))+
#   labs(x="14.4 Score",y="# of Athletes") +
#   theme_bw(base_size=14)

# 14.4 by gender
ggplot(leaderboard[wod=="wod4" & !is.na(gender),]) +
  geom_rect(data=wod4, aes(xmin=x_min, xmax=x_max+1), 
            ymin=0, ymax=20000, alpha=0.2, fill="grey60") +
  geom_histogram(aes(x=score, fill=gender), binwidth=1)+
  scale_x_continuous(limits = c(60,280),
                     breaks = seq(0,280,10))+
  labs(x="14.4 Score",y="# of Athletes") +
  facet_grid(gender~., scales="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none")
ggsave(filename="crossfit_14.4_hist_gender.png",width=10, height=6)

# 14.5 overall
# ggplot(leaderboard[wod=="wod5",], aes(score)) +
#   geom_histogram(binwidth=1)+
#   scale_x_continuous(limits = c(420,3600),
#                      breaks = seq(420,3600, 120),
#                      labels = seq(7,60,2))+
#   labs(x="14.5 time (minutes)",y="# of Athletes") +
#   theme_bw(base_size=14)

# 14.5 by gender
wod5 <- data.frame("x_min" = seq(600,3000,600),
                   "x_max" = seq(900,3300,600))
ggplot(leaderboard[wod=="wod5" & !is.na(gender),]) +
  geom_rect(data=wod5, aes(xmin=x_min, xmax=x_max+1), 
            ymin=0, ymax=10000, alpha=0.2, fill="grey60") +
  geom_histogram(aes(x=score, fill=gender), binwidth=1)+
  scale_x_continuous(limits = c(420,3600),
                     breaks = c(420,seq(480,3600, 120)),
                     labels = c(7,seq(8,60,2)))+
  labs(x="14.5 Score (minutes)",y="# of Athletes") +
  facet_grid(gender~., scales="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none")
ggsave(filename="crossfit_14.5_hist_gender.png",width=10, height=6)
