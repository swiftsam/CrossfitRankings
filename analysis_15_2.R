load("data/leaderboard.14.RData")
load("data/leaderboard.15.RData")
load("data/athletes.RData")

# create factors
leaderboard.15[, gender := factor(division, levels=2:1, labels=c("Women","Men"), ordered=TRUE)]
leaderboard.15[, wod    := factor(stage, levels=c(1,1.1,2), labels = c("15.1", "15.1A","15.2"), ordered = TRUE)]
leaderboard.15[, scaled := factor(scaled, levels=c(0,1), labels = c("Rx", "Scaled"), ordered = TRUE)]
leaderboard.15[, participated := factor(is.na(score), levels=c(F,T), labels=c("WOD'd", "No Show"), ordered=TRUE)]

setkeyv(leaderboard.15, c("wod", "participated", "scaled", "athlete_id"))
leaderboard.15 <- unique(leaderboard.15)

# 15.2 overall
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

# 15.2 by gender
ggplot(leaderboard.15[wod=="15.2" & scaled =="Rx" & !is.na(score),]) +
  geom_rect(data=wod2, aes(xmin=x_min, xmax=x_max+1), 
            ymin=0, ymax=10000, alpha=0.2, fill="grey60") +
  geom_histogram(aes(x=score, fill=gender), binwidth=1)+
  scale_x_continuous(limits = c(0,300),
                     breaks = seq(0,300,25))+
  labs(x="15.2 Score",y="# of Athletes") +
  facet_grid(gender~.,scale="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none")
ggsave(filename="crossfit_14.2_hist_gender.png",width=10, height=6)


compare.1415 <- merge(leaderboard.15[stage == 2 & !is.na(score),
                                    list(athlete_id, score_152 = score, gender)],
                     leaderboard.14[stage == 2 & !is.na(score),
                                    list(athlete_id, score_142 = score)],
                     by="athlete_id")

compare.1415[, delta := score_152 - score_142]

compare.1415[, mean(delta)]

ggplot(compare.1415, aes(score_142, score_152, color=gender)) + 
  geom_point(shape=1) + 
  geom_abline(yintercept=0, slope=1) + 
  facet_grid(gender~.,scale="free") + 
  labs(x="14.2 Score", y="15.2 Score") + 
  theme_bw(base_size=14)
  
ggplot(compare.1415, aes(delta, fill = gender)) + 
  geom_vline(xintercept=0) + 
  geom_histogram(binwidth=1) + 
  facet_grid(gender~.,scale="free") + 
  labs(x="Difference in Score 14.2 --> 15.2", y="# Athletes") + 
  theme_bw(base_size=14) +
  theme(legend.position = "none")
ggsave(filename="~/Desktop/crossfit_15.2_delta_gender.png",width=10, height=6)






