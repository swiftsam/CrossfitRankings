library(data.table)
library(reshape2)
library(ggplot2)

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

# 15.2 Rx
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

breaks.rx <- c(mins[2:11]-1, maxs[1:11])

# 15.2 by gender
ggplot(leaderboard.15[wod=="15.2" & scaled =="Rx" & !is.na(score),]) +
  geom_rect(data=wod2, aes(xmin=x_min, xmax=x_max+1), 
            ymin=0, ymax=10000, alpha=0.2, fill="grey60") +
  geom_histogram(aes(x=score, fill=gender), binwidth=1)+
  scale_x_continuous(limits = c(0,300),
                     breaks = breaks.rx)+
  labs(x="15.2 Score",y="# of Athletes", title="Crossfit Open 15.2 Scores (Rx)") +
  facet_grid(gender~.,scale="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none")
ggsave(filename="~/Desktop/crossfit_15.2_hist_gender.png",width=10, height=6)

leaderboard.15[wod == "15.2" & scaled == "Rx" & !is.na(score), sum(score == 10)]
leaderboard.15[wod == "15.2" & scaled == "Rx" & !is.na(score) & gender == "Men", sum(score == 10) / .N]
leaderboard.15[wod == "15.2" & scaled == "Rx" & !is.na(score) & gender == "Women", sum(score == 10) / .N]

leaderboard.15[wod == "15.2" & !is.na(score), sum(scaled=="Scaled") / .N]

percentiles.14.15 <- rbind(leaderboard.14[!is.na(score) & stage == 2, 
                                          as.list(quantile(score, c(.5, .75, .95))), 
                                          by = list(gender = division)],
                          leaderboard.15[!is.na(score) & wod == "15.2" & scaled == "Rx", 
                                         as.list(quantile(score, c(.5, .75, .95))), 
                                         by = gender])

percentiles.14.15[, year := c("14.2","14.2","15.2","15.2")]
percentiles.14.15[, gender := c("Men","Women")]

ggplot(melt(percentiles.14.15, id.vars=c("year","gender")),
       aes(variable,value, fill=year)) + 
  geom_bar(stat="identity", position="dodge") + 
  geom_text(aes(label=value), position = position_dodge(width=1), hjust=1.2) + 
  scale_fill_manual(values=c("#a6cee3","#b2df8a")) + 
  scale_y_continuous(limits = c(0,208),
                     breaks = breaks.rx)+
  facet_grid(gender~.) +
  labs(x="Percentile",y="Score to acheive percentile", color="WOD", title="Crossfit Open 14.2 & 15.2 Percentile Thresholds (Rx)") + 
  coord_flip()+
  theme_bw(base_size=14)
ggsave(filename="~/Desktop/crossfit_14.2_15.2_percentiles.png",width=10, height=5)

# 15.2 scaled
rep.ct   <- 1
reps.rd  <- 6
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

wod2.scaled <- data.frame("x_min" = mins,
                   "x_max" = maxs)
breaks.scaled <- c(mins[2:13]-1, maxs[1:13])

# 15.2 scaled
ggplot(leaderboard.15[wod=="15.2" & scaled =="Scaled" & !is.na(score),]) +
  geom_rect(data=wod2.scaled, aes(xmin=x_min, xmax=x_max+1), 
            ymin=0, ymax=10000, alpha=0.2, fill="grey60") +
  geom_histogram(aes(x=score, fill=gender), binwidth=1)+
  scale_x_continuous(limits = c(0,282),
                     breaks = breaks.scaled)+
  labs(x="15.2 Score",y="# of Athletes", title="Crossfit Open 15.2 Scores (scaled)") +
  facet_grid(gender~.,scale="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none")
ggsave(filename="~/Desktop/crossfit_15.2.scaled_hist_gender.png",width=10, height=6)


compare.1415 <- merge(leaderboard.15[stage == 2 & !is.na(score) & scaled == "Rx",
                                    list(athlete_id, score_152 = score, gender)],
                     leaderboard.14[stage == 2 & !is.na(score),
                                    list(athlete_id, score_142 = score)],
                     by="athlete_id")

compare.1415[, delta := score_152 - score_142]

compare.1415[, mean(delta)]

ggplot(compare.1415, aes(score_142, score_152, color=gender)) + 
  geom_abline(yintercept=0, slope=1) + 
  geom_point(shape=1, alpha = .1) + 
  geom_density2d(color="white") +
  facet_grid(gender~.) + 
  scale_x_continuous(limits = c(0,300),
                     breaks = breaks.rx)+
  scale_y_continuous(limits = c(0,300),
                     breaks = breaks.rx)+
  labs(x="14.2 Score", y="15.2 Score", title="Crossfit Open 14.2 vs 15.2 Scores (Rx)") + 
  theme_bw(base_size=14) + theme(legend.position="none", panel.grid.minor = element_blank())
ggsave(filename="~/Desktop/crossfit_15.2_14.2_gender.png",width=10, height=8)

  
ggplot(compare.1415, aes(delta, fill = gender)) + 
  geom_histogram(binwidth=1) + 
  geom_vline(xintercept=0) + 
  facet_grid(gender~.,scale="free") + 
  scale_x_continuous(limits=c(-100,150)) + 
  labs(x="Difference in Score 14.2 --> 15.2", y="# Athletes", title="Crossfit Open Athlete Improvement 14.2 to 15.2 (Rx)") + 
  theme_bw(base_size=14) +
  theme(legend.position = "none")
ggsave(filename="~/Desktop/crossfit_15.2_delta_gender.png",width=10, height=6)



leaderboard.15 <- merge(leaderboard.15, 
                        athletes[, list(athlete_id, age, height, weight)], 
                        by="athlete_id", all.x=T)

#### AGE
scores.age <- leaderboard.15[wod=="15.2" & scaled =="Rx" & !is.na(score),
                             list(score_avg = mean(score),
                                  n_athletes = .N),
                             by=list(age = as.integer(age),gender)]

ggplot(scores.age[age >= 17 & age <=55 & !is.na(age)],
       aes(age, score_avg, fill=gender)) + 
  geom_point(shape=21, aes(size=n_athletes)) + 
  scale_size_continuous(range=c(3,9))+
  scale_x_continuous(limits = c(17,55),
                     breaks = seq(20,55,5))+
  labs(x="Age", y="Avg Score", size="# Athletes", fill="Gender",
       title="Crossfit Open 15.2 (Rx) Scores by Age") + 
  theme_bw(base_size = 14)
ggsave(filename="~/Desktop/crossfit_15.2_age.png",width=10, height=6)


#### HEIGHT
scores.height <- leaderboard.15[wod=="15.2" & scaled =="Rx" & !is.na(score) & 
                                  12*4.5 <= height & height <= 12*7.5,
                             list(score_avg = mean(score),
                                  n_athletes = .N),
                             by=list(height,gender)]

ggplot(scores.height[score_avg < 150],
       aes(height, score_avg, fill=gender)) + 
  geom_point(shape=21, aes(size=n_athletes)) + 
  scale_size_continuous(range=c(3,9))+
  scale_x_continuous(limits   = c(57,81),
                     breaks   = seq(57,81,by=3),
                     labels   = c("4\'9\"","5\'","5\'3\"","5\'6\"","5\'9\"","6\'","6\'3\"","6\'6\"","6\'9\""))+
  labs(x="Height (ft, in)", y="Avg Score", size="# Athletes", color="Gender",
       title="Crossfit Open 15.2 (Rx) Scores by Height") + 
  theme_bw(base_size = 14)
ggsave(filename="~/Desktop/crossfit_15.2_height.png",width=10, height=6)


#### WEIGHT
scores.weight <- leaderboard.15[wod=="15.2" & scaled =="Rx" & !is.na(score),
                                list(score_avg = mean(score),
                                     n_athletes = .N),
                                by=list(weight = floor(weight/10)*10,gender)]
scores.weight[gender == "Men" & weight < 100, score_avg:=NA]

ggplot(scores.weight[weight < 400 & weight >= 80],
       aes(weight, score_avg, fill=gender)) + 
  geom_point(shape=21, aes(size=n_athletes)) + 
  scale_size_continuous(range=c(3,9))+
  scale_x_continuous(breaks = seq(100,400, 20)) + 
  labs(x="Weight (10 lb bins)", y="Avg Score", size="# Athletes", color="Gender",
       title="Crossfit Open 15.2 (Rx) Scores by Weight") + 
  theme_bw(base_size = 14)
ggsave(filename="~/Desktop/crossfit_15.2_weight.png",width=10, height=6)

