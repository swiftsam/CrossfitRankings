####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Analysis of scores from 15.5
###
### Notes:
###  * uses data scraped by scrape_athletes.R and scrape_leaderboard.R
###  * used to create this post:
###   
###
### Primary Creator(s): Sam Swift (samswift@gmail.com)
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(reshape2)
library(ggplot2)
library(scales)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Load and Clean Data ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# binary data files saved from database using export_data.R
load("data/leaderboard.15.RData")
load("data/athletes.RData")

# create factors
leaderboard.15[, gender := factor(division, levels=2:1, labels=c("Women","Men"), ordered=TRUE)]
leaderboard.15[, wod    := factor(stage, levels=c(1,1.1,2,3,4,5), 
                                  labels = c("15.1", "15.1A","15.2","15.3","15.4","15.5"), ordered = TRUE)]
leaderboard.15[, scaled := factor(scaled, levels=c(0,1), labels = c("Rx", "Scaled"), ordered = TRUE)]
leaderboard.15[, participated := factor(is.na(score), levels=c(F,T), labels=c("WOD'd", "No Show"), ordered=TRUE)]

setkeyv(leaderboard.15, c("wod", "participated", "scaled", "athlete_id"))
leaderboard.15 <- unique(leaderboard.15)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Attrition and Scaled vs Rx ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

participation.wod <- leaderboard.15[, 
                                    list(n_total = .N,
                                         n_participated = sum(participated == "WOD'd"),
                                         n_out = sum(participated == "No Show"),
                                         n_scaled = sum(participated == "WOD'd" & scaled == "Scaled"),
                                         n_rx = sum(participated == "WOD'd" & scaled == "Rx")),
                                    by=list(gender, wod)]
participation.wod[, p_participated := n_participated / n_total]
participation.wod[, p_scaled := n_scaled / n_participated]

participated.total <- leaderboard.15[,length(unique(athlete_id))]

leaderboard.15[, list(complete = sum(!is.na(score)) == 6), 
                by=athlete_id][complete == TRUE,length(unique(athlete_id))]

leaderboard.15[scaled == "Rx", 
               list(complete = sum(!is.na(score)) == 6), 
               by=athlete_id][complete == TRUE,length(unique(athlete_id))]


ggplot(participation.wod[wod != "15.1A"], 
       aes(wod, n_participated, fill=gender))+
  geom_bar(stat="identity") + 
  geom_bar(aes(y=n_scaled), stat="identity", fill="white", alpha=.5) + 
  geom_text(aes(y=n_scaled,
                label=paste0(round(p_scaled,2)*100,"%")),vjust=2) + 
  geom_text(aes(y=n_participated,
                label=format(n_participated,big.mark = ",")),vjust=2) + 
  scale_alpha_manual(values = c(.4,1)) + 
  scale_y_continuous(labels=comma, breaks=seq(0,150000,25000)) + 
  labs(x="2015 WoD", y="# Athletes",
       title="CrossFit Open 2015 Participation and Scaled vs Rx") +
  facet_grid(.~gender, scales="free") + 
  theme_bw()
ggsave(filename="~/Desktop/crossfit_15.5_participation.png",width=12, height=6)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 15.5 Rx Histogram and Percentile plot ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
breaks.rx <- data.table("reps" = seq(60*5, 60*30, 60)+1)

ggplot(leaderboard.15[wod=="15.5" & scaled =="Rx" & !is.na(score),]) +
  geom_histogram(aes(x=score, fill=gender), binwidth=1)+
  scale_x_continuous(limits = range(breaks.rx[,reps]),
                     breaks = breaks.rx[,reps],
                     labels = seq(5, 30, 1))+
  labs(x = "15.5 Time (minutes)",
       y = "# of Athletes", 
       title = "Crossfit Open 15.5 Scores (Rx)") +
  facet_grid(gender~.,scale="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename="~/Desktop/crossfit_15.5rx_hist_gender.png",width=10, height=6)

percentiles.rx <- melt(leaderboard.15[wod=="15.5" & scaled =="Rx" & !is.na(score), 
                                      as.list(quantile(score,
                                                       seq(.01,.99,.01))), 
                                      by=gender],
                       id.vars = "gender", variable.name="percentile", value.name="score_cutoff")
percentiles.rx[, percentile := 1 - (as.numeric(percentile)/100)]
ggplot(percentiles.rx) + 
  geom_line(aes(score_cutoff, percentile, color=gender), size=2) + 
  scale_y_continuous(labels=percent) + 
  scale_x_continuous(limits = range(breaks.rx[,reps]),
                     breaks = breaks.rx[,reps],
                     labels = seq(5, 30, 1))+
  labs(x = "15.5 Time (minutes)",
       y = "Percentile", 
       title = "CrossFit Open 15.5 Percentile and Scores (Rx)") +
  theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename="~/Desktop/crossfit_15.5rx_percentile_gender.png",width=10, height=6)

scores.seconds.rx <- leaderboard.15[wod=="15.5" & scaled =="Rx" & !is.na(score), 
                                 list(score, mod_score = score %% 60, gender)]

freq.seconds.gender.rx <- scores.seconds.rx[, list(n_athletes = .N), 
                                             by=list(mod_score, gender)]
freq.seconds.gender.rx[, per_athletes := n_athletes / sum(n_athletes), by=gender ]

ggplot(freq.seconds.gender.rx,
       aes(mod_score, per_athletes, color=gender)) +
  geom_point(size=5, alpha=.4) +
  geom_point(size=5, shape=21, alpha=1) +
  geom_smooth(method="lm", se=F) + 
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(0,60,5)) +
  labs(title = "CrossFit Open 15.5 Finishing Time Seconds",
       x = "Finishing Time Seconds (:SS)", 
       y = "% Athletes") + 
  theme_bw()
ggsave(filename="~/Desktop/crossfit_15.5rx_seconds.png",width=10, height=6)


scores.seconds.rx[mod_score %between% c(50,59), .N] / 
  scores.seconds.rx[mod_score %between% c(1,10), .N]

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 15.5 Scaled Histogram ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
breaks.sc <- data.table("reps" = seq(60*5, 60*30, 60)+1)

ggplot(leaderboard.15[wod=="15.5" & scaled =="Scaled" & !is.na(score),]) +
  geom_histogram(aes(x=score, fill=gender), binwidth=1)+
  scale_x_continuous(limits = range(breaks.sc[,reps]),
                     breaks = breaks.sc[,reps],
                     labels = seq(5, 30, 1))+
  labs(x = "15.5 Time (minutes)",
       y = "# of Athletes", 
       title = "Crossfit Open 15.5 Scores (Scaled)") +
  facet_grid(gender~.,scale="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename="~/Desktop/crossfit_15.5sc_hist_gender.png",width=10, height=6)

percentiles.sc <- melt(leaderboard.15[wod=="15.5" & scaled =="Scaled" & !is.na(score), 
                                      as.list(quantile(score,
                                                       seq(.01,.99,.01))), 
                                      by=gender],
                       id.vars = "gender", variable.name="percentile", value.name="score_cutoff")
percentiles.sc[, percentile := 1 - (as.numeric(percentile)/100)]
ggplot(percentiles.sc) + 
  geom_line(aes(score_cutoff, percentile, color=gender), size=2) + 
  scale_y_continuous(labels=percent) + 
  scale_x_continuous(limits = range(breaks.sc[,reps]),
                     breaks = breaks.sc[,reps],
                     labels = seq(5, 30, 1))+
  labs(x = "15.5 Time (minutes)",
       y = "Percentile", 
       title = "Crossfit Open 15.5 Percentile and Scores (Scaled)") +
  theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename="~/Desktop/crossfit_15.5sc_percentile_gender.png",width=10, height=6)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 15.5 Performance by Profile Stats: Age, Height, Weight ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# merge in profile stats with leaderboard scores
leaderboard.15 <- merge(leaderboard.15, 
                        athletes[, list(athlete_id, age, height, weight, candj)], 
                        by="athlete_id", all.x=T)

leaderboard.15[!is.na(score), 
               percentile := 1 - (rank(score) / .N), 
               by=list(wod, gender, scaled)]

### Age
scores.age <- leaderboard.15[wod=="15.5" & !is.na(score),
                             list(perc_avg = mean(percentile),
                                  n_athletes = .N),
                             by=list(age = as.integer(age),gender,scaled)]

ggplot(scores.age[age >= 17 & age <=55 & !is.na(age) & n_athletes > 50],
       aes(age, perc_avg, fill=gender)) + 
  geom_point(shape=21, aes(size=n_athletes)) + 
  scale_size_continuous(range=c(3,9))+
  scale_x_continuous(limits = c(17,55),
                     breaks = seq(20,55,5))+
  scale_y_continuous(labels=percent) +
  labs(x="Age", y="Avg Percentile", size="# Athletes", fill="Gender",
       title="Crossfit Open 15.5 Percentile by Age") + 
  theme_bw(base_size = 14) + 
  facet_grid(.~scaled)
ggsave(filename="~/Desktop/crossfit_15.5_age.png",width=10, height=6)


### Height
scores.height <- leaderboard.15[wod=="15.5" & !is.na(score) & 
                                  12*4.5 <= height & height <= 12*7.5,
                                list(perc_avg = mean(percentile),
                                     n_athletes = .N),
                                by=list(height,gender,scaled)]

ggplot(scores.height[n_athletes > 50],
       aes(height, perc_avg, fill=gender)) + 
  geom_point(shape=21, aes(size=n_athletes)) + 
  scale_size_continuous(range=c(3,9))+
  scale_x_continuous(limits   = c(57,81),
                     breaks   = seq(57,81,by=3),
                     labels   = c("4\'9\"","5\'","5\'3\"","5\'6\"","5\'9\"","6\'","6\'3\"","6\'6\"","6\'9\""))+
  scale_y_continuous(labels=percent) +
  labs(x="Height (ft, in)", y="Avg Percentile", size="# Athletes", color="Gender",
       title="Crossfit Open 15.5 Percentile by Height") + 
  theme_bw(base_size = 14) + 
  facet_grid(.~scaled)
ggsave(filename="~/Desktop/crossfit_15.5_height.png",width=10, height=6)

### Weight
scores.weight <- leaderboard.15[wod=="15.5" & !is.na(score),
                                list(perc_avg = mean(percentile),
                                     n_athletes = .N),
                                by=list(weight = floor(weight/10)*10,gender,scaled)]
scores.weight[gender == "Men" & weight < 100, perc_avg:=NA]

ggplot(scores.weight[weight < 400 & weight >= 80 & n_athletes > 50],
       aes(weight, perc_avg, fill=gender)) + 
  geom_point(shape=21, aes(size=n_athletes)) + 
  scale_size_continuous(range=c(3,9))+
  scale_x_continuous(breaks = seq(100,300, 20)) +
  scale_y_continuous(labels=percent) +
  labs(x="Weight (10 lb bins)", y="Avg Percentile", size="# Athletes", color="Gender",
       title="Crossfit Open 15.5 Percentile by Weight") + 
  theme_bw(base_size = 14) + 
  facet_grid(.~scaled)
ggsave(filename="~/Desktop/crossfit_15.5_weight.png",width=10, height=6)

### Weight & Height
scores.height.weight <- leaderboard.15[wod=="15.5" & 
                                         !is.na(score) & 
                                         weight <= 300 &
                                         weight >= 95 & 
                                         height >= 57 & 
                                         height <= 81 & 
                                         !(gender == "Men" & height == 60),
                                       list(perc_avg = mean(percentile),
                                            n_athletes = .N),
                                       by=list(weight = floor(weight/10)*10,
                                               height,
                                               gender,scaled)]

ggplot(scores.height.weight[n_athletes > 10], 
       aes(height, weight)) + 
  geom_tile(aes(fill=factor(round(perc_avg,1),
                            levels = seq(0,1,.1),
                            labels = paste0(seq(0,100,10),"%")))) +
  scale_x_continuous(breaks   = seq(57,81,by=3),
                     labels   = c("4\'9\"","5\'","5\'3\"","5\'6\"","5\'9\"","6\'","6\'3\"","6\'6\"","6\'9\""))+
  scale_y_continuous(breaks = seq(100,300, 20)) +
  scale_size_continuous(range = c(4,8)) +
  scale_fill_brewer(palette = "RdYlGn") + 
  labs(x="Height (ft, in)", y="Weight (10 lb bins)", fill="Average\nPercentile",
       title="Crossfit Open 15.5 Percentile by Height & Weight") + 
  theme_bw(base_size = 14) + 
  facet_grid(scaled ~ gender, scales = "free",space = "free")
ggsave(filename="~/Desktop/crossfit_15.5_height_weight.png",width=10, height=8)

leaderboard.15[scaled == "Scaled" & gender == "Men" & wod=="15.5" & weight > 240]

leaderboard.15[!is.na(score) & gender == "Men" & scaled == "Rx" & wod=="15.5" & score > 3 & candj < 185 & candj > 0, .N]
leaderboard.15[!is.na(score) & gender == "Women" & scaled == "Rx" & wod=="15.5" & score > 3 & candj < 125 & candj > 0, .N]


