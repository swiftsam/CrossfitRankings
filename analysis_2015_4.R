####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Analysis of scores from 15.4
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
load("data/leaderboard.15.1.RData")
load("data/leaderboard.15.1.1.RData")
load("data/leaderboard.15.2.RData")
load("data/leaderboard.15.3.RData")
load("data/leaderboard.15.4.RData")
load("data/athletes.RData")

leaderboard.15 <- rbindlist(list(leaderboard.15.1,
                                 leaderboard.15.1.1,
                                 leaderboard.15.2,
                                 leaderboard.15.3,
                                 leaderboard.15.4))

# create factors
leaderboard.15[, gender := factor(division, levels=2:1, labels=c("Women","Men"), ordered=TRUE)]
leaderboard.15[, wod    := factor(stage, levels=c(1,1.1,2,3,4), 
                                  labels = c("15.1", "15.1A","15.2","15.3","15.4"), ordered = TRUE)]
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

ggplot(participation.wod[wod != "15.1A"], 
       aes(wod, n_participated, fill=gender))+
  geom_bar(stat="identity") + 
  geom_bar(aes(y=n_scaled), stat="identity", fill="white", alpha=.5) + 
  geom_text(aes(y=n_scaled,
                label=paste0(round(p_scaled,2)*100,"%")),vjust=2) + 
  geom_text(aes(y=n_participated,
                label=n_participated),vjust=2) + 
  scale_alpha_manual(values = c(.4,1)) + 
  scale_y_continuous(labels=comma, breaks=seq(0,150000,25000)) + 
  labs(x="2015 WoD", y="# Athletes",
       title="CrossFit Open 2015 Participation and Scaled vs Rx") +
  facet_grid(.~gender, scales="free") + 
  theme_bw()
ggsave(filename="~/Desktop/crossfit_15.4_participation.png",width=10, height=6)

leaderboard.15[scaled == "Rx" & !is.na(score), 
               list(gender, rx = .N == 4), 
               by=athlete_id][,sum(rx==TRUE) / .N, by=gender]

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 15.4 Rx Histogram and Percentile plot ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
breaks.rx <- data.table("reps" = c(1,8,58,158,165,215,315,322,372,472,479,529,629))
rounds.rx <- data.table("x_min" = breaks.rx[c(1,7), reps],
                        "x_max" = breaks.rx[c(4,10), reps])

ggplot(leaderboard.15[wod=="15.4" & scaled =="Rx" & !is.na(score),]) +
  geom_rect(data=rounds.rx, aes(xmin=x_min, xmax=x_max+1), 
            ymin=0, ymax=10000, alpha=0.2, fill="grey60") +
  geom_vline(data=breaks.rx, aes(xintercept = reps), color="darkgrey", alpha=.7) + 
  geom_histogram(aes(x=score, fill=gender), binwidth=1)+
  scale_x_continuous(limits = c(0,120),
                     breaks = breaks.rx[,reps])+
  labs(x="15.4 Score",y="# of Athletes", title="Crossfit Open 15.4 Scores (Rx)") +
  facet_grid(gender~.,scale="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename="~/Desktop/crossfit_15.4rx_hist_gender.png",width=10, height=6)

percentiles.rx <- melt(leaderboard.15[wod=="15.4" & scaled =="Rx" & !is.na(score), 
                                      as.list(quantile(score,
                                                       seq(.01,.99,.01))), 
                                      by=gender],
                       id.vars = "gender", variable.name="percentile", value.name="score_cutoff")
percentiles.rx[, percentile := as.numeric(percentile)/100]
ggplot(percentiles.rx) + 
  geom_rect(data=rounds.rx, aes(xmin=x_min, xmax=x_max+1), 
            ymin=0, ymax=10000, alpha=0.2, fill="grey60") +
  geom_vline(data=breaks.rx, aes(xintercept = reps), color="darkgrey", alpha=.7) + 
  geom_line(aes(score_cutoff, percentile, color=gender), size=2) + 
  scale_y_continuous(labels=percent) + 
  scale_x_continuous(limits = c(0,120),
                     breaks = breaks.rx[,reps])+
  labs(x="15.4 Score (Rx)",y="Percentile", title="Crossfit Open 15.4 Percentile and Scores (Rx)") +
  theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename="~/Desktop/crossfit_15.4rx_percentile_gender.png",width=10, height=6)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 15.4 Scaled Histogram ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

breaks.sc <- data.table("reps" = c(1,51,251,301,501,551,751,801,1001,1051))
rounds.sc <- data.table("x_min" = breaks.sc[c(1,5,9), reps],
                        "x_max" = breaks.sc[c(3,7,10), reps])

ggplot(leaderboard.15[wod=="15.4" & scaled =="Scaled" & !is.na(score),]) +
  geom_rect(data=rounds.sc, aes(xmin=x_min, xmax=x_max+1), 
            ymin=0, ymax=10000, alpha=0.2, fill="grey60") +
  geom_vline(data=breaks.sc, aes(xintercept = reps), color="darkgrey", alpha=.5) + 
  geom_histogram(aes(x=score, fill=gender), binwidth=1)+
  scale_x_continuous(limits = c(0,150),
                     breaks = breaks.sc[,reps])+
  labs(x="15.4 Score",y="# of Athletes", title="Crossfit Open 15.4 Scores (Scaled)") +
  facet_grid(gender~.,scale="free_y") +
  theme_bw(base_size=14) +
  theme(legend.position = "none", panel.grid.minor.x = element_blank())
ggsave(filename="~/Desktop/crossfit_15.4sc_hist_gender.png",width=10, height=6)

percentiles.sc <- melt(leaderboard.15[wod=="15.4" & scaled =="Scaled" & !is.na(score), 
                                      as.list(quantile(score,
                                                       seq(.01,.99,.01))), 
                                      by=gender],
                       id.vars = "gender", variable.name="percentile", value.name="score_cutoff")
percentiles.sc[, percentile := as.numeric(percentile)/100]
ggplot(percentiles.sc) + 
  geom_rect(data=rounds.sc, aes(xmin=x_min, xmax=x_max+1), 
            ymin=0, ymax=10000, alpha=0.2, fill="grey60") +
  geom_vline(data=breaks.sc, aes(xintercept = reps), color="darkgrey", alpha=.7) + 
  geom_line(aes(score_cutoff, percentile, color=gender), size=2) + 
  scale_y_continuous(labels=percent) + 
  scale_x_continuous(limits = c(0,150),
                     breaks = breaks.sc[,reps])+
  labs(x="15.4 Score (Scaled)",y="Percentile", title="Crossfit Open 15.4 Percentile and Scores (Scaled)") +
  theme_bw() + 
  theme(legend.position = "none", panel.grid.minor.x = element_blank())
ggsave(filename="~/Desktop/crossfit_15.4sc_percentile_gender.png",width=10, height=6)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 15.4 Performance by Profile Stats: Age, Height, Weight ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# merge in profile stats with leaderboard scores
leaderboard.15 <- merge(leaderboard.15, 
                        athletes[, list(athlete_id, age, height, weight)], 
                        by="athlete_id", all.x=T)

leaderboard.15[!is.na(score), 
               percentile := rank(score) / .N, 
               by=list(wod, gender, scaled)]

### Age
scores.age <- leaderboard.15[wod=="15.4" & !is.na(score),
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
       title="Crossfit Open 15.4 Percentile by Age") + 
  theme_bw(base_size = 14) + 
  facet_grid(.~scaled)
ggsave(filename="~/Desktop/crossfit_15.4_age.png",width=10, height=6)


### Height
scores.height <- leaderboard.15[wod=="15.4" & !is.na(score) & 
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
       title="Crossfit Open 15.4 Percentile by Height") + 
  theme_bw(base_size = 14) + 
  facet_grid(.~scaled)
ggsave(filename="~/Desktop/crossfit_15.4_height.png",width=10, height=6)

### Weight
scores.weight <- leaderboard.15[wod=="15.4" & !is.na(score),
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
       title="Crossfit Open 15.4 Percentile by Weight") + 
  theme_bw(base_size = 14) + 
  facet_grid(.~scaled)
ggsave(filename="~/Desktop/crossfit_15.4_weight.png",width=10, height=6)


