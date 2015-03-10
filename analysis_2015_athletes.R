####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Summarize profile data from athletes in 2015 Open
###
### Notes:
###  * uses data scraped by scrape_athletes.R and scrape_leaderboard.R
###  * used to create this post:
###    swift.pw/data/whats-normal-or-top-5-for-a-crossfit-athlete
###
### Primary Creator(s): Sam Swift (samswift@gmail.com)
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggplot2)
library(scales)
library(data.table)
library("xtable")

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Load and Clean Data ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# binary data files saved from database using export_data.R
load("data/athletes.RData")
load("data/leaderboard.15.RData")

# subset to 2015 athletes
athletes <- athletes[athlete_id %in% leaderboard.15[, unique(athlete_id)]]

# fix data issues
athletes <- athletes[!is.na(gender)]
athletes[, age:=as.integer(age)]

#replace --'s with NA's
for (col in names(athletes)){
  athletes[(get(col)=="--"), (col) := NA]
}

# remove spurious values
athletes[12*4.5 >= height   | height   >= 12*7.5, height:=NA]
athletes[80     >= weight   | weight   >= 500,    weight:=NA]
athletes[60     >= fran     | fran     >= 1200,   fran:= NA]
athletes[60     >= helen    | helen    >= 2400,   helen:= NA]
athletes[30     >= grace    | grace    >= 1200,   grace:= NA]
athletes[120    >= filthy50 | filthy50 >= 5400,   filthy50:= NA]
athletes[43     >= run400   | run400   >= 300,    run400:= NA]
athletes[757    >= run5k    | run5k    >= 3600,   run5k:= NA]
athletes[33     >= candj    | candj    >= 600,    candj:= NA]
athletes[33     >= snatch   | snatch   >= 400,    snatch:= NA]
athletes[33     >= deadlift | deadlift >= 750,    deadlift:= NA]
athletes[33     >= backsq   | backsq   >= 750,    backsq:= NA]
athletes[0      >= pullups  | pullups  >= 150,    pullups:= NA]

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summarize each profile variable by gender ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
athletes.long <- melt(athletes[, c(1,6:21), with=F], 
                      id.vars = c("athlete_id", "gender"))

athlete.summary <- athletes.long[!is.na(value), 
                                 as.list(c(.N,
                                           mean(value),
                                           quantile(value, c(.05,.25,.5,.75,.95)))), 
                                 by=list(gender,variable)]

setkeyv(athlete.summary, c("variable","gender"))
athlete.summary[, variable := NULL]

# print html tables
for(measure in athlete.summary[, unique(variable)]){
  message(measure)
  sub.table <- athlete.summary[variable == measure]
  sub.table[, variable:=NULL]
  setnames(sub.table, c("","# profiles", "Average","5th percentile","25th","50th","75th","95th"))
  print(xtable(sub.table, digits=0), type="html",html.table.attributes = "style='width:600px'", include.rownames=FALSE)
}

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Generate histogram by gender of each stat ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GenderHist <-function(variable, binwidth=1, limits, breaks, labels=breaks, xlab){
  ggplot(athletes[gender %in% c("Male", "Female")], 
         aes_string(x=variable, fill="gender")) +
    geom_histogram(aes(y=..density..),binwidth=binwidth, position="dodge")+
    geom_density(alpha=.4,adjust=3, color=NA) +
    scale_x_continuous(limits = limits,
                       breaks = breaks,
                       labels = labels) +
    scale_y_continuous(labels=percent) +
    labs(fill="Gender",x=xlab,y="% of Athletes") +
    theme_bw(base_size=18) +
    theme(legend.position = c(.9, .9))
}

plots <- list(list(variable = "age",
                   binwidth = 1,
                   limits   = c(15,65),
                   breaks   = seq(15,65,5),
                   labels   = seq(15,65,5),
                   xlab     = "Age"),
              list(variable = "height",
                   binwidth = 1,
                   limits   = c(57,81),
                   breaks   = seq(57,81,by=3),
                   labels   = c("4\'9\"","5\'","5\'3\"","5\'6\"","5\'9\"","6\'","6\'3\"","6\'6\"","6\'9\""),
                   xlab     = "Height (feet/in)"),
              list(variable = "weight",
                   limits   = c(75,325),
                   binwidth = 5,
                   breaks   = seq(75,325,25),
                   labels   = seq(75,325,25),
                   xlab     = "Weight (lbs)"),
              list(variable = "fran",
                   binwidth = 10,
                   limits   = c(90,1200),
                   breaks   = seq(120,1200,60),
                   labels   = seq(2,20,1),
                   xlab     = "Fran (minutes)"),
              list(variable = "helen",
                   binwidth = 10,
                   limits   = c(360,1200),
                   breaks   = seq(360,1200,60),
                   labels   = seq(6,20,1),
                   xlab     = "Helen (minutes)"),
              list(variable = "grace",
                   binwidth = 10,
                   limits   = c(0,840),
                   breaks   = seq(0,840,60),
                   labels   = seq(0,14,1),
                   xlab     = "Grace (minutes)"),
              list(variable = "fgonebad",
                   binwidth = 5,
                   limits   = c(100,500),
                   breaks   = seq(100,500,50),
                   labels   = seq(100,500,50),
                   xlab     = "Fight Gone Bad (reps)"),
              list(variable = "filthy50",
                   binwidth = 15,
                   limits   = c(720,2520),
                   breaks   = seq(720,2520,120),
                   labels   = seq(12,42,2),
                   xlab     = "Filthy Fifty (minutes)"),
              list(variable = "run400",
                   binwidth = 5,
                   limits   = c(40,200),
                   breaks   = seq(40,200,10),
                   labels   = seq(40,200,10),
                   xlab     = "400m Run (seconds)"),
              list(variable = "run5k",
                   binwidth = 15,
                   limits   = c(720,2520),
                   breaks   = seq(720,2520,120),
                   labels   = seq(12,42,2),
                   xlab     = "Run 5k (minutes)"),
              list(variable = "snatch",
                   binwidth = 5,
                   limits   = c(45,400),
                   breaks   = seq(45,400,25),
                   labels   = seq(45,400,25),
                   xlab     = "Snatch (lbs)"),
              list(variable = "candj",
                   binwidth = 5,
                   limits   = c(45,400),
                   breaks   = seq(45,400,25),
                   labels   = seq(45,400,25),
                   xlab     = "Clean & Jerk (lbs)"),
              list(variable = "backsq",
                   binwidth = 5,
                   limits   = c(50,550),
                   breaks   = seq(50,550,50),
                   labels   = seq(50,550,50),
                   xlab     = "Back Squat (lbs)"),
              list(variable = "pullups",
                   binwidth = 1,
                   limits   = c(0,80),
                   breaks   = seq(0,80,5),
                   labels   = seq(0,80,5),
                   xlab     = "Pull Ups"),
              list(variable = "deadlift",
                   binwidth = 10,
                   limits   = c(45,675),
                   breaks   = seq(0,675,50),
                   labels   = seq(0,675,50),
                   xlab     = "Deadlift"))


for(p in plots){
  ggsave(width=10, height=6,
         plot=GenderHist(variable = p$variable,
                         binwidth = p$binwidth,
                         limits   = p$limits,
                         breaks   = p$breaks,
                         labels   = p$labels,
                         xlab     = p$xlab),
         filename=paste("~/Desktop/",p$variable,".png",sep=""))
}
