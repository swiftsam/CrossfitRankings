library(ggplot2)
library(scales)
library(data.table)

load("data/athletes.RData")
load("data/leaderboard.RData")

### Histograms by profile stats
GenderHist <-function(variable, binwidth=1, limits, breaks, labels=breaks, xlab){
  ggplot(athletes, aes_string(x=variable, fill="gender")) +
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

plots <- list(list(variable = "height",
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
              list(variable = "pullups",
                   binwidth = 1,
                   limits   = c(0,80),
                   breaks   = seq(0,80,5),
                   labels   = seq(0,80,5),
                   xlab     = "Pull Ups"),
              list(variable = "deadlift",
                   binwidth = 10,
                   limits   = c(45,675),
                   breaks   = seq(0,675,25),
                   labels   = seq(0,675,25),
                   xlab     = "Deadlift"))


for(p in plots){
#   p <- plots[[10]]
#   p <- list(variable = "pullups",
#            binwidth = 1,
#            limits   = c(0,80),
#            breaks   = seq(0,80,5),
#            labels   = seq(0,80,5),
#            xlab     = "Pull Ups")
  ggsave(width=10, height=6,
         plot=GenderHist(variable = p$variable,
                   binwidth = p$binwidth,
                   limits   = p$limits,
                   breaks   = p$breaks,
                   labels   = p$labels,
                   xlab     = p$xlab),
         filename=paste("~/Desktop/",p$variable,".png",sep=""))
}
