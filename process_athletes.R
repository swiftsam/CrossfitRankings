# load("data/athletes.local.RData")
# athletes.local <- athletes
# load("data/athletes.finzi.RData")
# athletes.finzi <- athletes
# load("data/athletes.swiftnode.RData")
# athletes.swiftnode <- athletes
# load("data/athletes.gordo.RData")
# athletes.gordo <- athletes
# 
# athletes <- rbindlist(list(athletes.local,
#                            athletes.finzi,
#                            athletes.swiftnode,
#                            athletes.gordo))
# 
# athletes <- unique(athletes)
# save(athletes, file="data/athletes.raw.RData")

library(ggplot2)
library(data.table)

source("process_fns.R")

load("data/athletes.raw.RData")

#replace --'s with NA's
for (col in names(athletes)){
  athletes[(get(col)=="--"), (col) := NA]
}

# fix data types and formatting
athletes[, gender  := factor(gender, levels=c("Female","Male"))]
athletes[, age     := as.integer(age)]
athletes$height    <- unlist(lapply(athletes$height, DeUnitHeight))
athletes$weight    <- unlist(lapply(athletes$weight, DeUnitWeight))
athletes$fran      <- unlist(lapply(athletes$fran, MinSecToSec))
athletes$helen     <- unlist(lapply(athletes$helen, MinSecToSec))
athletes$grace     <- unlist(lapply(athletes$grace, MinSecToSec))
athletes$filthy50  <- unlist(lapply(athletes$filthy50, MinSecToSec))
athletes[,fgonebad := as.integer(fgonebad)]
athletes$run400    <- unlist(lapply(athletes$run400, MinSecToSec))
athletes$run5k     <- unlist(lapply(athletes$run5k, MinSecToSec))
athletes$candj     <- unlist(lapply(athletes$candj, DeUnitWeight))
athletes$snatch    <- unlist(lapply(athletes$snatch, DeUnitWeight))
athletes$deadlift  <- unlist(lapply(athletes$deadlift, DeUnitWeight))
athletes$backsq    <- unlist(lapply(athletes$backsq, DeUnitWeight))
athletes[,pullups  := as.integer(pullups)]

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

save(athletes, file="data/athletes.RData")
