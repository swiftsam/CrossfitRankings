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

DeUnitWeight <- function(str.val){
  if(is.na(str.val)) {return(NA)}
  unit  <- substr(str.val, 
                  nchar(str.val)-1, 
                  nchar(str.val))
  value <- as.numeric(substr(str.val,
                             1, nchar(str.val)-3))
  value <- switch(unit,
                  kg = value * 2.20462,
                  lb = value,
                  NA)
  return(value)
}

DeUnitHeight <- function(str.val){
  if(is.na(str.val)) {return(NA)}
  n.char <- nchar(str.val)
  if(substr(str.val,2,2) == "'"){
    feet <- as.numeric(substr(str.val,1,1))
    inch <- as.numeric(substr(str.val,3,n.char-1))
    return(feet*12 + inch)
  } else if(substr(str.val, n.char-1, n.char) == "cm"){
    cm <- substr(str.val, 1, n.char-3)
    return(as.numeric(cm) * 0.393701)
  } else {
    return(NA)
  }

}

MinSecToSec <- function(str.val){
  if(is.na(str.val) | str.val == "0:00") {return(NA)}
  vec.val <- strsplit(str.val,split=":")[[1]]
  if(length(vec.val) == 2){
    return(as.numeric(vec.val[1])*60 + as.numeric(vec.val[2]))
  } else {
    return(NA)
  }
}

load("data/athletes.raw.RData")

#replace --'s with NA's
for (col in names(athletes)){
  athletes[(get(col)=="--"), (col) := NA]
}

# fix data types and formatting
athletes[, gender  := factor(gender, levels=c("Male","Female"))]
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
athletes[1      >= pullups  | pullups  >= 150,    pullups:= NA]

save(athletes, file="data/athletes.RData")
