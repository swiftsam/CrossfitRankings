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
# save(athletes, file="data/athletes.RData")

library(ggplot2)
library(data.table)

load("data/athletes.RData")

athletes[, age:=as.integer(age)]
athletes[weight == "--", weight:= ""]
athletes[, weight_unit := substr(weight, nchar(weight)-2, nchar(weight))]

ggplot(athletes, aes(age)) +
  geom_histogram(breaks=14:55)

table(athletes$gender)
