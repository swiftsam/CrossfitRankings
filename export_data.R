####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Export leaderboard and athlete data from DB to .RData
###
### Notes:
###
### Primary Creator(s): Sam Swift (samswift@gmail.com)
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(RMySQL)

source("db_query.R")

ExportWod <- function(year, stage){
  obj.name <- paste("leaderboard",year,stage, sep=".")
  leaderboard <- QueryDB(paste0("SELECT * FROM leaderboard WHERE year = ", year, " AND stage = ",stage))
  assign(obj.name, leaderboard)
  save(list = c(obj.name), 
       file=paste0("data/leaderboard.",year,".",stage,".RData"))
}

ExportAthletes <- function(){
  athletes <- QueryDB("SELECT * FROM athletes WHERE 1;")
  save(athletes, file="data/athletes.RData")
}

