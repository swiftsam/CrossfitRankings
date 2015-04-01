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

ExportWod <- function(year, stage = NULL, file.type = c("csv","RData","zip")){
  if(is.null(stage)){
    obj.name  <- paste("leaderboard",year, sep=".")
    query     <- paste0("SELECT * FROM leaderboard WHERE year = ", year)
  } else {
    obj.name  <- paste("leaderboard",year,stage, sep=".")
    query     <- paste0("SELECT * FROM leaderboard WHERE year = ", year, " AND stage = ",stage)
  }
  file.path   <- paste0("data/",obj.name)
  
  leaderboard <- QueryDB(query)
  assign(obj.name, leaderboard)
  if("RData" %in% file.type){
    save(list = c(obj.name), file=paste0(file.path,".RData"))
  }
  if("csv" %in% file.type){
    write.csv(x         = get(obj.name),
              file      = paste0(file.path, ".csv"),
              row.names = F)
  }
  if("zip" %in% file.type){
    rm(obj.name)
    system(paste0("zip ",file.path,".zip ", file.path,".csv"))
  }
}

ExportAthletes <- function(file.type = ".RData"){
  athletes <- QueryDB("SELECT * FROM athletes WHERE 1;")
  
  if(file.type == ".RData"){
    save(athletes, file = "data/athletes.RData")
  } else if(file.type == ".csv"){
    write.csv(athletes, "data/athletes.csv", row.names = F)
  }
}

