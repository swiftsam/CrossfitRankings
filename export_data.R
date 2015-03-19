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

ExportWod <- function(year, stage = NULL, file.type = ".RData"){
  if(is.null(stage)){
    obj.name  <- paste("leaderboard",year, sep=".")
    query     <- paste0("SELECT * FROM leaderboard WHERE year = ", year)
  } else {
    obj.name  <- paste("leaderboard",year,stage, sep=".")
    query     <- paste0("SELECT * FROM leaderboard WHERE year = ", year, " AND stage = ",stage)
  }
  file.name   <- paste0("data/",obj.name,file.type)
  
  leaderboard <- QueryDB(query)
  assign(obj.name, leaderboard)
  if(file.type == ".RData"){
    save(list = c(obj.name), file=file.name)
  } else if(file.type == ".csv"){
    write.csv(x         = get(obj.name),
              file      = file.name,
              row.names = F)
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

