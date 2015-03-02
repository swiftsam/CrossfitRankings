library(data.table)
library(reshape2)
library(stringr)
library(rvest)
library(XML)

source("process_fns.R")
source("scape_fns.R")
source("db_query.R")

athletes.all <- QueryDB("SELECT DISTINCT athlete_id
                    FROM leaderboard;")


athletes.done <- QueryDB("SELECT athlete_id
                    FROM athletes;")

athletes <- athletes.all[!athlete_id %in% athletes.done[,athlete_id], athlete_id]

message(Sys.time(), " starting with ", length(athletes), " athletes to scrape")

for(athlete_id in athletes){
  message(Sys.time(), " Athlete: ", athlete_id)
  
    athlete <- GetAthlete(athlete_id)
  
  if(!is.null(athlete)) {
    # write new leaderboard records
    db.con <- dbConnect(RMySQL::MySQL(), 
                        dbname   = "crossfit",
                        user     = "crossfit",
                        password = "",
                        host     = "127.0.0.1")
    
    dbWriteTable(db.con, name = "athletes", value=athlete, row.names = F, append=TRUE)
    
    dbDisconnect(db.con)
  }
}

