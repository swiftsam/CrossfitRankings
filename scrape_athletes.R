library(data.table)
library(reshape2)
library(stringr)
library(rvest)
library(XML)

library(foreach)
library(doMC)
doMC::registerDoMC()

source("process_fns.R")
source("scrape_fns.R")
source("db_query.R")

ScrapeAtheletesByYear <- function(year){
  athletes.year <- QueryDB(paste0("SELECT DISTINCT athlete_id
                                  FROM leaderboard
                                  WHERE year = ", year))
  
  athletes.db <- QueryDB("SELECT athlete_id
                          FROM athletes;")
  
  n.year    <- athletes.year[, length(unique(athlete_id))]
  n.scraped <- athletes.year[athlete_id %in% athletes.db[, athlete_id], length(unique(athlete_id))]
  n.todo    <- athletes.year[!athlete_id %in% athletes.db[, athlete_id], length(unique(athlete_id))]

  message(Sys.time(), " ScapeAtheletes: ", n.year, " athletes competed in 20", year)
  message(Sys.time(), " ScapeAtheletes: ", n.scraped, " atheletes in the DB")
  message(Sys.time(), " ScapeAtheletes: ", n.todo, " atheletes left to scrape")
  message(Sys.time(), " ScapeAtheletes: ", round((n.scraped / n.year)*100), "% complete")
  
  athletes.todo <- athletes.year[!athlete_id %in% athletes.db[, athlete_id], unique(athlete_id)]
    
  foreach(i = 1:length(athletes.todo)) %dopar% {
    
    db.con <- dbConnect(RMySQL::MySQL(), 
                        dbname   = "crossfit",
                        user     = "crossfit",
                        password = "",
                        host     = "127.0.0.1")
    athlete_id <- athletes.todo[i]
    message(Sys.time(), " Athlete: ", athlete_id)
    
    athlete <- GetAthlete(athlete_id)
    
    if(!is.null(athlete)) {
      dbWriteTable(db.con, name = "athletes", value=athlete, row.names = F, append=TRUE)
    }
    
    dbDisconnect(db.con)
  }
}

