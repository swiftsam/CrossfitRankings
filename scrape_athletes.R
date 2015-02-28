library(data.table)
library(reshape2)
library(stringr)
library(rvest)
library(XML)

source("process_fns.R")
source("db_query.R")

GetAthlete <- function(athlete_id){
  
  profile.page <- html(paste0("http://games.crossfit.com/athlete/",athlete_id))
  
  if(html_text(html_nodes(profile.page, "h2#page-title")) == "Athlete: Not found"){
    return(data.table("athlete_id" = athlete_id))
  }

  labels  <- html_text(html_nodes(profile.page, "div.profile-details dl dt"))
  demo    <- html_text(html_nodes(profile.page, "div.profile-details dl dd"))
  stats   <- html_text(html_nodes(profile.page, "div.profile-stats td"))
  history    <- html_nodes(profile.page, "div.history")
  
  eat        <- c(html_text(html_nodes(history[[1]], "h4")),"")
  train      <- c(html_text(html_nodes(history[[2]], "h4")),"")
  background <- c(html_text(html_nodes(history[[3]], "h4")),"")
  experience <- c(html_text(html_nodes(history[[4]], "h4")),"")
  schedule   <- c(html_text(html_nodes(history[[5]], "h4")),"")
  howlong    <- c(html_text(html_nodes(history[[6]], "h4")),"")
  
  
  athlete <- data.table(
    "athlete_id" = athlete_id,
    "name"       = substring(html_text(html_nodes(profile.page, "h2#page-title")),10),
    "region"     = paste(demo[which(labels == "Region:")],"",sep=""),
    "team"       = paste(demo[which(labels == "Team:")],"", sep=""),
    "affiliate"  = paste(demo[which(labels == "Affiliate:")],"",sep=""),
    "gender"     = paste(demo[which(labels == "Gender:")],"",sep=""),
    "age"        = paste(demo[which(labels == "Age:")],"",sep=""),
    "height"     = DeUnitHeight(paste(demo[which(labels == "Height:")],"",sep="")),
    "weight"     = DeUnitWeight(paste(demo[which(labels == "Weight:")],"",sep="")),
    "fran"       = MinSecToSec(stats[2]),
    "helen"      = MinSecToSec(stats[4]),
    "grace"      = MinSecToSec(stats[6]),
    "filthy50"   = MinSecToSec(stats[8]),
    "fgonebad"   = as.integer(stats[10]),
    "run400"     = MinSecToSec(stats[12]),
    "run5k"      = MinSecToSec(stats[14]),
    "candj"      = DeUnitWeight(stats[16]),
    "snatch"     = DeUnitWeight(stats[18]),
    "deadlift"   = DeUnitWeight(stats[20]),
    "backsq"     = DeUnitWeight(stats[22]),
    "pullups"    = as.integer(stats[24]),
    "eat"        = paste(eat, collapse="|"),
    "train"      = paste(train, collapse="|"),
    "background" = paste(background, collapse="|"),
    "experience" = paste(experience, collapse="|"),
    "schedule"   = paste(schedule, collapse="|"),
    "howlong"    = paste(howlong, collapse="|"))
  
  return(athlete)
}

athletes.all <- QueryDB("SELECT DISTINCT athlete_id
                    FROM leaderboard;")


athletes.done <- QueryDB("SELECT athlete_id
                    FROM athletes;")

athletes <- athletes.all[!athlete_id %in% athletes.done[,athlete_id], athlete_id]

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

