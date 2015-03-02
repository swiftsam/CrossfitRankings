library(data.table)
library(RMySQL)
library(rvest)

source("db_query.R")

GetPageCount <- function(year = 14, division = 1, stage = 0){
  if(year == 15 & stage == 1.1) {
    stage <- 1
  }
  url <- paste0(
    "http://games.crossfit.com/scores/leaderboard.php?stage=",stage,"&sort=",stage,
    "&page=0&division=",division,"&region=0&numberperpage=100&competition=0&frontpage=0",
    "&expanded=0&year=",year,"&full=1&showtoggles=0&hidedropdowns=1",
    "&showathleteac=1&is_mobile=1")
  #message(url)
  html.page <- html(url)
  page.count <- as.integer(html_text(html_nodes(html.page, "div#leaderboard-pager a.button")[[1]]))
  message(page.count)
  return(page.count)
}

ExpandBoard <- function(i, boards){  
  data.table(year = boards[i, year],
             division = boards[i,division],
             stage = boards[i, stage],
             pages = 1:boards[i,pages])
}

InitAllYears <-function(){
  # generate table of leaderboard pages
  boards <- rbindlist(list(
    data.table(year = 12, division = 1, stage = 0:5),
    data.table(year = 12, division = 2, stage = 0:5),
    data.table(year = 13, division = 1, stage = 0:5),
    data.table(year = 13, division = 2, stage = 0:5),
    data.table(year = 14, division = 1, stage = 0:5),
    data.table(year = 14, division = 2, stage = 0:5),
    data.table(year = 15, division = 1, stage = c(1,1.1)),
    data.table(year = 15, division = 2, stage = c(1,1.1))))
  
  boards[, id :=1:.N]
  boards[, pages := GetPageCount(year = year,division = division,stage = stage), by=id]
  
  leaderboard.pages <- rbindlist(lapply(boards[, id], ExpandBoard, boards = boards))
  
  leaderboard.pages[, retrieved_datetime := NA]
  
  # write tables
  db.con <- dbConnect(RMySQL::MySQL(), 
                      dbname   = "crossfit",
                      user     = "crossfit",
                      password = "",
                      host     = "127.0.0.1")
  
  dbWriteTable(db.con, name = "leaderboard_pages", value=leaderboard.pages, row.names=F)
  dbDisconnect(db.con)
}

Init15Wod <- function(stage){
  boards <- data.table(year = 15, division = 1:2, stage = stage)
  
  boards[, id :=1:.N]
  boards[, pages := GetPageCount(year = year,division = division,stage = stage), by=id]
  
  leaderboard.pages <- rbindlist(lapply(boards[, id], ExpandBoard, boards))
  
  leaderboard.pages[, retrieved_datetime := NA]
  
  # write tables
  db.con <- dbConnect(RMySQL::MySQL(), 
                      dbname   = "crossfit",
                      user     = "crossfit",
                      password = "",
                      host     = "127.0.0.1")
  
  QueryDB(paste0("DELETE FROM leaderboard_pages WHERE year = 15 AND stage = ", stage))
  QueryDB(paste0("DELETE FROM leaderboard WHERE year = 15 AND stage = ", stage))
  
  dbWriteTable(db.con, name = "leaderboard_pages", value=leaderboard.pages, row.names=F, append=TRUE)
  dbDisconnect(db.con)
  
}




