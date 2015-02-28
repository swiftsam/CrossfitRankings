library(data.table)
library(RMySQL)
library(rvest)

GetPageCount <- function(year = 14, division = 1, stage = 0){
  #get page count
  
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

# generate table of leaderboard pages
boards <- rbindlist(list(
  data.table(year = 12, division = 1, stage = 0:5),
  data.table(year = 12, division = 2, stage = 0:5),
  data.table(year = 13, division = 1, stage = 0:5),
  data.table(year = 13, division = 2, stage = 0:5),
  data.table(year = 14, division = 1, stage = 0:5),
  data.table(year = 14, division = 2, stage = 0:5)))

boards[, id :=1:.N]
boards[, pages := GetPageCount(year = year,division = division,stage = stage), by=id]

ExpandBoard <- function(i){  
  data.table(year = boards[i, year],
             division = boards[i,division],
             stage = boards[i, stage],
             pages = 1:boards[i,pages])
}

leaderboard.pages <- rbindlist(lapply(boards[, id], ExpandBoard))

leaderboard.pages[, retrieved_datetime := NA]
#leaderboard.pages[year == 14, retrieved_datetime := Sys.time() - 365*24*60*60]

# write tables
db.con <- dbConnect(RMySQL::MySQL(), 
                    dbname   = "crossfit",
                    user     = "crossfit",
                    password = "",
                    host     = "127.0.0.1")

#dbWriteTable(db.con, name = "athletes",    value=athletes,    row.names = F)
#dbWriteTable(db.con, name = "leaderboard", value=leaderboard, row.names = F)
dbWriteTable(db.con, name = "leaderboard_pages", value=leaderboard.pages, row.names=F)
dbDisconnect(db.con)


