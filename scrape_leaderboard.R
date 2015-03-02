library(data.table)
library(reshape2)
library(stringr)
library(rvest)

source("process_fns.R")
source("scrape_fns.R")
source("db_query.R")

pages <- QueryDB("SELECT * FROM leaderboard_pages WHERE retrieved_datetime IS NULL;")

for(i in 1:pages[,.N]){
  year     <- pages[i, year]
  division <- pages[i, division]
  stage    <- pages[i, stage]
  page     <- pages[i, pages]
  score.type <- "points"
  if(year == 14 & stage == 5) { 
    score.type <- "time"
  }
  
  lb.page <- GetLeaderboardPage(year, division, stage, page, score.type)
  
  if(!is.null(lb.page)) {
    # write new leaderboard records
    db.con <- dbConnect(RMySQL::MySQL(), 
                        dbname   = "crossfit",
                        user     = "crossfit",
                        password = "",
                        host     = "127.0.0.1")
    
    dbWriteTable(db.con, name = "leaderboard", value=lb.page, row.names = F, append=TRUE)
    
    QueryDB(paste0(
      "UPDATE leaderboard_pages
       SET retrieved_datetime = '",Sys.time(),"'
       WHERE year = ",year,"
       AND division = ",division,"
       AND stage = ", stage, "
       AND pages = ",page))
    
    dbDisconnect(db.con)
    
    message(Sys.time(), " Success! year: ",year," div: ",division," stage: ",stage," page: ",page)
  }
}
