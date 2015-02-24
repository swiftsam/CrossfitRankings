library(httr)
library(XML)
library(data.table)
library(reshape2)
library(stringr)

source("process_fns.R")
source("db_query.R")

wod.types <- data.table(year = c(rep(12,6), rep(13,6), rep(14,6), rep(15,6)),
                        wod  = rep(c(paste0("wod",1:5),"overall"),4),
                        wod_type = c(c(rep("points", 5),"ranksum"),
                                     c(rep("points", 5),"ranksum"),
                                     c(rep("points",4),"time","ranksum"),
                                     c(rep(NA,5),"ranksum")))

GetLeaderboardPage <- function(year = 14, division = 1, page = 1){
  resp <- GET(paste0(
    "http://games.crossfit.com/scores/leaderboard.php?stage=5&sort=0&page=",page,
    "&division=",division,"&region=0&numberperpage=100&competition=0&frontpage=0",
    "&expanded=0&year=",year,"&full=1&showtoggles=0&hidedropdowns=1",
    "&showathleteac=1&is_mobile=1"))  
  
  if (resp$status_code==200){
    html.page <- htmlParse(resp)
    lb.page   <- data.table(readHTMLTable(html.page, 
                                          skip.rows=1,
                                          stringsAsFactors=FALSE)[[1]])
    
    setnames(lb.page, c("overall","name","wod1","wod2","wod3","wod4","wod5"))
    
    lb.page$url <- xpathSApply(html.page, "//td[@class='name']/a/@href")
    
    lb.page$athlete_id <-  as.numeric(
      unlist(
        lapply(
          lb.page$url, 
          function(url){ 
            url.vector <- strsplit(url,"/")[[1]]
            id <- url.vector[length(url.vector)]
            return(id)
          })))
    
    lb.page[, url := NULL]
    lb.page[, name := NULL]
    
    
    lb.long <- data.table(melt(lb.page, 
                                   id.vars       = "athlete_id", 
                                   value.name    = "rank_score", 
                                   variable.name = "wod"))
    lb.long[, year := year]
    lb.long[, division := division]
    
    lb.long[rank_score == "--\n                No score", rank_score:=NA]
    lb.long[,rank  := as.integer(sub(" .*", "", rank_score))]
    lb.long[,score_pre :=gsub(".*\\(|\\).*", "", rank_score)]
    
    lb.long <- merge(lb.long, wod.types, by=c("year","wod"))
    
    lb.long[wod_type == "time", score := as.integer(MinSecToSec(score_pre)), by=athlete_id]
    lb.long[wod_type %in% c("points","ranksum"), score := as.integer(score_pre)]
    
    lb.long[, rank_score := NULL]
    lb.long[, score_pre  := NULL]
    lb.long[, wod_type   := NULL]
    
    lb.long[, retrieved_datetime := Sys.time()]
    
    setorderv(lb.long, c("year","division","wod","athlete_id","rank","score","retrieved_datetime"))
    setkeyv(lb.long, c("year", "division","wod","athlete_id"))
    
    return(lb.long)
  } else {
    message(Sys.time(), " Failed to GET year: ",year," div: ",division," page: ",page)
    return(NULL)
  }
}

pages <- QueryDB("SELECT * FROM leaderboard_pages WHERE retrieved_datetime IS NULL;")

for(i in 1:pages[,.N]){
  year     <- pages[i, year]
  division <- pages[i, division]
  page     <- pages[i, page]
  
  lb.page <- GetLeaderboardPage(year, division, page)
  
  if(!is.null(page)) {
    # write new leaderboard records
    db.con <- dbConnect(RMySQL::MySQL(), 
                        dbname   = "crossfit",
                        user     = "root",
                        password = "",
                        host     = "127.0.0.1")
    
    dbWriteTable(db.con, name = "leaderboard", value=lb.page, row.names = F, append=TRUE)
    
    QueryDB(paste0(
      "UPDATE leaderboard_pages
       SET retrieved_datetime = '",Sys.time(),"'
       WHERE year = ",year,"
       AND division = ",division,"
       AND page = ",page))
    
    dbDisconnect(db.con)
    
    message(Sys.time(), " Success! year: ",year," div: ",division," page: ",page)
  }
}

