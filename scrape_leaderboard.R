library(data.table)
library(reshape2)
library(stringr)
library(rvest)

source("process_fns.R")
source("db_query.R")

GetLeaderboardPage <- function(year = 14, division = 1, stage = 5, page = 1, score.type = "points"){

  html.page <- html(paste0(
  "http://games.crossfit.com/scores/leaderboard.php?stage=",stage,"&sort=",stage,"&page=",page,
  "&division=",division,"&region=0&numberperpage=100&competition=0&frontpage=0",
  "&expanded=0&year=",year,"&full=1&showtoggles=0&hidedropdowns=1",
  "&showathleteac=1&is_mobile=1"))  
  
  #athletes     <- html_text(html_nodes(html.page, "td.name"))
  athlete.urls <- html_attr(html_nodes(html.page, "td.name a"), "href")
  
  GetAthleteID <- function(url){ 
    url.vector <- strsplit(url,"/")[[1]]
    id <- url.vector[length(url.vector)]
    return(id)
  }
  
  athlete.ids  <- as.integer(unlist(lapply(athlete.urls, GetAthleteID)))
  
  leaderboard <- data.table(year       = year,
                            division   = division,
                            stage      = stage,
                            athlete_id = athlete.ids)
  
  if(stage == 0){
    leaderboard[, rank := 0]
    leaderboard[, score := 0]
    leaderboard[, retrieved_datetime := Sys.time()]
    setorderv(leaderboard, c("year","division","stage","athlete_id","rank","score","retrieved_datetime"))
    setkeyv(leaderboard, c("year", "division","stage","athlete_id"))
    return(leaderboard)
  }
  
  scores <- html_table(html.page, fill=TRUE)[[1]]
  scores <- scores[paste0("Workout0",stage)]
  
  leaderboard[, rank_score := scores]
  
  leaderboard[rank_score == "--\n                No score", rank_score := NA]
  leaderboard[,rank  := as.integer(sub(" .*", "", rank_score))]
  leaderboard[,score_pre := unlist(str_split(rank_score, "[\\(\\)]"))[2], by=athlete_id]

  if(score.type == "points"){
    leaderboard[, score := as.integer(score_pre), by=athlete_id]
  } else if(score.type == "time"){
    leaderboard[, score := as.integer(MinSecToSec(score_pre)), by=athlete_id]
  }

  leaderboard[, rank_score := NULL]
  leaderboard[, score_pre  := NULL]
  leaderboard[, retrieved_datetime := Sys.time()]

  setorderv(leaderboard, c("year","division","stage","athlete_id","rank","score","retrieved_datetime"))
  setkeyv(leaderboard, c("year", "division","stage","athlete_id"))
  
  return(leaderboard)
}

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

leaderboard <- QueryDB("SELECT * FROM leaderboard;")