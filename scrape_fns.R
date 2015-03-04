library(data.table)
library(reshape2)
library(stringr)
library(rvest)

GetLeaderboardPage <- function(year = 14, division = 1, stage = 5, page = 1, score.type = "points"){
  
  
  html.page <- html(paste0(
    "http://games.crossfit.com/scores/leaderboard.php?stage=",round(stage),"&sort=",round(stage),"&page=",page,
    "&division=",division,"&region=0&numberperpage=100&competition=0&frontpage=0",
    "&expanded=0&year=",year,"&full=1&showtoggles=0&hidedropdowns=1",
    "&showathleteac=1&is_mobile=1"))  
  
  #athletes     <- html_text(html_nodes(html.page, "td.name"))
  athlete.urls <- html_attr(html_nodes(html.page, "td.name a"), "href")
  if(length(athlete.urls) == 0){
    return(NULL)
  }
  
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
  if(stage == 1.1){
    scores <- scores[paste0("Workout01A")]
  } else {
    scores <- scores[paste0("Workout0",stage)]
  }
  
  leaderboard[, rank_score := scores]
  
  leaderboard[rank_score == "--\n                No score", rank_score := NA]
  leaderboard[,rank  := as.integer(sub(" .*", "", rank_score))]
  leaderboard[,score_pre := unlist(str_split(rank_score, "[\\(\\)]"))[2], by=athlete_id]
  leaderboard[,scaled := as.integer(substring(rank_score, 
                                              nchar(rank_score)-3,
                                              nchar(rank_score)) == " - s")]
  
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
    "howlong"    = paste(howlong, collapse="|"),
    "retrieved_datetime" = Sys.time())
  
  return(athlete)
}