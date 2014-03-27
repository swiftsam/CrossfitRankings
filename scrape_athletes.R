
library(httr)
library(XML)
library(data.table)
library(reshape2)
library(stringr)

if(file.exists("data/athlete_ids.RData")){
  load("data/athlete_ids.RData")  
} else {
  stop("Can't find ids data")  
}

if(file.exists("data/athletes.RData")){
  load("data/athletes.RData")  
}

if(exists("athletes")){
  ids <- setdiff(athlete.ids$id, athletes$id)
}

ScrapeAthletes <- function(start.i = 1, 
                           end.i   = length(ids)){
  # identify IDs in the desired range which are not already scraped
  ids.loop <- ids[start.i:end.i]

  # loop through IDs we don't have yet
  for(i in 1:length(ids.loop)){
    id <- ids.loop[i]
    #message(id)
    resp <- GET(paste("http://games.crossfit.com/athlete/",id,sep=""))  
    if (resp$status_code==200){
      page   <- htmlParse(resp)
      labels <- xpathSApply(page, "//div[@class='profile-details']/dl/dt", xmlValue)
      demo   <- xpathSApply(page, "//div[@class='profile-details']/dl/dd", xmlValue)
      stats  <- xpathSApply(page, "//div[@class='profile-stats']//td",     xmlValue)
      
      if(!is.null(demo)){        
        athlete <- data.table("id"        = id,
                              "region"    = paste(demo[which(labels == "Region:")],"",sep=""),
                              "team"      = paste(demo[which(labels == "Team:")],"", sep=""),
                              "affiliate" = paste(demo[which(labels == "Affiliate:")],"",sep=""),
                              "gender"    = paste(demo[which(labels == "Gender:")],"",sep=""),
                              "age"       = paste(demo[which(labels == "Age:")],"",sep=""),
                              "height"    = paste(demo[which(labels == "Height:")],"",sep=""),
                              "weight"    = paste(demo[which(labels == "Weight:")],"",sep=""),
                              "fran"      = stats[2],
                              "helen"     = stats[4],
                              "grace"     = stats[6],
                              "filthy50"  = stats[8],
                              "fgonebad"  = stats[10],
                              "run400"    = stats[12],
                              "run5k"     = stats[14],
                              "candj"     = stats[16],
                              "snatch"    = stats[18],
                              "deadlift"  = stats[20],
                              "backsq"    = stats[22],
                              "pullups"   = stats[24])
        if(exists("athletes")){
          athletes <- rbind(athletes, athlete)
        } else {
          athletes <- athlete
        }
        if(i%%100 == 0){
          message(Sys.time(), " done with i:", i," id:",id, " total records:",nrow(athletes))
          save(athletes, file=paste("data/athletes.RData", sep=""))  
        }
      }
    }
  }
  save(athletes, file=paste("data/athletes.RData", sep="")) 
}
