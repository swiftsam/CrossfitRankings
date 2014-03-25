
library(httr)
library(XML)
library(data.table)
library(reshape2)
library(stringr)

load("data/leaderboard.RData")

urls <- sort(unique(scores.bind$url))

for(i in 1:length(urls)){
  url <- urls[i]
  url.vector <- strsplit(url,"/")[[1]]
  id  <- url.vector[length(url.vector)]
  resp <- GET(url)  
  if (resp$status_code==200){
    page  <- htmlParse(resp)
    
    labels <- xpathSApply(page, "//div[@class='profile-details']/dl/dt", xmlValue)
    demo   <- xpathSApply(page, "//div[@class='profile-details']/dl/dd", xmlValue)
    stats  <- xpathSApply(page, "//div[@class='profile-stats']//td",     xmlValue)
    
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
      if(file.exists(paste("data/athletes_",i-2,".RData", sep=""))){
        file.remove(paste("data/athletes_",i-2,".RData", sep=""))
      }
      save(athletes, file=paste("data/athletes_",i,".RData", sep=""))      
    }
  }
}

save(athletes, file="data/athletes.RData")

