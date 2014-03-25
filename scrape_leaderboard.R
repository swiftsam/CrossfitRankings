
library(httr)
library(XML)
library(data.table)
library(reshape2)
library(stringr)

scores.bind <- data.table()

for(i in 1:877){
  resp <- GET(
    paste("http://games.crossfit.com/scores/leaderboard.php?stage=4&sort=0&page=",i,"&division=1&region=0&numberperpage=100&competition=0&frontpage=0&expanded=0&year=14&full=1&showtoggles=0&hidedropdowns=1&showathleteac=1&is_mobile=1", sep="")
  )  
  
  if (resp$status_code==200){
    page  <- htmlParse(resp)
    table <- data.table(readHTMLTable(page, 
                                      skip.rows=1,
                                      stringsAsFactors=FALSE)[[1]])
    
    setnames(table, c("overall","name","wod1","wod2","wod3","wod4","wod5"))
    table$wod5 <- NULL
    
    scores <- data.table(melt(table, id.vars="name", value.name="rank_score", variable.name="wod"))
    scores[,rank  := as.integer(sub(" .*", "", rank_score))]
    scores[,score := as.integer(gsub(".*\\(|\\).*", "", rank_score))]
    
    scores$links <- xpathSApply( page, "//td[@class='name']/a/@href")
    
    scores.bind <- rbindlist(list(scores.bind, scores))    
    message(i, " : ", nrow(scores.bind))
    save(scores, file=paste("data/table_",i,".RData",sep=""))
  }
}

save(scores.bind, file="data/leaderboard.RData")
