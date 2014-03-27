library(httr)
library(XML)
library(data.table)
library(reshape2)
library(stringr)

boards <- data.table("division" = c(1,2),
                     "pages"     = c(937,597))

for(div in boards$division){
  for (i in 1:boards[division == div,pages]){
    resp <- GET(
      paste("http://games.crossfit.com/scores/leaderboard.php?stage=4&sort=0&page=",i,"&division=",div,"&region=0&numberperpage=100&competition=0&frontpage=0&expanded=0&year=14&full=1&showtoggles=0&hidedropdowns=1&showathleteac=1&is_mobile=1", sep="")
    )  
    
    if (resp$status_code==200){
      page    <- htmlParse(resp)
      lb.page <- data.table(readHTMLTable(page, 
                                          skip.rows=1,
                                          stringsAsFactors=FALSE)[[1]])
      
      setnames(lb.page, c("overall","name","wod1","wod2","wod3","wod4","wod5"))
      
      lb.page$url <- xpathSApply( page, "//td[@class='name']/a/@href")
      
      if(exists("leaderboard")){
        leaderboard <- rbind(leaderboard, lb.page)
      } else {
        leaderboard <- lb.page
      }
      if(i%%10 == 0){
        message(Sys.time(), " division ",div," i:",i)
        save(leaderboard, file=paste("data/leaderboard.RData", sep=""))  
      }
    }
  }    
}
save(leaderboard, file="data/leaderboard.RData")
