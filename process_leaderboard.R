library(data.table)
library(reshape2)
library(stringr)

load("data/leaderboard.RData")

leaderboard$id <-  as.numeric(
                          unlist(
                            lapply(
                              leaderboard$url, 
                              function(url){ 
                                url.vector <- strsplit(url,"/")[[1]]
                                id <- url.vector[length(url.vector)]
                                return(id)
                              })))

athlete.ids <- leaderboard[,list(id,name,url)]
leaderboard$url <- NULL
leaderboard$name <- NULL
save(athlete.ids, file="data/athlete_ids.RData")

leaderboard <- data.table(melt(leaderboard, id.vars="id", value.name="rank_score", variable.name="wod"))

leaderboard[rank_score == "--\n                No score", rank_score:=NA]
leaderboard[,rank  := as.integer(sub(" .*", "", rank_score))]
leaderboard[,score := as.integer(gsub(".*\\(|\\).*", "", rank_score))]

save(leaderboard, file="data/leaderboard.RData")





