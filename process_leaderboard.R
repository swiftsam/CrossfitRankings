load("data/leaderboard.RData")

leaderboard$wod5 <- NULL

leaderboard <- data.table(melt(leaderboard, id.vars="name", value.name="rank_score", variable.name="wod"))
leaderboard[,rank  := as.integer(sub(" .*", "", rank_score))]
leaderboard[,score := as.integer(gsub(".*\\(|\\).*", "", rank_score))]
