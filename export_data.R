library(data.table)
library(RMySQL)

source("db_query.R")

leaderboard.15 <- QueryDB("SELECT * FROM leaderboard WHERE year = 15;")
save(leaderboard.15, file="data/leaderboard.15.RData")
rm(leaderboard.15)

leaderboard.14 <- QueryDB("SELECT * FROM leaderboard WHERE year = 14;")
save(leaderboard.14, file="data/leaderboard.14.RData")
rm(leaderboard.14)

leaderboard.13 <- QueryDB("SELECT * FROM leaderboard WHERE year = 13;")
save(leaderboard.13, file="data/leaderboard.13.RData")
rm(leaderboard.13)

leaderboard.12 <- QueryDB("SELECT * FROM leaderboard WHERE year = 12;")
save(leaderboard.12, file="data/leaderboard.12.RData")
rm(leaderboard.12)

athletes <- QueryDB("SELECT * FROM athletes WHERE 1;")
save(athletes, file="data/athletes.RData")
rm(athletes)
