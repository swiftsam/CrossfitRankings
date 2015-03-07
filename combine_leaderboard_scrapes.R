load("data/leaderboard.15.RData",verbose = TRUE)
leaderboard.15a <- leaderboard

load("data/leaderboard.15b.RData", verbose = TRUE)

leaderboard.meta <- rbind(leaderboard.15a, leaderboard15.b)
leaderboard.meta <- leaderboard.meta[order(retrieved_datetime),]

leaderboard.meta[, .N, by=list(pull_date = as.Date(retrieved_datetime))]

setkeyv(leaderboard.meta, c("athlete_id", "stage", "score"))

leaderboard.meta <- unique(leaderboard.meta, fromLast=TRUE, by = c("athlete_id", "stage","score"))
leaderboard.meta[, .N, by=list(pull_date = as.Date(retrieved_datetime))]

leaderboard <- leaderboard.meta

save(leaderboard,file =  "data/leaderboard.combined.pull.RData")
