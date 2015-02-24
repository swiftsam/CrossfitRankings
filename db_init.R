library(data.table)
library(RMySQL)

# load saved results from 2014 scrape of leaderboard pages
load("data/leaderboard.RData")
leaderboard[, year:= 14]
leaderboard[, retrieved_datetime := Sys.time() - 365*24*60*60]

# load saved results from 2014 scrape of athelete profile pages
load("data/athletes.RData")
athletes[, retrieved_datetime := Sys.time() - 365*24*60*60]

# generate table of leaderboard pages
leaderboard.pages <- rbindlist(list(
  data.table(year = 12, division = 1, page = 1:251),
  data.table(year = 12, division = 2, page = 1:143),
  data.table(year = 13, division = 1, page = 1:522),
  data.table(year = 13, division = 2, page = 1:327),
  data.table(year = 14, division = 1, page = 1:803),
  data.table(year = 14, division = 2, page = 1:521)))

leaderboard.pages[, retrieved_datetime := NA]
#leaderboard.pages[year == 14, retrieved_datetime := Sys.time() - 365*24*60*60]

# write tables
db.con <- dbConnect(RMySQL::MySQL(), 
                    dbname   = "crossfit",
                    user     = "root",
                    password = "",
                    host     = "127.0.0.1")

dbWriteTable(db.con, name = "athletes",    value=athletes,    row.names = F)
dbWriteTable(db.con, name = "leaderboard", value=leaderboard, row.names = F)
dbWriteTable(db.con, name = "leaderboard_pages", value=leaderboard.pages, row.names=F)
dbDisconnect(db.con)


