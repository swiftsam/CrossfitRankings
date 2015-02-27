library(RMySQL)

QueryDB <- function(query       = NULL, 
                    db          = "crossfit",
                    silent      = TRUE){
  
  time.start = Sys.time()
  
  # create database connection
  db.con <- dbConnect(RMySQL::MySQL(), 
                      dbname   = db,
                      user     = "root",
                      password = "",
                      host     = "127.0.0.1")
  # run query
  result <- dbGetQuery(db.con, query)
  
  # close connection to DB
  dbDisconnect(db.con)
  
  result <- data.table(result)
  
  # report time elapsed
  if(!silent){
    time.end   = Sys.time()
    message(Sys.time(), " QueryDB: query completed in ", 
            round(difftime(time.end, time.start, units="mins"), 3), 
            " minutes")
  }
  
  # return query results
  return(result)
}
