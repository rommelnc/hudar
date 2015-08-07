tweetsToMongoBatch = function (file.name = NULL, ns = NULL, host = "localhost", username = "", 
          password = "", verbose = TRUE) 
{
  if (!is.null(ns)) {
    require(rmongodb)
  }
  results.list <- readTweets(file.name, verbose = FALSE)
  db <- strsplit(ns, "\\.")[[1]][1]
  coll <- strsplit(ns, "\\.")[[1]][2]
  if (verbose == TRUE) {
    message("Storing tweets in collection '", coll, "' of database '", 
            db, "' in MongoDB\n")
  }
  mongo <- mongo.create(host = host, username = username, 
                        password = password, db = db)
  if (mongo.get.err(mongo) != 0) {
    stop("Error in connection to MongoDB")
  }
  
  if (length(results.list) < 1) {
    return
  }
  
  pb <- txtProgressBar(min = 1, max = length(results.list), 
                       style = 3)
#   i <- 1
  for (i in 1:length(results.list)) {
    fields <- names(results.list[[i]])
    if ("text" %in% fields) {
      results.list[[i]][["_id"]] <- results.list[[i]][["id_str"]]
      results.list[[i]][["timestamp"]] <- format.twitter.date(results.list[[i]][["created_at"]])
      results.list[[i]][["random_number"]] <- runif(1, 0, 1)
      ##mongo.insert(mongo = mongo, ns = ns, results.list[[i]])
    }
#     i <- i + 1
    setTxtProgressBar(pb, i)
  }
#   print(head(results.list))
  res <- lapply(results.list, function(x) mongo.bson.from.list(x))
#   print(head(res))
  ## It seems the limit for batch operation is 1k elements, so let's split it up
  t.list = split(res, ceiling(seq_along(res)/1000))

  if (verbose == TRUE) {
    message("Batch inserting tweets in collection '", coll, "' of database '", 
            db, "' in MongoDB\n")
  }

  if (length(t.list) < 1) {
    return
  }

  pb <- txtProgressBar(min = 1, max = length(t.list), 
                     style = 3)

  for (i in 1:length(t.list)) {
    val = mongo.insert.batch(mongo = mongo, ns = ns, t.list[[i]])
    if (!val) {
#       print(mongo.get.server.err.string(mongo))
#       print(mongo.get.err(mongo))
#       print(mongo.get.last.err(mongo, db = db))
      
#       stop("Error in connection to MongoDB")
    }
    setTxtProgressBar(pb, i)
  }
}

format.twitter.date <- function(datestring, format="datetime"){
  if (format=="datetime"){
    date <- as.POSIXct(datestring, format="%a %b %d %H:%M:%S %z %Y")
  }
  if (format=="date"){
    date <- as.Date(datestring, format="%a %b %d %H:%M:%S %z %Y")
  }   
  return(date)
}
