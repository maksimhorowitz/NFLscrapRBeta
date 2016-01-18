######## Code Below is Associated with  Player Game Dataframe Creation #########

# Loading Libraries

library(RJSONIO)
library(RCurl)
library(stringr)
library(lubridate)


PlayerGame.Function <- function(URLString) {
  # This function outputs a single dataframe containing all rushing, passing,
  # and receiving statistics for each player in each game.  Each player is 
  # assigned one line associated wih their statisitcs
  # Args:
  #      URLString - a string containing the URL of the relevant JSON data
  # Returns: 
  #      A dataframe containing the rushing, receiving, and passing statistics
  #      for each player that recorded such a statistic in a single game
  #
  
  # Converting URL into readable JSON format
  nfl.json <- fromJSON(getURL(URLString))
  
  # Here we build the dataframes for the rushing, passing, receving, and 
  # fumbling stats
  
  dfpass <- rbind(data.frame(t(sapply(nfl.json[[1]][[1]]$stats$passing, c))),
                  data.frame(t(sapply(nfl.json[[1]][[2]]$stats$passing, c))))
  dfrush <- rbind(data.frame(t(sapply(nfl.json[[1]][[1]]$stats$rushing, c))),
                  data.frame(t(sapply(nfl.json[[1]][[2]]$stats$rushing, c))))
  dfrec <- rbind(data.frame(t(sapply(nfl.json[[1]][[1]]$stats$receiving, c))),
                 data.frame(t(sapply(nfl.json[[1]][[2]]$stats$receiving, c))))
  #dffumb <- rbind(data.frame(t(sapply(nfl.json[[1]][[1]]$stats$fumbles, c))),
  #              data.frame(t(sapply(nfl.json[[1]][[2]]$stats$fumbles, c))))
  #colnames(dffumb) <- c("name", "totalfumbs", "recfumbs", "totalrecfumbs",
  #                     "fumbyds", "fumbslost")
  # Initialize a new variable with the player IDs
  dfpass$playerID <- rownames(dfpass)
  dfrush$playerID <- rownames(dfrush)
  dfrec$playerID <- rownames(dfrec)
  #dffumb$playerID <- rownames(dffumb)
  
  # This stage is where we merge all the dataframes together so each player 
  # has one line
  final.df <- Reduce(function(x, y) 
  {merge(x, y, by = c("playerID", "name"),all=TRUE)},
  list(dfpass, dfrush, dfrec))
  
  #if (!is.null(dffumb)) {
  # final.df <- merge(final.df, dffumb, by = c("playerID", "name"),
  #                sort = FALSE, all = TRUE)
  #}
  
  final.df <- sapply(final.df, function(x) ifelse(x == "NULL", NA, x))
  rownames(final.df) <- NULL
  
  #   GameID
  gameID <- str_extract(URLString, pattern = "[0-9]{10}")
  
  # Date of Game   
  datestep1 <- str_extract(URLString, pattern = "/[0-9]{10}/")
  datestep2 <- str_extract(datestep1, pattern = "[0-9]{8}")
  year <- substr(datestep2, start = 1, stop = 4)
  month <- substr(datestep2, start = 5, stop = 6)
  day <- substr(datestep2, start = nchar(datestep2)-1, stop = nchar(datestep2))
  date <- as.Date(paste(month, day, year, sep = "/"), format = "%m/%d/%Y")
  
  # Output dataframe which has the gameID, date of game, and the player info
  # and statistics 
  data.frame(gameID, date, final.df)
}

PlayerGame.Function(nfl.data.url2)

SeasonPlayerGame <- function(Year) {
  
  gameIDS <- Extracting_NFL_GameIDs(Year)
  gameURLs <- sapply(gameIDS, Proper.PBP.URL.Formatting)
  playergameseason.unformatted <- lapply(gameURLs, FUN = PlayerGame.Function)
  
  playergameseason <- do.call(rbind, playergameseason.unformatted)
  
  playergameseason$playerID <- unlist(playergameseason$playerID)
  playergameseason$name <- unlist(playergameseason$name)
  playergameseason$att.x <- unlist(playergameseason$att.x)
  playergameseason$cmp <- unlist(playergameseason$cmp)
  playergameseason$yds.x <- unlist(playergameseason$yds.x)
  playergameseason$tds.x <- unlist(playergameseason$tds.x)
  playergameseason$ints <- unlist(playergameseason$ints)
  playergameseason$twopta.x <- unlist(playergameseason$twopta.x)
  playergameseason$twoptm.x <- unlist(playergameseason$twoptm.x)
  playergameseason$att.y <- unlist(playergameseason$att.y)
  playergameseason$yds.y <- unlist(playergameseason$yds.y)
  playergameseason$tds.y <- unlist(playergameseason$tds.y)
  playergameseason$lng.x <- unlist(playergameseason$lng.x)
  playergameseason$lngtd.x <- unlist(playergameseason$lngtd.x)
  playergameseason$twopta.y <- unlist(playergameseason$twopta.y)
  playergameseason$twoptm.y <- unlist(playergameseason$twoptm.y)
  playergameseason$rec <- unlist(playergameseason$rec)
  playergameseason$yds <- unlist(playergameseason$yds)
  playergameseason$tds <- unlist(playergameseason$tds)
  playergameseason$lng.y <- unlist(playergameseason$lng.y)
  playergameseason$lngtd.y <- unlist(playergameseason$lngtd.y)
  playergameseason$twopta <- unlist(playergameseason$twopta)
  playergameseason$twoptm <- unlist(playergameseason$twoptm)
  playergameseason$totalfumbs <- unlist(playergameseason$totalfumbs)
  playergameseason$recfumbs <- unlist(playergameseason$recfumbs)
  playergameseason$totalrecfumbs <- unlist(playergameseason$totalrecfumbs)
  playergameseason$fumbyds <- unlist(playergameseason$fumbyds)
  playergameseason$fumbslost <- unlist(playergameseason$fumbslost)
  
  playergameseason
}


# Here we collect the playergame data for all games from 2010-2014

PlayerGame2010 <- SeasonPlayerGame(2010)
PlayerGame2011 <- SeasonPlayerGame(2011)
PlayerGame2012 <- SeasonPlayerGame(2012)
PlayerGame2013 <- SeasonPlayerGame(2013)
PlayerGame2014 <- SeasonPlayerGame(2014)

# Combining them into one dataframe

PlayerGame10to14 <- rbind(PlayerGame2010, PlayerGame2011, PlayerGame2012,
                          PlayerGame2013, PlayerGame2014)
PlayerGame10to14 <- data.frame(PlayerGame10to14)

# Writing the CSV

write.csv(PlayerGame10to14, file = "PlayerGame10to14.csv")

