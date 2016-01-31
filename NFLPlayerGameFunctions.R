######## Code Below is Associated with  Player Game Dataframe Creation #########

# Loading Libraries

library(RJSONIO)
library(RCurl)
library(stringr)
library(lubridate)

source("ReadingNFLJSONData.R")


PlayerGame.Function <- function(URLString) {
  # This function outputs a single dataframe containing all rushing, passing,
  # and receiving statistics for each player in a single  game.  Each player is 
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
  
  dfpass <- rbind(data.frame(Team = nfl.json[[1]][[1]]$abbr,
                             t(sapply(nfl.json[[1]][[1]]$stats$passing, c))),
                  data.frame(Team = nfl.json[[1]][[2]]$abbr,
                             t(sapply(nfl.json[[1]][[2]]$stats$passing, c))))
  dfrush <- rbind(data.frame(Team = nfl.json[[1]][[1]]$abbr,
                             t(sapply(nfl.json[[1]][[1]]$stats$rushing, c))),
                  data.frame(Team = nfl.json[[1]][[2]]$abbr,
                             t(sapply(nfl.json[[1]][[2]]$stats$rushing, c))))
  dfrec <- rbind(data.frame(Team = nfl.json[[1]][[1]]$abbr,
                            t(sapply(nfl.json[[1]][[1]]$stats$receiving, c))),
                 data.frame(Team = nfl.json[[1]][[2]]$abbr,
                            t(sapply(nfl.json[[1]][[2]]$stats$receiving, c))))
  

  # Case when both teams have at least one fumble
  if (length(nfl.json[[1]][[1]]$stats$fumbles) > 0 & 
        length(nfl.json[[1]][[2]]$stats$fumbles) > 0 ) {
    
        dffumb <- rbind(data.frame(Team = nfl.json[[1]][[1]]$abbr,
                             t(sapply(nfl.json[[1]][[1]]$stats$fumbles, c))),
                  data.frame(Team = nfl.json[[1]][[2]]$abbr,
                             t(sapply(nfl.json[[1]][[2]]$stats$fumbles, c))))
  }
  
  # Case when there are no fumbles for either team
  else if (length(nfl.json[[1]][[1]]$stats$fumbles) == 0 & 
             length(nfl.json[[1]][[2]]$stats$fumbles) == 0 ) {
    dffumb <- NULL
  }
  
  # Case when team 2 fumbles but team 1 does not 
  else if (length(nfl.json[[1]][[1]]$stats$fumbles) == 0 & 
             length(nfl.json[[1]][[2]]$stats$fumbles) > 0) {
    dffumb <- data.frame(Team = nfl.json[[1]][[2]]$abbr,
                               t(sapply(nfl.json[[1]][[2]]$stats$fumbles, c)))
  }
  
  # Case when team 1 fumbles but team 2 does not 
  else {
    dffumb <- data.frame(Team = nfl.json[[1]][[1]]$abbr,
                         t(sapply(nfl.json[[1]][[1]]$stats$fumbles, c)))
  }
  
  if (is.null(dffumb)) {
    
    # Initialize a new variable with the player IDs
    dfpass$playerID <- rownames(dfpass)
    dfrush$playerID <- rownames(dfrush)
    dfrec$playerID <- rownames(dfrec)
    
    # This stage is where we merge all the dataframes together so each player 
    # has one line
    final.df <- Reduce(function(x, y) 
    {merge(x, y, by = c("Team", "playerID", "name"),all=TRUE)},
    list(dfpass, dfrush, dfrec))
    
    # Adding Fumble columns with 0's due to no occurance of fumbles in game
    final.df$totalfumbs <- 0
    final.df$recfumbs <- 0
    final.df$totalrecfumbs <- 0
    final.df$fumbyds <- 0
    final.df$fumbslost <- 0
  }
  
  else {
  colnames(dffumb) <- c("Team", "name", "totalfumbs", "recfumbs", 
                        "totalrecfumbs",
                       "fumbyds", "fumbslost")
  # Initialize a new variable with the player IDs
  dfpass$playerID <- rownames(dfpass)
  dfrush$playerID <- rownames(dfrush)
  dfrec$playerID <- rownames(dfrec)
  dffumb$playerID <- rownames(dffumb)

  # This stage is where we merge all the dataframes together so each player 
  # has one line
  final.df <- Reduce(function(x, y) 
  {merge(x, y, by = c("Team","playerID", "name"),all=TRUE)},
  list(dfpass, dfrush, dfrec, dffumb))
  }
  
  final.df <- data.frame(Team = final.df[,1],
                    sapply(final.df[,-1], 
                           function(x) ifelse((x == "NULL" | is.na(x)), 0, x)))
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
  final.df2 <- data.frame(gameID, date, final.df)
  
  
  # Unlist the listed variables in order to return the output dataframe in a 
  # friendlier format
  
  final.df2$playerID <- unlist(final.df2$playerID)
  final.df2$name <- unlist(final.df2$name)
  final.df2$Team.x <- unlist(final.df2$Team.x)
  final.df2$att.x <- unlist(final.df2$att.x)
  final.df2$cmp <- unlist(final.df2$cmp)
  final.df2$yds.x <- unlist(final.df2$yds.x)
  final.df2$tds.x <- unlist(final.df2$tds.x)
  final.df2$ints <- unlist(final.df2$ints)
  final.df2$twopta.x <- unlist(final.df2$twopta.x)
  final.df2$twoptm.x <- unlist(final.df2$twoptm.x)
  final.df2$att.y <- unlist(final.df2$att.y)
  final.df2$yds.y <- unlist(final.df2$yds.y)
  final.df2$tds.y <- unlist(final.df2$tds.y)
  final.df2$lng.x <- unlist(final.df2$lng.x)
  final.df2$lngtd.x <- unlist(final.df2$lngtd.x)
  final.df2$twopta.y <- unlist(final.df2$twopta.y)
  final.df2$twoptm.y <- unlist(final.df2$twoptm.y)
  final.df2$rec <- unlist(final.df2$rec)
  final.df2$yds <- unlist(final.df2$yds)
  final.df2$tds <- unlist(final.df2$tds)
  final.df2$lng.y <- unlist(final.df2$lng.y)
  final.df2$lngtd.y <- unlist(final.df2$lngtd.y)
  final.df2$twopta <- unlist(final.df2$twopta)
  final.df2$twoptm <- unlist(final.df2$twoptm)
  final.df2$totalfumbs <- unlist(final.df2$totalfumbs)
  final.df2$recfumbs <- unlist(final.df2$recfumbs)
  final.df2$totalrecfumbs <- unlist(final.df2$totalrecfumbs)
  final.df2$fumbyds <- unlist(final.df2$fumbyds)
  final.df2$fumbslost <- unlist(final.df2$fumbslost)
  
  final.df2
}

nfl.data.urltest <- 
      "http://www.nfl.com/liveupdate/game-center/2013090800/2013090800_gtd.json"

PlayerGameTest <- PlayerGame.Function(gameURLs[17])


SeasonPlayerGame <- function(Year) {
  # This function outputs a single dataframe containing all rushing, passing,
  # receiving, and fumble statistics for each player in each game.  Each player 
  # is assigned one line associated wih their statisitcs per game
  # Args:
  #      Year - a numeric 4 digit number specifying a year 
  # Returns: 
  #      A dataframe containing the rushing, receiving, passing, and fumble 
  #      statistics for each player that recorded such a statistic in everygame 
  #      in a season
  #
  gameIDS <- Extracting_NFL_GameIDs(Year)
  gameURLs <- sapply(gameIDS, Proper.PBP.URL.Formatting)
  
  print("CHECK")
  playergameseason.unformatted <- lapply(gameURLs, FUN = PlayerGame.Function)
  
  print("CHECK")
  # Rowbinding all the games from the specified season
  
  playergameseason <- do.call(rbind, playergameseason.unformatted)
  
  # Final output dataframe
  playergameseason
}


# Here we collect the playergame data for all games from 2010-2014

PlayerGame2010 <- SeasonPlayerGame(2010)
PlayerGame2011 <- SeasonPlayerGame(2011)
PlayerGame2012 <- SeasonPlayerGame(2012)
PlayerGame2013 <- SeasonPlayerGame(2013)
PlayerGame2014 <- SeasonPlayerGame(2014)
PlayerGame2015 <- SeasonPlayerGame(2015)

### Player Season Stats Function ###

PlayerSeasonStats.Function <- function(Year) {
  
  PlayerData.Year <- SeasonPlayerGame(Year)
  
  TestSeasonAgg <- ddply(PlayerData.Year[,-c(1,2)], 
                         .(Team, playerID, name), 
                         numcolwise(sum))
}





