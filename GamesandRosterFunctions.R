######## Code Below is Associated Game and Roster Functions #########

SeasonAllGames <- function(Season) {
  
  gameIDs <- Extracting_NFL_GameIDs(Season)
  gameURLs <- sapply(gameIDS, Proper.PBP.URL.Formatting)
  
  # Game Dates
  year <- substr(gameIDs, start = 1, stop = 4)
  month <- substr(gameIDs, start = 5, stop = 6)
  day <- substr(gameIDs, start = 7, stop = 8)
  date <- as.Date(paste(month, day, year, sep = "/"), format = "%m/%d/%Y")
  
  # Home and Away Teams
  
  teams.unformatted <- lapply(gameURLs, 
                                         FUN = function(x) {
                                          cbind(t(sapply(fromJSON(getURL(x))[[1]]$home[2]$abbr, c)),
                                               t(sapply(fromJSON(getURL(x))[[1]]$away[2]$abbr, c)))
                                  
                                           
                                         })
  
  teams <- do.call(rbind, teams.unformatted)
  
  # Output Dataframe

  data.frame(date = date, home = teams[,1], away = teams[,2])
}

system.time(allgames2012 <- SeasonAllGames(2012))


