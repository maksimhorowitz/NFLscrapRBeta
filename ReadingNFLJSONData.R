################################################################################
################### Daily Fantasy Sports: Reading in JSON Data ################# 
################################################################################

### Maksim Horowitz ###
### Started on 08/11/2015 ###
### Reading in test NFL Data ###

#### Code Start ###

## Setting the Working Directory ##

setwd("~/Documents/Personal/DFS Project")

## Loading in Initial JSON Packages ##

#install.packages("RJSONIO")
#install.packages("RCurl")
library(RJSONIO)
library(RCurl)
library(stringr)
library(lubridate)
##############################
##############################

# Initializing URL with JSON Data 
nfl.data.url <- "http://www.nfl.com/liveupdate/game-center/2015080951/2015080951_gtd.json"

# Inputting it into a JSON list Object 
nfl.json.data <- fromJSON(getURL(nfl.data.url))

# Some Data Exploration for Max 
#class(nfl.json.data )
#length(nfl.json.data)
#names(nfl.json.data )
#names(nfl.json.data [[1]])

##############################
##############################
##############################
##############################

#### Aggregating Function to Pull the Above Stats in a list form ####

NFL.JSON.BoxScore.Pull <- function(URLString, 
                                   home = TRUE) {
  # This function pulls data from an NFL url and contructs it into a formatted 
  #boxscore.
  # Args: 
  #  URLString (character) is a character string of the URL of where to 
  #                         pull the data from
  #  home (boolean): home = TRUE will pull home stats, 
  #                  home = FALSE pulls away stats
  # Returns: 
  #         Properly structured list of Boxscore type statistics for the game
  
  ##################
  ##################
  
  #Loading Libraries
  library(RJSONIO)
  library(RCurl)
  library(stringr)
  
  #   Start of Function
  
  nfl.json.data <- fromJSON(getURL(URLString))
  
  #   GameID
  gameID <- str_extract(URLString, pattern = "[0-9]{10}")
  
  # Date of Game   
  datestep1 <- str_extract(URLString, pattern = "/[0-9]{10}/")
  datestep2 <- str_extract(datestep1, pattern = "[0-9]{8}")
  year <- substr(datestep2, start = 1, stop = 4)
  month <- substr(datestep2, start = 5, stop = 6)
  day <- substr(datestep2, start = nchar(datestep2)-1, stop = nchar(datestep2))
  date <- as.Date(paste(month, day, year, sep = "/"), format = "%m/%d/%Y")
  
  #   Parsing Data
  if (home == TRUE) {
    homeTeamName <- nfl.json.data[[1]]$home$abbr
    # Passing Stats
    qbStats <- data.frame(date, gameID, homeTeamName, 
                          t(sapply(nfl.json.data[[1]]$home$stats$passing, c)))
    qbStats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$home$stats$passing, 
                                          c)))
    # Running Stats
    rbStats <- data.frame(date, gameID, homeTeamName, 
                          t(sapply(nfl.json.data[[1]]$home$stats$rushing, c)))
    rbStats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$home$stats$rushing, 
                                          c)))
    # Receiving Stats
    wrStats <- data.frame(date, gameID, homeTeamName, 
                          t(sapply(nfl.json.data[[1]]$home$stats$receiving, c)))
    wrStats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$home$stats$receiving, 
                                          c)))
    # Defensive Stats
    defStats <- data.frame(date, gameID, homeTeamName, 
                           t(sapply(nfl.json.data[[1]]$home$stats$defense, c)))
    defStats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$home$stats$defense, 
                                           c)))
    # Kicking Stats
    kickerStats <- data.frame(date, gameID, homeTeamName, 
                              t(sapply(nfl.json.data[[1]]$home$stats$kicking, 
                                       c)))
    kickerStats$playerID <- rownames(t(
      sapply(nfl.json.data[[1]]$home$stats$kicking, 
             c)))
    # Fumble Stats
    fumbStats <- data.frame(date, gameID, homeTeamName, 
                            t(sapply(nfl.json.data[[1]]$home$stats$fumbles, c)))
    fumbStats$playerID <- rownames(t(
      sapply(nfl.json.data[[1]]$home$stats$fumbles, 
             c)))
    # Kick Return Stats
    krStats <- data.frame(date, gameID, homeTeamName, 
                          t(sapply(nfl.json.data[[1]]$home$stats$kickret, c)))
    krStats$playerID <- rownames(t(
      sapply(nfl.json.data[[1]]$home$stats$kickret, 
             c)))
    # Punt Return Stats
    prStats <- data.frame(date, gameID, homeTeamName, 
                          t(sapply(nfl.json.data[[1]]$home$stats$puntret, c)))
    prStats$playerID <- rownames(t(
      sapply(nfl.json.data[[1]]$home$stats$puntret, 
             c)))
    # List of Stats
    homeTeam.Stats <- list(qbStats, rbStats, wrStats, defStats, kickerStats, 
                           fumbStats, krStats, prStats)
    names(homeTeam.Stats) <- c("Passing", "Rushing", "Receiving", "Defense", 
                               "Kicking","Fumbles", "Kick Return", "Punt Return"
    )
    homeTeam.Stats
  } else {
    awayTeamName <- nfl.json.data[[1]]$away$abbr
    qbAwayStats <- data.frame(gameID, awayTeamName, 
                              t(sapply(nfl.json.data[[1]]$away$stats$passing, 
                                       c)))
    qbAwayStats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$passing, 
                                              c)))
    # Running AwayStats
    rbAwayStats <- data.frame(date, gameID, awayTeamName, 
                              t(sapply(nfl.json.data[[1]]$away$stats$rushing, 
                                       c)))
    rbAwayStats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$rushing, 
                                              c)))
    # Receiving AwayStats
    wrAwayStats <- data.frame(date, gameID, awayTeamName, 
                              t(sapply(nfl.json.data[[1]]$away$stats$receiving, 
                                       c)))
    wrAwayStats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$receiving, 
                                              c)))
    # Defensive AwayStats
    defAwayStats <- data.frame(date, gameID, awayTeamName, 
                               t(sapply(nfl.json.data[[1]]$away$stats$defense, 
                                        c)))
    defAwayStats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$defense, 
                                               c)))
    # Kicking AwayStats
    kickerAwayStats <- data.frame(date, gameID, awayTeamName, 
                                  t(sapply(nfl.json.data[[1]]$away$stats$kicking
                                           , c)))
    kickerAwayStats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$kicking, 
                                                  c)))
    # Fumble AwayStats
    fumbAwayStats <- data.frame(date, gameID, awayTeamName, 
                                t(sapply(nfl.json.data[[1]]$away$stats$fumbles, 
                                         c)))
    fumbAwayStats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$fumbles, 
                                                c)))
    # Kick Return AwayStats
    krAwayStats <- data.frame(date, gameID, awayTeamName,
                              t(sapply(nfl.json.data[[1]]$away$stats$kickret, 
                                       c)))
    krAwayStats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$kickret, 
                                              c)))
    # Punt Return AwayStats
    prAwayStats <- data.frame(date, gameID, awayTeamName, 
                              t(sapply(nfl.json.data[[1]]$away$stats$puntret, 
                                       c)))
    prAwayStats$playerID <- rownames(t(sapply(nfl.json.data[[1]]$away$stats$puntret, 
                                              c)))
    # List of AwayStats
    awayTeamStats <- list(qbAwayStats, rbAwayStats, wrAwayStats, defAwayStats, 
                          kickerAwayStats, fumbAwayStats, krAwayStats, 
                          prAwayStats)
    names(awayTeamStats) <- c("Passing", "Rushing", "Receiving", "Defense", 
                              "Kicking", "Fumbles", "Kick Return", 
                              "Punt Return")
    awayTeamStats
  }
}

NFL.JSON.BoxScore.Pull(nfl.data.url, home = T) # Example 


##############################
##############################
##############################
##############################

### Drive Summary Function ###

#Input a URL with NFL JSON data
#Outputs a data.frame of the drive summary data
Drive.Summary.Function <- function(URLString) {
  
  # This function outputs the end result of each drive of the game
  # Args:
  #      URLSTRING: String input of the URL to the JSON data for any given
  #                 nfl game
  # Returns: 
  #      A dataframe that has the summary statistics for each drive
  #      Final output includes first downs, drive result, penalty yards, 
  #      of plays, 
  #      time of possession, Quarter at the Start of the Drive, 
  #      Time at Start of Drive
  #      Yardline at start of drive, team with possession at start,
  #      End of Drive Quarter, End of Drive Time, End of drive Yard line, 
  #      End of drive team with possession
  
  ######################
  ######################
  
  # libraries
  library(RJSONIO)
  library(RCurl)
  
  # Converting JSON data
  nfl.json.data <- fromJSON(getURL(URLString))
  
  # Starting the splicing
  drive.Data <- data.frame(do.call(rbind, (nfl.json[[1]]$drives)))
  start.Data <- data.frame(do.call(rbind, (Drive.Data$start))) 
  colnames(Start.Data) <- c("StartQrt", "StartTime", "StartYardln", "StartTeam")
  end.Data <- data.frame(do.call(rbind, (Drive.Data$end)))
  colnames(End.Data) <- c("EndQrt", "EndTime", "EndYardln", "EndTeam")
  start.index <- which(colnames(Drive.Data) == "start")
  end.index <- which(colnames(Drive.Data) == "end")
  drive.Data.Final <- cbind(Drive.Data[, -c(start.index,end.index)], 
                            Start.Data, End.Data)
  drive.Data.Final[-nrow(Drive.Data), ]
}

# Example
Drive.Summary.Function(nfl.data.url)


###################################
###################################
# Function for Play-by-Play Summary 


##### Below is the formatted play-by-play data from an inputted JSON URL ####

Viewable.PBP.Function <- function(URLString) {
  
  # This function intakes the JSON play-by-play data of every game and parses it 
  # into a readable dataframe
  # Args:
  #       URLString (string):  Input URL string of JSON nfl data 
  # Returns:
  #       A dataframe of various statistics and outcomes of the plays
  #       of each NFL game.
  
  #########################
  #########################
  
  # libraries
  library(RJSONIO)
  library(RCurl)
  library(stringr)
  library(lubridate)
  # Converting JSON data
  nfl.json <- fromJSON(getURL(URLString))
  Number.drives <- length(nfl.json[[1]]$drives)-1
  PBP <- NULL
  for (ii in 1:Number.drives) {
    PBP <- rbind(PBP, cbind("Drive" = ii,data.frame(do.call(
      rbind, 
      (nfl.json[[1]]$drives[[ii]]$plays)))[,c(1:9)]))
  }
  
  # Fixing Possession team for Kick-Offs
  
  kickoff.Index <- which(sapply(PBP$desc, regexpr, 
                                pattern = 
                                  "kicks") != -1)
  posTeams <- unlist(unique(PBP$posteam))[1:2]
  correctKickoffPos <- ifelse(PBP$posteam[kickoff.Index] == posTeams[1], 
                              posTeams[2], posTeams[1])
  PBP[kickoff.Index, "posteam"] <- correctKickoffPos
  
  # Yard Line Information
  
  ylineInfo <- sapply(PBP$yrdln, strsplit, split = " ")
  
  PBP$SideofField <- sapply(ylineInfo, FUN = function(x) x[1])
  PBP$yrdln <- sapply(ylineInfo, FUN = function(x) x[2])
  
  # Game Date  
  datestep1 <- str_extract(URLString, pattern = "/[0-9]{10}/")
  datestep2 <- str_extract(datestep1, pattern = "[0-9]{8}")
  year <- substr(datestep2, start = 1, stop = 4)
  month <- substr(datestep2, start = 5, stop = 6)
  day <- substr(datestep2, start = nchar(datestep2)-1, stop = nchar(datestep2))
  date <- as.Date(paste(month, day, year, sep = "/"), format = "%m/%d/%Y")
  
  PBP$Date <- date
  PBP$GameCode <- str_extract(datestep1, pattern = "[0-9]{10}")
  # Picking Apart the Description Column
  
  #   Yards Gained
  Yards.Step1 <- sapply(PBP$desc, str_extract, 
                        pattern = "for (-)?([0-9]{1,2})?")
  
  PBP$Yards.Gained <- as.numeric(ifelse(grepl(x = Yards.Step1, 
                                              pattern = "(-)?([0-9]{1,2})"), 
                                        str_extract(Yards.Step1, "(-)?([0-9]{1,2})") 
                                        , "0"))
  
  #   Touchdown Play 
  
  TouchDown.Step1 <- 
    sapply(PBP$desc, str_extract, pattern = "TOUCHDOWN")
  nullified <- grep(PBP$desc, pattern = "TOUCHDOWN NULLIFIED")
  TouchDown.Step1[nullified] <- NA
  PBP$Touchdown <- ifelse(!is.na(TouchDown.Step1), 1, 0)
  
  #   Two Point Conversion 
  PBP$TwoPointConv <- NA
  
  Two.Point.Success <- which(sapply(PBP$desc, regexpr, 
                                    pattern = 
                                      "TWO-POINT CONVERSION ATTEMPT\\. (.){1,70}\\. ATTEMPT SUCCEEDS") != -1)
  Two.Point.Failure <- which(sapply(PBP$desc, regexpr, 
                                    pattern = 
                                      "TWO-POINT CONVERSION ATTEMPT\\. (.){1,70}\\. ATTEMPT FAILS") != -1)
  
  PBP$TwoPointConv[Two.Point.Success] <- "Success"
  PBP$TwoPointConv[Two.Point.Failure] <- "Failure"
  
  #   Penalty - Binary Column 
  
  PBP$Accepted.Penalty <- NA
  Penalty.Play <- sapply(PBP$desc, str_extract, pattern = "PENALTY")
  PBP$Accepted.Penalty <- ifelse(!is.na(Penalty.Play), 1, 0)
  
  #   Penalty Yards 
  
  PBP$Penalty.Yards <- NA
  
  Penalty.Yards.Step1 <- sapply(PBP$desc, str_extract, 
                                pattern = ", [0-9]{1,2} yard(s?), enforced")
  PBP$Penalty.Yards <- ifelse(!is.na(Penalty.Yards.Step1), 
                              as.numeric(str_extract(Penalty.Yards.Step1, 
                                                     "[0-9]{1,2}")), 0)
  
  #   Defenseive Team Column
  
  PBP$DefensiveTeam <- NA
  Teams.step1 <- str_extract(unlist(unique(PBP$posteam)), "[A-Z]{2,3}")
  
  Teams <- Teams.step1[which(!is.na(Teams.step1))]
  T1 <- Teams[1]
  T2 <- Teams[2]
  
  PBP$DefensiveTeam[which(PBP$posteam == T1)] <- T2
  PBP$DefensiveTeam[which(PBP$posteam == T2)] <- T1
  
  #   Players Involved 
  
  #     Passer
  Passer.Step1 <- sapply(PBP$desc, str_extract, 
                         pattern = "[A-Z]\\.[A-Z][A-z]{1,20} pass")
  PBP$Passer <- str_extract(Passer.Step1, pattern = "[A-Z]\\.[A-Z][A-z]{1,20}")
  
  #     Receiver
  
  Receiver.Step1 <- sapply(PBP$desc, str_extract, 
                           pattern = 
                             "pass (incomplete)?( )?[a-z]{4,5} [a-z]{4,6} to [A-Z]\\.[A-Z][A-z]{1,20}")
  
  PBP$Receiver <- str_extract(Receiver.Step1, 
                              pattern = "[A-Z]\\.[A-Z][A-z]{1,20}")
  
  #     Tacklers
  
  Tacklers.Step1 <- sapply(PBP$desc, str_extract, 
                           pattern = 
                             "(yard(s?)|no gain) \\([A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?(;)?( )?([A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?)?\\)\\.")
  
  #     Identifying the tacklers on the play (either one or two)
  Tacklers1 <- str_extract(Tacklers.Step1,
                           pattern = 
                             "\\([A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")
  Tacklers1 <- str_extract(Tacklers1, 
                           pattern = 
                             "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")
  
  #     Pulling out tacklers names
  Tacklers2 <- str_extract(Tacklers.Step1, 
                           pattern = 
                             ";( )[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")
  Tacklers2 <- str_extract(Tacklers2,
                           pattern = 
                             "[A-z]{1,3}\\.( )?[A-Z][A-z]{1,20}(('|-)?[A-z]{1,15})?")
  
  PBP$Tackler1 <- Tacklers1
  PBP$Tackler2 <- Tacklers2
  
  
  ## Type of Play ##
  
  PBP$PlayType <- NA
  
  #     Pass Plays
  
  PBP$PassOutcome <- NA
  pass.Play <- which(sapply(PBP$desc, regexpr, pattern = "pass") != -1)
  incomplete.Pass.Play <- which(sapply(PBP$desc, regexpr, 
                                       pattern = "(pass incomplete)|INTERCEPTED") != -1)
  
  PBP$PlayType[pass.Play] <- "Pass"
  
  ##   Pass Outcome
  PBP$PassOutcome[incomplete.Pass.Play] <- "Incomplete Pass"
  PBP$PassOutcome[setdiff(pass.Play,incomplete.Pass.Play)] <- "Complete"
  
  ## Pass Length
  PBP$PassLength <- NA
  short.Pass <- which(sapply(PBP$desc, regexpr, 
                             pattern = "pass (incomplete )?short") != -1)
  deep.Pass <- which(sapply(PBP$desc, regexpr, 
                            pattern = "pass (incomplete )?deep") != -1)
  PBP$PassLength[short.Pass] <- "Short"
  PBP$PassLength[deep.Pass] <- "Deep"
  
  ## Pass Location
  PBP$PassLocation <- NA
  pass.Left <- which(sapply(PBP$desc, regexpr, 
                            pattern = "(short|deep) left") != -1)
  pass.Right <- which(sapply(PBP$desc, regexpr, 
                             pattern = "(short|deep) right") != -1)   
  pass.Middle <- which(sapply(PBP$desc, regexpr, 
                              pattern = "(short|deep) middle") != -1) 
  PBP$PassLocation[pass.Left] <- "left"
  PBP$PassLocation[pass.Right] <- "right"
  PBP$PassLocation[pass.Middle] <- "middle"
  
  ## Pass Attempt
  # Can move this lower in the code to not include passes called back by penalties
  PBP$PassAttempt <- NA
  PBP$PassAttempt <- ifelse(sapply(PBP$desc, grepl, 
                                   pattern = "pass"),1,0)
  
  ## Reception Made
  PBP$Reception <- 0
  PBP$Reception[setdiff(pass.Play,incomplete.Pass.Play)] <- 1
  
  #     Interception Thrown
  PBP$InterceptionThrown <- ifelse(
    sapply(PBP$desc, grepl, 
           pattern = "INTERCEPTED"),1,0)
  
  #     Kick Off
  Kick.Off.Play <- which(sapply(PBP$desc, regexpr, pattern = "kick") != -1)
  
  PBP$PlayType[Kick.Off.Play] <- "Kick-Off"
  
  #     Punt
  Punt.Play <- which(sapply(PBP$desc, regexpr, pattern = "punts") != -1)
  
  PBP$PlayType[Punt.Play] <- "Punt"
  
  #     Field Goal
  fieldgoal <- which(sapply(PBP$desc, regexpr, 
                            pattern = "field goal") != -1)
  missed.FG <- which(sapply(PBP$desc, regexpr, 
                            pattern = "field goal is No Good") != -1)
  
  PBP$PlayType[fieldgoal] <- "Field Goal"
  
  ## Field Goal Result
  PBP$FieldGoalResult <- NA
  PBP$FieldGoalResult[missed.FG] <- "No Good"
  PBP$FieldGoalResult[setdiff(fieldgoal,missed.FG)] <- "Good"
  
  #     Extra Point
  Made.Extra.Point.Plays <- which(sapply(PBP$desc, regexpr,
                                         pattern = "extra point is GOOD") != -1)
  Missed.Extra.Point.Plays <- which(sapply(PBP$desc, regexpr, 
                                           pattern = "extra point is No Good") != -1)
  
  PBP$PlayType[Made.Extra.Point.Plays] <- "Extra-Point"
  
  ## Extra Point Result
  PBP$ExPointResult <- NA
  PBP$ExPointResult[Made.Extra.Point.Plays] <- "Good"
  PBP$ExPointResult[Missed.Extra.Point.Plays] <- "No Good"
  
  #     Fumbles
  
  PBP$Fumble <- NA
  fumble.index <- which(sapply(PBP$desc, regexpr, pattern = "FUMBLE") != -1) 
  PBP$Fumble[fumble.index] <- "Fumble"
  
  #     Time Outs
  TimeOuts <- which(sapply(PBP$desc, regexpr, 
                           pattern = "[A-z]imeout #[1-5] by") != -1)
  
  PBP$PlayType[TimeOuts] <- "Timeout"
  
  #     Quarter End
  End.Quarter <- which(sapply(PBP$desc, regexpr, 
                              pattern = "END QUARTER") != -1)
  
  PBP$PlayType[End.Quarter] <- "QuarterEnd"
  
  #     2 Minute Warning
  Two.Minute.Warning <- which(sapply(PBP$desc, regexpr, 
                                     pattern = "Two-Minute Warning") != -1)
  
  PBP$PlayType[Two.Minute.Warning] <- "Two Minute Warning"
  
  #     Sack 
  Sack.Plays <- which(sapply(PBP$desc, regexpr, pattern = "sacked") != -1)
  
  PBP$PlayType[Sack.Plays] <- "Sack"
  
  ## Sacked?
  PBP$Sack <- 0
  PBP$Sack[Sack.Plays] <- 1
  
  #     QB Kneel
  Kneel.Play <- which(sapply(PBP$desc, regexpr, pattern = "kneels") != -1)
  
  PBP$PlayType[Kneel.Play] <- "QB Kneel"
  
  #     Spike
  Spike.Play <- which(sapply(PBP$desc, regexpr, pattern = "spiked") != -1)
  
  PBP$PlayType[Spike.Play] <- "Spike"
  
  #     No Play
  No.Play.Plays <- which(sapply(PBP$desc, regexpr, 
                                pattern = "No Play") != -1)
  
  PBP$PlayType[No.Play.Plays] <- "No Play"
  
  #     End of Game 
  End.Game <- which(sapply(PBP$desc, regexpr, pattern = "END GAME") != -1)
  
  PBP$PlayType[End.Game] <- "End of Game"
  
  #     First Down 
  PBP$FirstDown <- NA
  first.downplays <- which(PBP$down == 1)
  first.downs <- first.downplays-1
  PBP$FirstDown[first.downs] <- ifelse(PBP$down[first.downs] ==0, NA, 1)
  
  #   Running Play
  
  Running.Play <- which(is.na(PBP$PlayType))
  PBP$PlayType[Running.Play] <- "Run"
  PBP$RushAttempt <- ifelse(PBP$PlayType == "Run", 1,0)
  
  ## Run Direction and Gap
  PBP$RunLocation <- NA
  PBP$RunGap <- NA
  
  run.Left <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                           pattern = "left") != -1)
  run.Right <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                            pattern = "right") != -1)   
  run.Middle <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                             pattern = "middle") != -1) 
  PBP[Running.Play,"RunLocation"][run.Left] <- "left"
  PBP[Running.Play,"RunLocation"][run.Right] <- "right"
  PBP[Running.Play,"RunLocation"][run.Middle] <- "middle"
  
  runGuard <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                           pattern = "guard") != -1)
  runTackle <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                            pattern = "tackle") != -1)   
  runEnd <- which(sapply(PBP[which(PBP$PlayType == "Run"),"desc"], regexpr, 
                         pattern = "end") != -1) 
  PBP[Running.Play,"RunGap"][runGuard] <- "Guard"
  PBP[Running.Play,"RunGap"][runTackle] <- "tackle"
  PBP[Running.Play,"RunGap"][runEnd] <- "end"
  
  # Rusher
  
  rusherStep1 <- sapply(PBP[which(PBP$PlayType == "Run"),"desc"], str_extract, 
                        pattern = "[A-Z]\\.[A-Z][A-z]{1,20}")
  PBP[Running.Play,"Rusher"] <- rusherStep1
  
  
  # The next few columns will be used for counting purposes
  ##  Used to help set up model for predictions 
  
  # Plays 
  PBP$PlayAttempted <- 1
  
  # Time Under
  PBP$TimeUnder <- substr(ceiling_date(as.POSIXct(paste("00:", PBP$time, sep = "") 
                                                  , format = "%H:%M:%S"), 
                                       "minute"), 15,16)
  PBP$TimeUnder <- as.numeric(as.character(PBP$TimeUnder))
  
  
  # Calculating Score of Game for Possesion team and Defensive Team
  
  teamHomeScore <- rep(0, times = nrow(PBP))
  teamAwayScore <- rep(0, times = nrow(PBP))
  awayTeamName <- nfl.json[[1]]$away$abbr
  homeTeamName <- nfl.json[[1]]$home$abbr
  
  # Away Team
  
  teamAwayScore[which(PBP$Touchdown == 1 & PBP$posteam == awayTeamName)] <- 6
  teamAwayScore[which(PBP$TwoPointConv == "Success" 
                      & PBP$posteam == awayTeamName)] <- 2
  teamAwayScore[which(PBP$ExPointResult == "Good" 
                      & PBP$posteam == awayTeamName)] <- 1
  teamAwayScore[which(PBP$FieldGoalResult == "Good" 
                      & PBP$posteam == awayTeamName)] <- 3
  
  teamAwayScore <- cumsum(teamAwayScore)
  
  awayTeamPos <- which(PBP$posteam == awayTeamName)
  awayTeamDef <- which(PBP$DefensiveTeam == awayTeamName)
  
  # Home Team
  
  teamHomeScore[which(PBP$Touchdown == 1 & PBP$posteam == homeTeamName)] <- 6
  teamHomeScore[which(PBP$TwoPointConv == "Success" 
                      & PBP$posteam == homeTeamName)] <- 2
  teamHomeScore[which(PBP$ExPointResult == "Good" 
                      & PBP$posteam == homeTeamName)] <- 1
  teamHomeScore[which(PBP$FieldGoalResult == "Good" 
                      & PBP$posteam == homeTeamName)] <- 3
  
  teamHomeScore <- cumsum(teamHomeScore)
  
  homeTeamPos <- which(PBP$posteam == homeTeamName)
  homeTeamDef <- which(PBP$DefensiveTeam == homeTeamName)
  
  
  ## Possesion and Defensive Team Scores
  PBP$PosTeamScore <- NA
  PBP$DefTeamScore <- NA
  
  ### Inputting Scores
  PBP$PosTeamScore[homeTeamPos] <- teamHomeScore[homeTeamPos]
  PBP$PosTeamScore[awayTeamPos] <- teamAwayScore[awayTeamPos]
  
  PBP$DefTeamScore[homeTeamDef] <- teamHomeScore[homeTeamDef]
  PBP$DefTeamScore[awayTeamDef] <- teamHomeScore[awayTeamDef]
  
  # Score Differential and Abs Score Differential 
  
  PBP$ScoreDiff <- PBP$PosTeamScore - PBP$DefTeamScore
  PBP$AbsScoreDiff <- abs(PBP$PosTeamScore - PBP$DefTeamScore)
  
  # Goal to Go
  
  PBP$GoalToGo <- ifelse(PBP$posteam != PBP$SideofField & PBP$yrdln <= 10, 1, 0)
  ##################
  ## Final OutPut ##
  PBP
}

## Testing PBP Function on Other Games ##

# Other Games 
nfl.data.url2 <- 
  "http://www.nfl.com/liveupdate/game-center/2015081351/2015081351_gtd.json"

nfl.data.url3 <- 
  "http://www.nfl.com/liveupdate/game-center/2015081353/2015081353_gtd.json"

Plays1 <- Viewable.PBP.Function(nfl.data.url)
Plays2 <- Viewable.PBP.Function(nfl.data.url2)
Plays3 <- Viewable.PBP.Function(nfl.data.url3)


######################################
######################################
# Function for Extracting NFL Game IDS

## Intermediate function  ##
# Used in final data pull #

Extracting_NFL_GameIDs <- function(Year) {
  # This function scrapes the HTML code of NFL websites to pull the game IDs
  # of each game given a year
  #boxscore.
  # Args: 
  #  Year: A numeric form of the year of interest, i.e. 2014
  # Returns: 
  #         The game IDs for each game in a season in the form of strings
  
  ###############
  ###############
  
  library(scrapeR)
  # Setting up to Pull Regular Season Weeks
  urlYearSched <- paste("http://www.nfl.com/schedules", Year, "REG", sep = "/")
  urlScheduleWeeks <- sapply(1:17, FUN = function(x) 
  {paste(urlYearSched, x, sep = "" )})
  gameIDList <- sapply(urlScheduleWeeks, 
                       FUN = function(x) {sourceHTML <- scrape(url = x, 
                                                               headers = TRUE,
                                                               parse = FALSE)
                                          extractGameID <- str_extract_all(unlist(sourceHTML),
                                                                           pattern = "data-gameid=\"[0-9]{10}\"")
                                          gameIDs <- str_extract_all(unlist(extractGameID),
                                                                     pattern = "[0-9]{10}")
                       })
  gameIDList <- unlist(gameIDList)
  names(gameIDList) <- NULL
  gameIDList
}

Games2013 <- Extracting_NFL_GameIDs(2013)


## Intermediate function  ##
# Used in final data pull #
Proper.PBP.URL.Formatting <- function(GameID) {
  # This function pastes together the proper formatting of the nfl play by play 
  # data URL such that it can be used in our PBP function
  # Args: 
  #  GameID: A Single Game ID as a string
  # Returns: 
  #         A url where the game data for the given game can be found
  
  paste("http://www.nfl.com/liveupdate/game-center/",GameID, "/",
        GameID,"_gtd.json", sep = "")
}

Game20151to5 <- sapply(Games2013[1:5], Proper.PBP.URL.Formatting)

Season.PlaybyPlay.Data <- function(Year) {
  # This function outputs all plays of an entire season in one dataframe.  It 
  # call upon the Viewable.PBP.Function and the helper functions 
  # Extracting_NFL_GameIDs and Proper.PBP.URL.Formatting to parse and string 
  # split the URLs where the JSON data is stored.
  # Args:
  #      Year - a numeric input of year
  # Returns: 
  #      A dataframe contains all the play-by-play information for a single
  #      season.  This includes all the statistics collected in our viewable
  #      PBP function created above.
  gameIDS <- Extracting_NFL_GameIDs(Year)
  gameURLs <- sapply(gameIDS, Proper.PBP.URL.Formatting)
  pbp.Data.Unformatted <- lapply(gameURLs, FUN = Viewable.PBP.Function)
  
  df.pbp.data <- do.call(rbind, pbp.Data.Unformatted)
  
  df.pbp.data$sp <- unlist(df.pbp.data$sp)
  df.pbp.data$qtr <- unlist(df.pbp.data$qtr)
  df.pbp.data$down <- unlist(df.pbp.data$down)
  df.pbp.data$time <- unlist(df.pbp.data$time)
  df.pbp.data$ydstogo <- unlist(df.pbp.data$ydstogo)
  df.pbp.data$ydsnet <- unlist(df.pbp.data$ydsnet)
  df.pbp.data$posteam <- unlist(df.pbp.data$posteam)
  df.pbp.data$desc <- unlist(df.pbp.data$desc)
  
  df.pbp.data
}

### Writing all 2014 Data to a CSV ###
pbp.all2014 <- Season.PlaybyPlay.Data(2014)
write.csv(pbp.all2014, file = "NFLPBPData2014.csv")

