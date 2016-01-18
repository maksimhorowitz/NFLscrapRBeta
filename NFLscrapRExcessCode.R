##### Pulling Home Team Statistics #####

# Passing Stats
qbStats <- data.frame(t(sapply(nfl.json.data[[1]]$home$stats$passing, c)))

# Running Stats
rbStats <- data.frame(t(sapply(nfl.json.data[[1]]$home$stats$rushing, c)))

# Receiving Stats
wrStats <- data.frame(t(sapply(nfl.json.data[[1]]$home$stats$receiving, c)))

# Defensive Stats
defStats <- data.frame(t(sapply(nfl.json.data[[1]]$home$stats$defense, c)))

# Kicking Stats
kickerStats <- data.frame(t(sapply(nfl.json.data[[1]]$home$stats$kicking, c)))

# Fumble Stats
fumbStats <- data.frame(t(sapply(nfl.json.data[[1]]$home$stats$fumbles, c)))

# Kick Return Stats
krstats <- data.frame(t(sapply(nfl.json.data[[1]]$home$stats$kickret, c)))

# Punt Return Stats
prStats <- data.frame(t(sapply(nfl.json.data[[1]]$home$stats$puntret, c)))




## Passer

Passer.Step1 <- sapply(Test.DES, str_extract, pattern = "[A-Z]\\.[A-Z][a-z]{1,20} pass")
Passer <- str_extract(Passer.Step1, pattern = "[A-Z]\\.[A-Z][a-z]{1,20}")

## Runner 


## Receiver

Receiver.Step1 <- sapply(Test.DES, str_extract, 
                         pattern = "pass (incomplete)?( )?[a-z]{4,5} [a-z]{4,6} to [A-Z]\\.[A-Z][a-z]{1,20}")

Receiver <- str_extract(Receiver.Step1, pattern = "[A-Z]\\.[A-Z][a-z]{1,20}")

## Tacklers

Tacklers.Step1 <- sapply(Test.DES, str_extract, 
                         pattern = "yard(s?) \\([A-Z]\\.[A-Z][a-z]{1,20}(;)?( )?([A-Z]\\.[A-Z][a-z]{1,20})?\\)\\.")

sapply(Test.DES, str_extract, 
       pattern = "yard(s?) \\([A-Z]\\.([A-Z][A-z]{1,3})?(\\.)?( )?[A-Z][A-z]{1,20}(;)?( )?([A-Z]\\.([A-Z][A-z]{1,3})?(\\.)?( )?[A-Z][A-z]{1,20})?\\)\\.")

Tacklers1 <- str_extract(Tacklers.Step1, pattern = "\\([A-Z]\\.[A-Z][a-z]{1,20}")
Tacklers1 <- str_extract(Tacklers1, pattern = "[A-Z]\\.[A-Z][a-z]{1,20}")

Tacklers2 <- str_extract(Tacklers.Step1, pattern = ";( )[A-Z]\\.[A-Z][a-z]{1,20}")
Tacklers2 <- str_extract(Tacklers2, pattern = "[A-Z]\\.[A-Z][a-z]{1,20}")



## Type of Play ##

Plays1$PlayType <- NA



# Pass Plays
Pass.Play <- which(sapply(Test.DES, regexpr, pattern = "pass") != -1)
Incomplete.Pass.Play <- which(sapply(Test.DES, regexpr, pattern = "pass incomplete") != -1)

Plays1$PlayType[Pass.Play] <- "Pass"
Plays1$PlayType[Incomplete.Pass.Play] <- "Incomplete Pass"

# Kick Off
Kick.Off.Play <- which(sapply(Test.DES, regexpr, pattern = "kick") != -1)

Plays1$PlayType[Kick.Off.Play] <- "Kick-Off"

# Punt
Punt.Play <- which(sapply(Test.DES, regexpr, pattern = "punts") != -1)

Plays1$PlayType[Punt.Play] <- "Punt"

# Field Goal
made.FG <- which(sapply(Test.DES, regexpr, pattern = "field goal is GOOD") != -1)
missed.FG <- which(sapply(Test.DES, regexpr, pattern = "field goal is No Good") != -1)

Plays1$PlayType[made.FG] <- "Field Goal"
Plays1$PlayType[missed.FG] <- "Missed FG"

# Extra Point
Made.Extra.Point.Plays <- which(sapply(Test.DES, regexpr, pattern = "extra point is GOOD") != -1)
Missed.Extra.Point.Plays <- which(sapply(Test.DES, regexpr, pattern = "extra point is No Good") != -1)

Plays1$PlayType[Made.Extra.Point.Plays] <- "Extra-Point"

# Time Outs
TimeOuts <- which(sapply(Test.DES, regexpr, pattern = "[A-z]imeout #[1-5] by") != -1)

Plays1$PlayType[TimeOuts] <- "Timeout"

# Quarter End
End.Quarter <- which(sapply(Test.DES, regexpr, pattern = "END QUARTER") != -1)

Plays1$PlayType[End.Quarter] <- "QuarterEnd"

# 2 Minute Warning
Two.Minute.Warning <- which(sapply(Test.DES, regexpr, pattern = "Two-Minute Warning") != -1)

Plays1$PlayType[Two.Minute.Warning] <- "Two Minute Warning"

# Sack 
Sack.Plays <- which(sapply(Test.DES, regexpr, pattern = "sacked") != -1)

Plays1$PlayType[Sack.Plays] <- "Sack"

# QB Kneel
Kneel.Play <- which(sapply(Test.DES, regexpr, pattern = "kneels") != -1)

Plays1$PlayType[Kneel.Play] <- "QB Kneel"


# Penalty
##Tenative May need to change how this is done
Penalty.Plays <- which(sapply(Test.DES, regexpr, pattern = "PENALTY") != -1)


PBP2014 <- lapply(gameURLs2014, FUN = Viewable.PBP.Function)

PBP2014DF <- do.call(rbind, PBP2014)

PBP2014DF$sp <- unlist(PBP2014DF$sp)
PBP2014DF$qtr <- unlist(PBP2014DF$qtr)
PBP2014DF$down <- unlist(PBP2014DF$down)
PBP2014DF$time <- unlist(PBP2014DF$time)
PBP2014DF$ydstogo <- unlist(PBP2014DF$ydstogo)
PBP2014DF$ydsnet <- unlist(PBP2014DF$ydsnet)
PBP2014DF$posteam <- unlist(PBP2014DF$posteam)
PBP2014DF$desc <- unlist(PBP2014DF$desc)




View(Plays1[c(11,10)])

## Offensive Players Involved 


#nfl.json.data[[1]]$drives[[2]]$posteam
#nfl.json.data[[1]]$drives[[2]]$qtr
#nfl.json.data[[1]]$drives[[2]]$redzone
#nfl.json.data[[1]]$drives[[2]]$plays
#nfl.json.data[[1]]$drives[[2]]$fds
#nfl.json.data[[1]]$drives[[2]]$result
#nfl.json.data[[1]]$drives[[2]]$penyds
#nfl.json.data[[1]]$drives[[2]]$ydsgained
#nfl.json.data[[1]]$drives[[2]]$numplays
#nfl.json.data[[1]]$drives[[2]]$postime
#nfl.json.data[[1]]$drives[[2]]$start
#nfl.json.data[[1]]$drives[[2]]$end


#### Player Game Test Code ####

dfpass <- data.frame(t(sapply(nfl.json.data[[1]]$home$stats$passing, c)))

dfrush <- data.frame(t(sapply(nfl.json.data[[1]]$home$stats$rushing, c)))

dfrec <- data.frame(t(sapply(nfl.json.data[[1]][[1]]$stats$receiving, c)))

dffumb <- data.frame(t(sapply(nfl.json.data[[1]][[1]]$stats$fumbles, c)))
colnames(dffumb)[ncol(dffumb)] <- colnames(dfrec)[ncol(dfrec)]

rbind(data.frame(t(sapply(nfl.json.data[[1]][[1]]$stats$passing, c))),
      data.frame(t(sapply(nfl.json.data[[1]][[2]]$stats$passing, c))))
dfpass$playerID <- rownames(dfpass)
dfrush$playerID <- rownames(dfrush)
dfrec$playerID <- rownames(dfrec)
dffumb$playerID <- rownames(dffumb)

testmerge <- Reduce(function(x, y) {merge(x, y, by = c("playerID", "name"),all=TRUE)},
                    list(dfpass, dfrush, dffumb, dfrec))

mergeddf <- merge(dfrec, dffumb, by = c("playerID", "name"),
                  sort = FALSE, all = TRUE)

mergeddf <- merge(mergeddf, dffumb, by = c("playerID", "name"), 
                  sort = FALSE, all = TRUE, incomparables = NA)


