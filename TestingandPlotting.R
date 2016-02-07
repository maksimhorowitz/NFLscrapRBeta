## Testing and Plotting Script ##

########## Qbs with over 2,000 passing yard from 2010-2013

system.time(offensivestate2010 <- PlayerSeasonStats.Function(2010))
system.time(offensivestate2011 <- PlayerSeasonStats.Function(2011))
system.time(offensivestate2012 <- PlayerSeasonStats.Function(2012))
system.time(offensivestate2013 <- PlayerSeasonStats.Function(2013))

qbs2010 <- subset(offensivestate2010, yds.x > 2000)


## QBs 2010
qbsplot2010 <- ggplot(data = qbs2010, aes(x = name, y = yds.x)) + geom_bar(stat="identity",
                                                                           fill = "black", 
                                                                           colour = "red",
                                                                           aes(alpha= yds.x)) +
  scale_alpha_continuous(guide=FALSE) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Quarterbacks", y = "Yards (Season)", 
       title = "Quarterbacks with over 2000 passing yards in 2010") +
  theme(plot.title = element_text(size = rel(1.5), face = "bold"), 
        axis.text.x = element_text(size = rel(1.25), colour = "black"),
        axis.text.y = element_text(size = rel(1.25), colour = "black"),
        axis.title.y = element_text(size = rel(1.25)),
        axis.title.x = element_text(size = rel(1.25)))

## Qbs 2011

qbs2011 <- subset(offensivestate2011, yds.x > 2000)

qbsplot2011 <- ggplot(data = qbs2011, aes(x = name, y = yds.x)) + geom_bar(stat="identity",
                                                                           fill = "goldenrod3", 
                                                                           colour = "navy",
                                                                           aes(alpha= yds.x)) +
  
  scale_alpha_continuous(guide=FALSE) +                                                             
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Quarterbacks", y = "Yards (Season)", 
       title = "Quarterbacks with over 2000 passing yards in 2011") +
  theme(plot.title = element_text(size = rel(1.5), face = "bold"), 
        axis.text.x = element_text(size = rel(1.25), colour = "black"),
        axis.text.y = element_text(size = rel(1.25), colour = "black"),
        axis.title.y = element_text(size = rel(1.25)),
        axis.title.x = element_text(size = rel(1.25)))

## Qbs 2012

qbs2012 <- subset(offensivestate2012, yds.x > 2000)

qbsplot2012 <- ggplot(data = qbs2012, aes(x = name, y = yds.x)) + geom_bar(stat="identity",
                                                                           fill = "orangered2", 
                                                                           colour = "steelblue1",
                                                                           aes(alpha= yds.x)) +
  
  scale_alpha_continuous(guide=FALSE) +                                                             
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Quarterbacks", y = "Yards (Season)", 
       title = "Quarterbacks with over 2000 passing yards in 2012") +
  theme(plot.title = element_text(size = rel(1.5), face = "bold"), 
        axis.text.x = element_text(size = rel(1.25), colour = "black"),
        axis.text.y = element_text(size = rel(1.25), colour = "black"),
        axis.title.y = element_text(size = rel(1.25)),
        axis.title.x = element_text(size = rel(1.25)))

## QBs 2013

qbs2013 <- subset(offensivestate2013, yds.x > 2000)

qbsplot2013 <- ggplot(data = qbs2013, aes(x = name, y = yds.x)) + geom_bar(stat="identity",
                                                                           fill = "springgreen3", 
                                                                           colour = "mediumorchid1",
                                                                           aes(alpha= yds.x)) +
  
  scale_alpha_continuous(guide=FALSE) +                                                             
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Quarterbacks", y = "Yards (Season)", 
       title = "Quarterbacks with over 2000 passing yards in 2013") +
  theme(plot.title = element_text(size = rel(1.5), face = "bold"), 
        axis.text.x = element_text(size = rel(1.25), colour = "black"),
        axis.text.y = element_text(size = rel(1.25), colour = "black"),
        axis.title.y = element_text(size = rel(1.25)),
        axis.title.x = element_text(size = rel(1.25)))


grid.arrange(qbsplot2010,
             qbsplot2011,
             qbsplot2012,
             qbsplot2013, nrow = 2, ncol = 2)

## Combining all playergames from 2011-2014


### Plot for Sams Talk

aggstats2010 <- PlayerSeasonStats.Function(2010)
aggstats2011 <- PlayerSeasonStats.Function(2011)
aggstats2012 <- PlayerSeasonStats.Function(2012)
aggstats2013 <- PlayerSeasonStats.Function(2013)
aggstats2014 <- PlayerSeasonStats.Function(2014)
aggstats2015 <- PlayerSeasonStats.Function(2015)

#Manning

pmanning2010 <- subset(aggstats2010, name == "P.Manning")
pmanning2010$yrdsperattempt <- pmanning2010$yds.x / pmanning2010$att.x

pmanning2012 <- subset(aggstats2012, name == "P.Manning")
pmanning2012$yrdsperattempt <- pmanning2012$yds.x / pmanning2012$att.x

pmanning2013 <- subset(aggstats2013, name == "P.Manning")
pmanning2013$yrdsperattempt <- pmanning2013$yds.x / pmanning2013$att.x

pmanning2014 <- subset(aggstats2014, name == "P.Manning")
pmanning2014$yrdsperattempt <- pmanning2014$yds.x / pmanning2014$att.x

pmanning2015 <- subset(aggstats2015, name == "P.Manning")
pmanning2015$yrdsperattempt <- pmanning2015$yds.x / pmanning2015$att.x

# Newton

cnewton2012 <- subset(aggstats2012, name == "C.Newton")
cnewton2012$yrdsperattempt <- cnewton2012$yds.x / cnewton2012$att.x

cnewton2013 <- subset(aggstats2013, name == "C.Newton")
cnewton2013$yrdsperattempt <- cnewton2013$yds.x / cnewton2013$att.x

cnewton2014 <- subset(aggstats2014, name == "C.Newton")
cnewton2014$yrdsperattempt <- cnewton2014$yds.x / cnewton2014$att.x

cnewton2015 <- subset(aggstats2015, name == "C.Newton")
cnewton2015$yrdsperattempt <- cnewton2015$yds.x / cnewton2015$att.x

# 
manningnewton.peratt <- data.frame(Year = c(2012:2015,2012:2015), 
                                   Player = c(rep("Manning", times =4),
                                              rep("Newton", times = 4)),
                                  YardsperAttempt = 
                                   c(pmanning2012$yrdsperattempt,
                                     pmanning2013$yrdsperattempt,
                                     pmanning2014$yrdsperattempt,
                                     pmanning2015$yrdsperattempt,
                                     cnewton2012$yrdsperattempt,
                                     cnewton2013$yrdsperattempt,
                                    cnewton2014$yrdsperattempt,
                                    cnewton2015$yrdsperattempt))

ggplot(data = manningnewton.peratt, aes(x = Year, 
                                        y = YardsperAttempt)) + 
         geom_bar(stat="identity", colour = "mediumorchid1", position = "dodge"
                  ) + aes(fill = Player) +                                                           
labs(x = "Year", y = "Yards Per Attempt", 
title = "C. Newton vs. P. Manning Yards Per Attempt 2012-2015") +
  theme(plot.title = element_text(size = rel(1.5), face = "bold"), 
        axis.text.x = element_text(size = rel(1.5), colour = "black"),
        axis.text.y = element_text(size = rel(1.5), colour = "black"),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5))) +
  scale_fill_manual(values = c("chocolate1", "cyan3"), guide=FALSE)




# Combining them into one dataframe

PlayerGame10to14 <- rbind(PlayerGame2010, PlayerGame2011, PlayerGame2012,
                          PlayerGame2013, PlayerGame2014)
PlayerGame10to14 <- data.frame(PlayerGame10to14)

# Writing the CSV

write.csv(PlayerGame10to14, file = "PlayerGame10to14.csv")



