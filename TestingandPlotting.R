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