# much of this code comes from https://benjaminlmoore.wordpress.com/2015/04/09/recreating-the-vaccination-heatmaps-in-r/
library(reshape2)
library(ggplot2)
library(dplyr)
library(animation)
library(gganimate)
setwd(paste0(getwd(),"/animation_samples/measles"))
measles <- read.csv("measles.csv", skip=2)
measles <- melt(measles, id.var=c("YEAR", "WEEK"))
colnames(measles) <- c("year", "week", "state", "cases")
measles$cases <- ifelse(measles$cases=="\u002D",NA,measles$cases)
measles$cases <- as.numeric(measles$cases)
stateAP <- read.csv("stateName.csv")
stateAP$stateName <- toupper(stateAP$stateName)
measles$state <- stateAP$stateAP[match(measles$state,stateAP$stateName)]

mdf <- measles %>% group_by(state, year) %>% 
  summarise(c=if(all(is.na(cases))) NA else
    sum(cases, na.rm=T))
mdf$state <- factor(mdf$state, levels=rev(levels(mdf$state)))


cols<- c("#e7f0fa", #lighter than light blue
         "#c9e2f6", #light blue
         "#95cbee", #blue
         "#0099dc", #darker blue
         "#4ab04a", #green
         "#ffd73e", #yellow
         "#eec73a", #mustard
         "#e29421", #dark khaki (?)
         "#f05336", #orange red
         "#ce472e") #red

gg <- ggplot(mdf, aes(y=state, x=year, fill=c, frame=year)) + 
  geom_tile(colour="white", linewidth=2, 
            width=.9, height=.9) + theme_minimal() +
  scale_fill_gradientn(colours=cols, limits=c(0, 4000),
                       values=c(0,0.01, 0.02, 0.03, 0.09, 0.1, .15, .25, .4, .5,1), 
                       na.value=rgb(246, 246, 246, max=255),
                       labels=c("0k", "1k", "2k", "3k", "4k"),
                       guide=guide_colourbar(ticks=T, nbin=50,
                                             barheight=.5, label=T, 
                                             barwidth=10)) +
  scale_x_continuous(expand=c(0,0), 
                     breaks=seq(1930, 2010, by=10)) +
  geom_segment(x=1963, xend=1963, y=0, yend=51.5, size=.9) +
  labs(x="", y="", fill="") +
  ggtitle("Measles") +
  theme(legend.position=c(.5, -.13),
        legend.direction="horizontal",
        legend.text=element_text(colour="grey20"),
        plot.margin=grid::unit(c(.5,.5,1.5,.5), "cm"),
        axis.text.y=element_text(size=6, family="Helvetica", 
                                 hjust=1),
        axis.text.x=element_text(size=8),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank(),
        title=element_text(hjust=-.07, face="bold", vjust=1, 
                           family="Helvetica"),
        text=element_text(family="URWHelvetica")) +
  annotate("text", label="Vaccine introduced", x=1963, y=53, 
           vjust=1, hjust=0, size=I(3), family="Helvetica")

gganimate(gg, "measles.gif")

# cool fuckup - didn't subset correctly
gg <- ggplot(subset(mdf, year==1928+i-1), aes(y=state, x=year, fill=c, frame=year)) 


# accordion - didn't set limits in x axis
scale_x_continuous(expand=c(0,0), 
                   breaks=seq(1930, 2010, by=10)) 

# what we want!
saveGIF(
  for (i in 1:76) {
    
    gg <- ggplot(subset(mdf, year<=1928+i-1), aes(y=state, x=year, fill=c, frame=year)) + 
      geom_tile(colour="white", linewidth=2, 
                width=.9, height=.9) + theme_minimal() +
      scale_fill_gradientn(colours=cols, limits=c(0, 4000),
                           values=c(0,0.01, 0.02, 0.03, 0.09, 0.1, .15, .25, .4, .5,1), 
                           na.value=rgb(246, 246, 246, max=255),
                           labels=c("0k", "1k", "2k", "3k", "4k"),
                           guide=guide_colourbar(ticks=T, nbin=50,
                                                 barheight=.5, label=T, 
                                                 barwidth=10)) +
      scale_x_continuous(expand=c(0,0), 
                         breaks=seq(1930, 2010, by=10), limits = c(1928,2003))
      if(max(subset(mdf, year<=1928+i-1)$year) >= 1963) {
        
     gg <- gg + geom_segment(x=1963, xend=1963, y=0, yend=51.5, size=.9) +
          annotate("text", label="Vaccine introduced", x=1963, y=53, 
                   vjust=1, hjust=0, size=I(3), family="Helvetica")
      } else {
        gg <- gg +
          annotate("text", label="", x=1963, y=53, 
                   vjust=1, hjust=0, size=I(3), family="Helvetica")
      }
    gg <- gg + labs(x="", y="", fill="") +
      ggtitle("Measles") +
      theme(legend.position=c(.5, -.075),
            legend.direction="horizontal",
            legend.text=element_text(colour="grey20"),
            plot.margin=grid::unit(c(.5,.5,1.5,.5), "cm"),
            axis.text.y=element_text(size=8, family="Helvetica", 
                                     hjust=1),
            axis.text.x=element_text(size=8),
            axis.ticks.y=element_blank(),
            panel.grid=element_blank(),
            title=element_text(hjust=-.07, face="bold", vjust=1, 
                               family="Helvetica"),
            text=element_text(family="URWHelvetica"))
    
    if(max(subset(mdf, year<=1928+i-1)$year) == 1963) {
      replicate(30,grid.draw(gg))
    } else if (max(subset(mdf, year<=1928+i-1)$year) == 2003) {
        replicate(30,grid.draw(gg))
  } else {
  print(gg)
}
  }
  ,movie.name="measles.gif",interval = .1, ani.width = 1024, ani.height = 612)
