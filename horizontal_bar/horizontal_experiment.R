library(ggplot2)
library(gganimate)
library(tweenr)
library(dplyr)
library(RColorBrewer)
library(animation)
majorData <- read.csv("major.csv")
# g <- ggplot(majorData, aes(x = Immigrant, y = Percent, frame = Major))
# g <- g + geom_bar(aes(fill = Percent), position = "fill", stat="identity")
# gg_animate(g,title_frame = FALSE)

majorData$Major <- factor(majorData$Major, 
                          levels = c("Unknown","Engineering","SocialSci","Health", "Business", "Humanities", "Education"), 
                          labels = c("Unknown/\nundecided","Engineering,\nmath, computer,\nand physical sciences","Social sciences,\nincluding\npsychology",
                                     "Health\ncare\nfields", "Business", "Humanities,\ngeneral studies,\nand history", "Education and\nother applied"))

# states2<-majorData$Major
# majorData %>% group_by(Immigrant) %>% mutate(roll_sum = cumsum(Percent)) -> majorData
# majorData <- majorData[,c(1,2,4)]

majorData %>% group_by(Immigrant) %>% mutate(roll_sum2 = cumsum(Percent)) -> majorData
majorData %>% group_by(Immigrant) %>% mutate(roll_sum3 = roll_sum2/sum(roll_sum2)) -> majorData
majorData <- majorData[,c(1,2,5)]

myf<-function(mystate){as.data.frame(majorData[majorData$Major==mystate,])}

# use lapply to generate the list of data sets:
my.list2<-lapply(unique(majorData$Major),myf)
tf <- tween_states(my.list2, tweenlength=2, statelength=1, ease=rep('elastic-out',7),nframes=40)

tf$Major <- factor(tf$Major,
                   levels = rev(c("Unknown/\nundecided","Engineering,\nmath, computer,\nand physical sciences","Social sciences,\nincluding\npsychology",
                                  "Health\ncare\nfields", "Business", "Humanities,\ngeneral studies,\nand history", "Education and\nother applied")))

# g <- ggplot(tf, aes(x = Immigrant, y = roll_sum,  frame=.frame))
# g <- g + geom_bar(aes(fill = Major), position = "fill")
tf2 <- tf
# gganimate(g,title_frame = FALSE)

theme_white <- theme(
  panel.background=element_blank(),
  panel.grid.major.y=element_blank(),
  panel.grid.major.x=element_line(color="gray", linetype="dashed"),
  panel.grid.minor.x=element_blank(),
  panel.grid.minor.y=element_blank(),
  axis.title.y = element_blank(),
  axis.title.x = element_text(size=14),
  # axis.line.x=element_line(color="black"),
  # axis.line.y=element_line(color="black"),
  axis.text.x=element_text(size=12),
  axis.text.y=element_text(size=12),
  axis.ticks.x=element_line(),
  axis.ticks.y=element_blank(),
  # axis.ticks.length=unit(5, "pt"),
  # plot.margin=unit(rep(0.5, 4), "cm"),
  # axis.text.y=element_text(margin=margin(r=-5)),
  plot.title=element_text(size=18,face = "bold",lineheight=1.15),
  plot.subtitle=element_text(family="sans",size=12, margin = margin(t=15)),
  plot.caption=element_text(size=12, hjust=0, margin=margin(t=15),lineheight=1.15)
)
# black, native, hisp
mypalette <- brewer.pal(7,"Greens")

oopt = ani.options(interval = 0.2)

saveGIF({for (i in 1:max(tf2$.frame)) {
  g<-
    ggplot(data=subset(tf2,.frame<=i), aes(x = Immigrant, y= roll_sum3,frame=.frame)) +
      scale_y_continuous(limits=c(0, 100)) + coord_flip() +
    geom_col(aes(fill = Major)) + theme_white + scale_fill_manual(values=mypalette, drop=FALSE)
  print(g); ani.pause()
}},movie.name="ehs ytd july 2016.gif",ani.width = 800, ani.height = 450)