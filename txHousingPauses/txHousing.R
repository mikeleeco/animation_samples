
library(ggplot2)
library(scales)
library(extrafont)
library(dplyr)
library(tweenr)
library(lubridate)
library(animation)
library(RColorBrewer)
library(grid)
loadfonts()


# create data frame from txhousing data included in tidyverse package
txHousingData <- txhousing %>% filter(city %in% unique(txhousing$city)[1:2] & month==1)
txHousingData <- txHousingData[,c(1,2,6)]
txHousingData$city <- factor(txHousingData$city, levels = c("Abilene","Amarillo"), labels = c("Abilene","Amarillo"))

# determine the unique data values that tweenr will iterate over (in this case year)
txList <- split(txHousingData,txHousingData$year)

# Apply tweenr to interpolate frames between data points
txTweenedData <- tween_states(txList, tweenlength= 1, statelength=0, ease='linear',nframes=200)

# plot aesthetics
theme_white <- theme(text = element_text(family="Open Sans"),
                     panel.grid.major.y=element_blank(),
                     panel.grid.major.x=element_blank(),
                     panel.grid.minor.x=element_blank(),
                     panel.grid.minor.y=element_blank(),
                     axis.title.x=element_text(size=20, margin = margin(t=10)),
                     axis.title.y=element_text(size=20, margin = margin(t=10)),
                     axis.text.x=element_text(size=16),
                     axis.text.y=element_text(size=16),
                     axis.ticks = element_blank(),
                     plot.title=element_text(size=26,family = "Open Sans",lineheight=1.15),
                     plot.subtitle=element_text(size=20, margin = margin(t=20, b = -5),hjust = -0.03, family = "Open Sans"),
                     plot.caption=element_text(size=14, margin=margin(t=15,r=-15),hjust = -.03,lineheight=1.15, family = "Open Sans", face  = "italic"),
                     legend.position="none"
)

# color palette
my_palette = c("#3EC7F4", "#3FA66C")

# y axis breaks and labels
yAxisBreaks <- seq(0, 140000, by = 20000)
yAxisLabels <- paste0(format(yAxisBreaks))
yAxisLimits <- c(0,max(yAxisBreaks)* 1.03)
yAxisLabels <- c(yAxisLabels[1:length(yAxisLabels)-1], paste0("$",yAxisLabels[length(yAxisLabels)]))

# x axis breaks and labels
xAxis <- seq(2000, 2015, by = 2)
xAxisBreaks <- xAxis
xAxisLimits <- c(1999.5, 2019)

# plot text
title <- "Median sale price of homes in Abilene and Amarillo, 2000 through 2015"
subtitle <- ""
caption <- "Information about the housing market in Texas provided by the TAMU real estate center, http://recenter.tamu.edu/."
xLab <- "Year"
yLab <- "Median sale price"

midPoint <- round(max(txTweenedData$.frame) / 2)

# add adjustments for line labels
txTweenedData$LevelLabel <- as.character(txTweenedData$city)
txTweenedData$nudge_x <- ifelse(txTweenedData$LevelLabel=="Abilene", 1.096,ifelse(txTweenedData$LevelLabel=="Amarillo",1.233,0))

saveGIF({
  
  # print from tue first frame to the midpoint of the figure
  for(i in 1:midPoint) {
    g1 <-  ggplot(data = subset(txTweenedData, .frame <= i), aes(x = year, y = median, .frame = i)) +
      geom_point(data = subset(txTweenedData, .frame == min(.frame)),aes(group=city, color=city), size=5) + 
      geom_line(aes(group=city, color=city, cumulative = TRUE, label=LevelLabel), size=2.5) +
      scale_x_continuous(labels=xAxisBreaks, expand = c(0, 0), breaks=xAxisBreaks, limits =xAxisLimits) +                     
      scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
      theme_minimal() + theme_white + scale_color_manual(values=my_palette) +
      geom_text(data = subset(txTweenedData, .frame == i),aes(label =LevelLabel), size = 9, nudge_y = subset(txTweenedData, .frame == i)$nudge_y, nudge_x = subset(txTweenedData, .frame == i)$nudge_x, family = "Open Sans", lineheight = 0.75) +
      labs(x=xLab, y=yLab, title = title, subtitle = subtitle, 
                 caption = caption)
      print(g1);  
    }
  
  # print the midpoint 75 times to create a pause
  g2 <- ggplot(data = subset(txTweenedData, .frame <= midPoint), aes(x = year, y = median, .frame = i)) +
      geom_point(data = subset(txTweenedData, .frame == min(.frame)),aes(group=city, color=city), size=5) + 
      geom_point(data = subset(txTweenedData, .frame == midPoint),aes(group=city, color=city), size=5) + 
      geom_line(aes(group=city, color=city, cumulative = TRUE, label=LevelLabel), size=2.5) +
      scale_x_continuous(labels=xAxisBreaks, expand = c(0, 0), breaks=xAxisBreaks, limits =xAxisLimits) +                     
      scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
      theme_minimal() + theme_white + scale_color_manual(values=my_palette) +
      geom_text(data = subset(txTweenedData, .frame == midPoint),aes(label =LevelLabel), size = 9, nudge_y = subset(txTweenedData, .frame == midPoint)$nudge_y, nudge_x = subset(txTweenedData, .frame == midPoint)$nudge_x, family = "Open Sans", lineheight = 0.75) + 
      labs(x=xLab, y=yLab, title = title, subtitle = subtitle, 
                 caption = caption)

  replicate(75,grid.draw(g2))
  
  # print from tue midpoint of the figure until the last frame
  for(i in midPoint:max(txTweenedData$.frame)) {
    g3 <- ggplot(data = subset(txTweenedData, .frame <= i), aes(x = year, y = median, .frame = i)) +
      geom_point(data = subset(txTweenedData, .frame == min(.frame)),aes(group=city, color=city), size=5) + 
      geom_line(aes(group=city, color=city, cumulative = TRUE, label=LevelLabel), size=2.5) +
      scale_x_continuous(labels=xAxisBreaks, expand = c(0, 0), breaks=xAxisBreaks, limits =xAxisLimits) +                     
      scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
      theme_minimal() + theme_white + scale_color_manual(values=my_palette) +
      geom_text(data = subset(txTweenedData, .frame == i),aes(label =LevelLabel), size = 9, nudge_y = subset(txTweenedData, .frame == i)$nudge_y, nudge_x = subset(txTweenedData, .frame == i)$nudge_x, family = "Open Sans", lineheight = 0.75) +
      labs(x=xLab, y=yLab, title = title, subtitle = subtitle, 
                 caption = caption)
    print(g3);  }

  # print the final frame of the figure 110 times to create an ending pause 
  wholeFig <- ggplot(data = subset(txTweenedData, .frame <= i), aes(x = year, y = median, .frame = i)) +
      geom_point(data = subset(txTweenedData, .frame == min(.frame)),aes(group=city, color=city), size=5) + 
      geom_point(data = subset(txTweenedData, .frame == i),aes(group=city, color=city), size=5) +
      geom_line(aes(group=city, color=city, cumulative = TRUE, label=LevelLabel), size=2.5) +
      scale_x_continuous(labels=xAxisBreaks, expand = c(0, 0), breaks=xAxisBreaks, limits =xAxisLimits) +                     
      scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
      theme_minimal() + theme_white + scale_color_manual(values=my_palette) +
      geom_text(data = subset(txTweenedData, .frame == i),aes(label =LevelLabel), size = 9, nudge_y = subset(txTweenedData, .frame == i)$nudge_y, nudge_x = subset(txTweenedData, .frame == i)$nudge_x, family = "Open Sans", lineheight = 0.75) +
      labs(x=xLab, y=yLab, title = title, subtitle = subtitle, 
                 caption = caption)   
  
  replicate(110,grid.draw(wholeFig))
},movie.name="/home/michael/Documents/animation_samples/txHousing/txHousing2.gif",interval = .02, ani.width = 1024, ani.height = 612)

# compress gif
gif_compress <- function(ingif, outgif, show=TRUE, extra.opts=""){
  command <-  sprintf("gifsicle -O3 %s < %s > %s", extra.opts, ingif, outgif)
  system.fun <- if (.Platform$OS.type == "windows") shell else system
  if(show) message("Executing: ", strwrap(command, exdent = 2, prefix = "\n"))
  system.fun(ifelse(.Platform$OS.type == "windows", sprintf("\"%s\"", shQuote(command)), command))
}

gif_compress("/home/michael/Documents/animation_samples/txHousing/txHousing2.gif","/home/michael/Documents/animation_samples/txHousing/tx_Housing2.gif",extra.opts="--colors 256")
