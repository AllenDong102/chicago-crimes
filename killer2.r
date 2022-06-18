##WEEK 6 SATURDAY
#only some are used
library(data.table)
library(ggplot2)
library(dplyr)
library(maps)
library(magrittr)
library(ggmap)
library(lubridate)
library(tidyverse)
library(ggrepel)
library(devtools)
library(grid) 

#import data
data1 <- fread("chicago_crimes.csv")

#takes a list of years as input, returns a square plot
#acts as helper function for killer function
square_plot <- function(yrs) {

  data2 <- subset(data1, data1$Year %in% yrs) 
  
  num <- nrow(data2)
  A1 <- nrow(subset(data2, data2$Arrest == TRUE & data2$Domestic == TRUE))/num
  A2 <- nrow(subset(data2, data2$Arrest == TRUE & data2$Domestic == FALSE))/num
  A3 <- nrow(subset(data2, data2$Arrest ==  FALSE & data2$Domestic == TRUE))/num
  A4 <- nrow(subset(data2, data2$Arrest == FALSE & data2$Domestic == FALSE))/num

  vp0 <- viewport(x = 0.5, y = 0.5, width = 0.8, height = 0.8)
  
  pushViewport(vp0)
  
  #draws squre outline
  grid.rect(gp = gpar(lwd = 1), vp = vp0)
  grid.lines(x = unit(c(0, 1), 'npc'),
             y = unit(c(0.5, 0.5), 'npc'),
             vp = vp0)
  grid.lines(x = unit(c(0.5, 0.5), 'npc'),
             y = unit(c(0, 1), 'npc'),
             vp = vp0)
  
  a <- 2.2 #adjusts all square size
  vp1 <- viewport(x = 0.5, y = 0.5, width = sqrt(A1)/a, height = sqrt(A1)/a,
                  just = c("right", "bottom"))
  vp2 <- viewport(x = 0.5, y = 0.5, width = sqrt(A2)/a, height = sqrt(A2)/a,
                  just = c("left", "bottom"))
  vp3 <- viewport(x = 0.5, y = 0.5, width = sqrt(A3)/a, height = sqrt(A3)/a,
                  just = c("right", "top"))
  vp4 <- viewport(x = 0.5, y = 0.5, width = sqrt(A4)/a, height = sqrt(A4)/a,
                  just = c("left", "top"))

  #plots four squares
  pushViewport(vp1)
  grid.rect(gp = gpar(lwd = 3))
  popViewport()

  pushViewport(vp2)
  grid.rect(gp = gpar(lwd = 3))
  popViewport()

  pushViewport(vp3)
  grid.rect(gp = gpar(lwd = 3))
  popViewport()

  pushViewport(vp4)
  grid.rect(gp = gpar(lwd = 3))
  popViewport()
  
  #add text
  pushViewport(vp0)
  fs <- 8
  grid.text('Arrested', x = -0.08, y = 0.75, rot = 90,
            gp = gpar(fontsize = fs))
  grid.text('Not arrested', x = -0.08, y = 0.25, rot = 90,
            gp = gpar(fontsize = fs))
  grid.text('Domestic', x = 0.25, y = -0.07,
            gp = gpar(fontsize = fs))
  grid.text('Not domestic', x = 0.75, y = -0.07,
            gp = gpar(fontsize = fs))
  #add plural depending on number of years
  if (length(yrs) == 1) {
    grid.text(paste0('Year: ', yrs),
              x = 0.5, y = 1.08, gp = gpar(fontsize = fs))
  }
  else {
    grid.text(paste0('Years: ', yrs[1], ' - ', yrs[length(yrs)]), 
              x = 0.5, y = 1.08, gp = gpar(fontsize = fs))
  }
  popViewport(2)
}


#test: one square plot, one year
grid.newpage()
square_plot(2021)
#test: one square plot, more than one year
grid.newpage()
square_plot(2019:2021)


#takes a vector of years and returns nine squre plots
killer <- function(yrs) {
  
  ly <- grid.layout(3, 3)

  grid.newpage()
  pushViewport(viewport(layout = ly))
  # showViewport()
  # grid.show.layout(ly)
  
  for (i in 1:9) {
    vp <- viewport(layout.pos.row = (i - 1) / 3 + 1,
                   layout.pos.col = (i - 1) %% 3 + 1)
    pushViewport(vp)
    print(square_plot(yrs[[i]]))
    popViewport()
  }
  
}

#test
yrs <- list(2001:2003, 2004:2006, 2007:2009, 
         2010:2012, 2013:2015, 2016:2018,
         2019, 2020, 2021)
killer(yrs)

