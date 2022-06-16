# Referenced: 
# https://github.com/dkahle/ggmap
# https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present-Dashboard/5cd6-ry5g
# https://housely.com/the-10-most-dangerous-neighborhoods-in-chicago/

#install.packages("data.table")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("data.table")
#install.packages("ggrepel")
#install.packages("dplyr")
#install.packages("data.table")
#install.packages("tidyverse")
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)
library(data.table)

setwd("/Users/allendong/Desktop/")
data1 <- fread("/Users/allendong/Desktop/chicagocrimes.csv")
# only take complete data
data1 <- data1[complete.cases(data1), ]
sub <- data1[1:100,]

n <- fread("/Users/allendong/Desktop/dangerousneighborhoods.csv")
n <- subset(n, Rank < 6)

col1 = "#011f4b"
col2 = "#6497b1"
col3 = "#b3cde0"
col4 = "#CC0000"

#install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

library("ggmap")
ggmap::register_google(key = "insert API key here")

# heatmap
sub <- data1[1:100000]

n$Neighborhood <- factor(n$Location)
n$Label <- paste(n$Rank, n$Location, sep="-")

# graph 1
# heatmap which shows location of crimes on a plot
# labels denote ranked most dangerous neighborhoods. 
p <- ggmap(get_googlemap(center = c(lon = -87.6298, lat = 41.8781), zoom = 11, scale = 2, maptype ='terrain', color = 'color'))
p + geom_point(aes(x = Longitude, y = Latitude), color = "red", data = sub, alpha = 0.1, size = 0.2) + 
  theme(legend.position="bottom") + geom_point(aes(x = X, y = Y, shape=Neighborhood, stroke = 2), colour=col4, data = n, size =2) + geom_label_repel(
    aes(X, Y, label = Label),
    data=n,
    family = 'Times', 
    size = 2, 
    box.padding = 0.2, point.padding = 0.3,
    segment.color = 'grey50') 

# stat density 2d plot (in progress), not one 
#p + geom_point(aes(x = Longitude, y = Latitude,  colour = col1), data = sub, alpha = 0.25, size = 0.5) + stat_density2d(
 # aes(x = Longitude, y = Latitude, fill = ..level.., alpha = 0.05),
  #size = 0.1, bins = 40, data = sub,
  #geom = "polygon"
#)+ theme(legend.position="bottom") + geom_point(aes(x = X, y = Y, shape=Neighborhood, stroke = 2), colour=col4, data = n, size =3) + geom_label_repel(
 #   aes(X, Y, label = Label),
  #  data=n,
   # family = 'Times', 
  #  size = 4, 
   # box.padding = 0.2, point.padding = 0.3,
    #segment.color = 'grey50') 
