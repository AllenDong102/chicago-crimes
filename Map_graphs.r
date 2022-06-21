library(knitr)
library(tidyverse)
library(forcats)
library(gridExtra)
library(RSQLite)
library(jsonlite)
library(gtable)
library(grid)
library(latex2exp)
library(gridBase)
library(nnet)
library(magrittr)
library(ggplot2)
library(data.table)
library(ggmap)

setwd("/Users/allendong/Desktop/")
data1 <- fread("/Users/allendong/Desktop/chicagocrimes.csv")

# subset every seventh row
sub <- data1[which(1:nrow(data1) %% 7 == 0),]

ggmap::register_google(key = "API KEY")

LatLonCounts <- as.data.frame(table(round(data1$Longitude,2), round(data1$Latitude,2)))
LatLonCounts$Long <- as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat <- as.numeric(as.character(LatLonCounts$Var2))
LatLonCounts2 <- subset(LatLonCounts, Freq > 0)
p <- ggmap(get_googlemap(center = c(lon = -87.6298, lat = 41.8781), zoom = 11, scale = 2, maptype ='terrain', color = 'color', alpha = 0.5)

p + geom_tile(data = LatLonCounts2, aes(x = Long, y = Lat, alpha = Freq), fill = "red")
p + geom_point(aes(x = Longitude, y = Latitude, col = Arrest), data = sub, alpha = 0.01, size = 3)
