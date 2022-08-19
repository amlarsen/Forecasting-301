#import packages
library(fpp2)
library(readr)
library(splines)

#import data
uheps <- read_csv("R/United Health EPS.csv", col_types = cols(EPS = col_number()))
View(uheps)

#splines
uhts <- ts(uheps[,2],start = c(2005,1),frequency = 4)
autoplot(uhts)
fc<-splinef(uhts)
summary(fc)
autoplot(fc)
