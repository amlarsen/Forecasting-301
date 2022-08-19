library(fpp2)
library(tidyverse)
library(dplyr)
library(data.table)
library(lubridate)

airlines <- read.csv('d:/Data/forecasting_311/flights.csv')

sapply(airlines, class)

airlines2 <- filter(airlines2, ORIGIN_AIRPORT=='MSP',DESTINATION_AIRPORT=='LAX')

rm(airlines)

newx <-summarise_at(group_by(airlines2,YEAR,MONTH,DAY),vars(DEPARTURE_DELAY),funs(mean(.,na.rm = TRUE)))

newx$date <- with(newx, ymd(paste(YEAR, MONTH, DAY, sep= ' ')))

newx <- newx[c("date",'DEPARTURE_DELAY')]  

delayts <- ts(newx[,2],frequency = 7)

ggseasonplot(delayts)
ggsubseriesplot(delayts)
ggAcf(delayts)

ggplot(newx, aes(x=DEPARTURE_DELAY)) + geom_histogram(binwidth=1)
ggplot(newx, aes(x=DEPARTURE_DELAY)) + geom_histogram(binwidth=.25)


library(forecast)
fit <- ets(delayts)
fc <- forecast(fit)
plot(fc)


