library(fpp2)
library(readr)
library(seasonal)

#import data
beer <- read_csv("D:/Data/forecasting_311/beer.csv")
View(beer)

#change to time series
myts <- ts(beer[,2],start=c(1992,1),frequency = 12)

#plot the data
autoplot(myts) + ylab('$ sales in Milliions') + xlab("year") + ggtitle("Fred: Beer, wine, and alcohol sales usa")

ggsubseriesplot(myts)

ggAcf(myts)

#break into test and train samples

mytrain <- window(myts, end = c(2018,12))
mytest <- window(myts, start=2019)

#lets run some ma's
fit5 <- ma(myts, 3)
fit24 <- ma(myts, 9)

autoplot(fit5) +
  autolayer(fit24)

autoplot(myts) +
  autolayer(fit5, series="MA-3") +
  ggtitle("beer")

autoplot(myts) +
  autolayer(fit24, series='MA-5')+
  ggtitle("beer")

# start with simple decomposition
dcompm<- decompose(myts,type="multiplicative")
dcompa<- decompose(myts,type='additive')


autoplot(dcompa) + xlab("Year") +
  ggtitle("Classical additive decomposition
    beer")

#############################################################################################
#these special for economic data only
# try x11
library(seasonal)
fit <-  seas(mytrain,x11="")
autoplot(fit) +
  ggtitle("X11 decomposition of us beer wine alcohol sales")

# try Seats
fit2 <- seas(mytrain)  

autoplot(fit2) +
  ggtitle("SEATS decomposition of us beer wine alcohol sales")

ggsubseriesplot(seasonal(fit2)) + ylab("Seasonal") +
  ggtitle("SEATS decomposition of us beer wine alcohol sales")
############################################################################################


#try using stlf  Seasonal and Trend decomposition using Loess
model_stl <- stlf(mytrain,method='naive',h=20)

autoplot(model_stl) +
  ggtitle("stl decomposition of us beer wine alcohol sales")

# now lets if using the forecast funciton
model_stl <- stlf(mytrain,method='naive',h=20)
model_n <- rwf(mytrain, drift=TRUE, h=20)

#Plot again
autoplot(window(mytrain)) +
  autolayer(model_stl, PI=FALSE, series="STL") +
  autolayer(model_n, PI=FALSE, series="Naive") +
  xlab("Time") + ylab("Sales in millions") +
  ggtitle("Beer Alcohol Wine forecast") +
  guides(colour=guide_legend(title="Forecast"))

#accuracy
accuracy(model_stl,mytest)
accuracy(model_n,mytest)

#lets write out our forecasts
write.csv(model_stl,'d:/Data/my_forecasts.csv')


