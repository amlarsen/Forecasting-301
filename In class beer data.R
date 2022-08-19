#import packages
library(fpp2)
library(readr)
library(seasonal)

#import data
beer <- read_csv("beer.csv")
View(beer)

#change to time series
myts <- ts(beer[,2],start=c(1992,1),frequency = 12)

#plot the data
autoplot(myts) + ggtitle("Beer/wine sales us") + ylab("Sales in millions") + xlab("year")

ggsubseriesplot(myts)

ggAcf(myts)

#testing and training

mytrain <- window(myts,end=c(2018,12))
mytest <- window(myts,start=2019)

#let's run some MA's
fit3 <- ma(myts,3)
fit9 <- ma(myts,9)
fit15 <- ma(myts,15)

autoplot(myts) +
  autolayer(fit3) + ggtitle("Beer/wine sales us")

autoplot(fit3)
autoplot(fit9)
autoplot(fit15)

#decompositions
dcompm <- decompose(myts,type="multiplicative")
dcompa <- decompose(myts,type="additive")

autoplot(dcompm) + ggtitle('Classical Decomposition beer sales')

######################################################################
#Special economic models
#try x11
fit11 <- seas(mytrain,x11='')
autoplot(fit11) + ggtitle('x11')

#try seats
fitse <- seas(mytrain)
autoplot(fitse) + ggtitle('SEATS')

######################################################################
#STL using Loess
fitstl <- stlf(mytrain,method='naive',h=20)
autoplot(fitstl)

#test against a benchmark model
model_n <- rwf(mytrain,drift=TRUE,h=20)

#plot models vs one another
autoplot(mytrain) +
  autolayer(fitstl,PI=FALSE, series = 'STL') +
  autolayer(model_n,PI=FALSE, series = 'DRIFT')

#accuracy
accuracy(fitstl,mytest)
accuracy(model_n,mytest)

#write out forecasts
#write.csv(fitstl,'d:/Data/newbeerfcsts.csv')

###############################################
#simple exponential smoothing
fc <- ses(mytrain,h=20)
summary(fc)

autoplot(fc)+
  autolayer(fitted(fc), series = "Fittesds")

checkresiduals(fc)

######################################
fthwa <- hw(mytrain,seasonal = 'additive')
fthwm <- hw(mytrain, seasonal='multiplicative')

accuracy(fitstl,mytest)
accuracy(fthwa,mytest)
accuracy(fthwm,mytest)

autoplot(window(mytrain,start=2010),series='Data') +
  autolayer(fitstl,PI=FALSE, series = 'STL') +
  autolayer(fthwa,PI=FALSE, series = 'HW') +
  scale_colour_manual(values=c("Data"="grey50","STL"="black", 'HW'='green' ),
                      breaks=c("Data","STL","HW"))
  




