#set you code up right for repeatable forecasting
#First: import libraries at the top
library(fpp2)
library(urca)
library(seasonal)
library(readr)

# get your dataset in and transformed

uheps <- read_csv("R/United Health EPS.csv", col_types = cols(EPS = col_number()))

newdata <- uheps[order(uheps$Date),]

View(newdata)

uhts <- ts(newdata[,2],start=c(2005,1), frequency = 4)

#plots
autoplot(uhts) + ggtitle("United Health EPS") + xlab("Date") + ylab("EPS")

ggAcf(uhts) + ggtitle("United Health EPS")

ggsubseriesplot(uhts) + ggtitle("United Health EPS")


#Now we need to introduct Training and Test samples
start(uhts)
end(uhts)
train <- window(uhts,end=c(2017,2))
test <-  window(uhts,start=c(2017,3))

#benchmarks
#run model on Training data set
model_m <- meanf(train, h=16)
model_n <- rwf(train, h=16)
model_d <- rwf(train, drift=TRUE, h=16)
model_s <- snaive(train,h=16)
autoplot(window(uhts,start=1)) +
  autolayer(model_m, PI=FALSE, series="Mean") +
  autolayer(model_n, PI=FALSE, series="Naïve") +
  autolayer(model_d, PI=FALSE, series="Drift") +
  autolayer(model_s, PI=FALSE, series='Seasonal Naive')+
  xlab("Days") + ylab("UHG EPS") +
  ggtitle("UHG EPS benchmark forecast") +
  guides(colour=guide_legend(title="Forecast"))

#use visuals to pick benchmark of mean

#decompose
autoplot(decompose(train,type='additive'))
model_stl <- stlf(train,method='naive',h=16)

#now let do ets
model_e <- ets(train)
summary(model_e)

fite <-forecast(model_e,h=16)

autoplot(window(uhts,start=1)) +
  autolayer(model_m, PI=FALSE, series="Mean") +
  autolayer(model_stl, PI=FALSE, series='Decomp')+
  autolayer(fite, PI=FALSE, series='ETS')+
  xlab("Days") + ylab("UHG EPS") +
  ggtitle("UHG EPS benchmark forecast") +
  guides(colour=guide_legend(title="Forecast"))

#arima do it
ndiffs(train)
summary(ur.kpss(train))

model_a <- auto.arima(train, stepwise = FALSE)
summary(model_a)
fita <- forecast(model_a, h=16)

autoplot(window(uhts)) +
  autolayer(model_m, PI=FALSE, series="Mean") +
  autolayer(fite, PI=FALSE, series='ETS')+
  autolayer(fita, PI=FALSE, series='ARIMA')+
  xlab("Days") + ylab("UHG EPS") +
  ggtitle("UHG EPS benchmark forecast") +
  guides(colour=guide_legend(title="Forecast"))

#final accuracy checks
accuracy(model_m,test)
accuracy(model_stl,test)
accuracy(fite,test)
accuracy(fita,test)

#choose ets model
autoplot(window(uhts)) +
  autolayer(fite, series='ETS')+
  xlab("Days") + ylab("UHG EPS") +
  ggtitle("UHG EPS benchmark forecast") +
  guides(colour=guide_legend(title="Forecast"))

fite

library(tsbox)


