#import libraries
library(fpp2)
library(urca)
library(seasonal)
library(readr)

#import in our data here
uheps <- read_csv("R/United Health EPS.csv", col_types = cols(EPS = col_number()))

View(uheps)

#sort data ascending
newdata <- uheps[order(uheps$Order),]
View(newdata)

uhts <- ts(newdata[,2],start = c(2005,1),frequency = 4)

#plot out data series
autoplot(uhts) + ggtitle("United Health EPS") + xlab('Date') + ylab("EPS $")

ggAcf(uhts) + ggtitle("United Health EPS")

ggsubseriesplot(uhts) + ggtitle("United Health EPS")

#Test and Training breakout
start(uhts)
end(uhts)
train <- window(uhts, end=c(2017,2))
test <- window(uhts, start=c(2017,3))

#run benchmarks
model_m <- meanf(train, h=8)
model_n <- rwf(train,h=8)
model_d <- rwf(train,drift=TRUE,h=8)
model_s <- snaive(train, h=8)
autoplot(uhts)+
  autolayer(model_m, PI=FALSE, series='mean')+
  autolayer(model_n, PI=FALSE, series='naive')+
  autolayer(model_d, PI=FALSE, series='drift')+
  autolayer(model_s, PI=FALSE, series='season')+
  ggtitle("United Health EPS bench")+
  xlab('Date') + ylab("EPS $")

#decomp
autoplot(decompose(train), type='additive')
model_stl <- stlf(train, method='naive', h=8)

autoplot(uhts)+
  autolayer(model_stl, PI=FALSE, series='stl')

#lets try ETS
model_e <- ets(train)
summary(model_e)

fite <- forecast(model_e, h=8)
fite

autoplot(uhts)+
  autolayer(fite, PI=FALSE, series='ETS')

#arima try
model_a <- auto.arima(train, stepwise = FALSE)
summary(model_a)

fita <- forecast(model_a, h=8)
fita

#visual
autoplot(uhts)+
  autolayer(fita, PI=FALSE, series='arima')+
  autolayer(fite, PI=FALSE, series='ETS')+
  autolayer(model_stl, PI=FALSE, series='STL')+
  ggtitle("United Health EPS Advanced")+
  xlab('Date') + ylab("EPS $")
  
