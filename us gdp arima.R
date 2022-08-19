#import packages
library(fpp2)
library(urca)

autoplot(usgdp)

#get stationary
ndiffs(usgdp)

dfgdp <- diff(usgdp,differences = 2)

autoplot(dfgdp)

summary(ur.kpss(dfgdp))

#Pacf chart
ggtsdisplay(dfgdp)

#auto arima
fitauto <- auto.arima(usgdp)
summary(fitauto)

fitauto2 <- auto.arima(dfgdp)
summary(fitauto2)

forecast(fitauto2,h=5)
