#import packages
library(fpp2)
library(urca)
autoplot(gas)

fit1 <- hw(gas,seasonal='multiplicative',damped=FALSE)
fit2 <- hw(gas,seasonal='multiplicative',damped=TRUE)

autoplot(gas) +
  autolayer(fit1, PI=FALSE, series = "HW non damped") +
  autolayer(fit2, PI=FALSE, series = "HW damped")

autoplot(gas) +
  autolayer(fit2, PI=FALSE, series = "HW damped")

accuracy(fit1)
accuracy(fit2)

checkresiduals(fit1)

fit3 <- ets(gas)
summary(fit3)

fit4 <- forecast(gas)
summary(fit4)

autoplot(gas) +
  autolayer(fit4, PI=FALSE, series = "ETS")

newgas <- window(gas,start=1985)
autoplot(newgas)

fit5 <- forecast(newgas)
summary(fit5)

autoplot(newgas) +
  autolayer(fit5, PI=FALSE, series = "ETS")

ggAcf()
