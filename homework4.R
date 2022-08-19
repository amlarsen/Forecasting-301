library(fpp2)

autoplot(USAccDeaths) + ggtitle('Accidental deaths in the USA')

check <-  window(USAccDeaths,start=1972)

fcst1 <- holt(USAccDeaths,h=5)
accuracy(fcst1)

fcst2 <- hw(USAccDeaths, h=5)
accuracy(fcst2)

fit3 <- ets(USAccDeaths)
summary(fit3)

autoplot(fit3)
