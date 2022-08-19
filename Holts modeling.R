library(fpp2)

livestock2 <- window(livestock, start=1970, end=2000)

autoplot(livestock)
#Holts modeling
fit1 <- ses(livestock2)
fit2 <- holt(livestock2)
fit3 <- holt(livestock2, damped = TRUE)


accuracy(fit1, livestock)
accuracy(fit2, livestock)
accuracy(fit3, livestock)


autoplot(livestock2) +
  autolayer(fit1, PI=FALSE,series='ses') +
  autolayer(fit2, PI=FALSE,series='holt') +
  autolayer(fit3, PI=FALSE,series='holt-damp')

###eggs
autoplot(eggs)

fit11 <- ses(eggs, h=100)
fit21 <- holt(eggs,h=100)
fit31 <- holt(eggs, damped = TRUE,h=100)

accuracy(fit11)
accuracy(fit21)
accuracy(fit31)

autoplot(eggs) +
  autolayer(fit11, PI=FALSE,series='ses') +
  autolayer(fit21, PI=FALSE,series='holt') +
  autolayer(fit31, PI=FALSE,series='holt-damp')

