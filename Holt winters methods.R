library(fpp2)


#Holts winters
aust <- window(austourists,start=2005)  
fit1 <- hw(aust,seasonal="additive")
fit2 <-hw(aust,seasonal="multiplicative")

autoplot(aust)  +
  autolayer(fit1,PI=FALSE, series='hw add') +
  autolayer(fit2,PI = FALSE ,series='hw mult')




