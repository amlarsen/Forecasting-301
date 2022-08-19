library(fpp2)
library(urca)

#same deal plot it out
autoplot(visitors)
autoplot(log(visitors))        

#acf drop?
ggAcf(visitors)

#we need something lets look at transformation

#box cox?
BoxCox.lambda(visitors)
trans <- BoxCox(visitors,lambda = .277)
autoplot(trans)

#logs?
vislog <- log(visitors)
autoplot(vislog)

#double check the series for stationarity
summary(ur.kpss(visitors))
summary(ur.kpss(trans))

#we need a differencing at least one
ndiffs(visitors)
dvis <- diff(visitors,lag=1)
autoplot(dvis)
ndiffs(dvis)
nsdiffs(dvis)

#lets check seasonal diffs
divs12 <- diff(visitors,lag=12)
autoplot(divs12)
ndiffs(divs12)
nsdiffs(divs12)

#let build a little model
