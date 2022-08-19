library(fpp2)
library(urca)

#same deal plot data
autoplot(visitors)

#ACF drop
ggAcf(visitors)

#we need variance reduction
BoxCox.lambda(visitors)
tran1 <- BoxCox(visitors,lambda = .277)
autoplot(tran1)

# logs
tran2 <- log(visitors)
autoplot(tran2)

#check stationarity
summary(ur.kpss(visitors))
summary(ur.kpss(tran1))

#how many differences
ndiffs(visitors)
nsdiffs(visitors)
ndiffs(tran1)
nsdiffs(tran1)

#take seasonal diff 1st
visds <- diff(tran1,lag=12)
autoplot(visds)        

ndiffs(visds)       
summary(ur.kpss(visds))        

visds2 <- diff(visds)        
autoplot(visds2)        
summary(ur.kpss(visds2))        

ndiffs(visds2)

#helpful hints
is.ts(visds2)
start(visds2)
end(visds2)

#arima
ggtsdisplay(visds2)

ar1 <- arima(visds2,order=c(1,0,0))
summary(ar1)

ar2 <- arima(visds2,order = c(2,0,0))
summary(ar2)

#ma part
ma1 <- arima(visds2,order=c(0,0,1))
summary(ma1)

ma2 <- arima(visds2, order = c(0,0,2))
summary(ma2)

#combine techniques
arma1 <- arima(visds2, order = c(1,0,1))
summary(arma1)

arma2 <- arima(visds2, order = c(1,0,2))
summary(arma2)

arma3 <- arima(visds2, order = c(2,0,1))
summary(arma3)

arma4 <- arima(visds2, order = c(2,0,2))
summary(arma4)

#check winner
checkresiduals(arma3)

autodiff <- auto.arima(visds2, stepwise = FALSE)
summary(autodiff)

autodiffttl <- auto.arima(visitors, stepwise = FALSE)
summary(autodiffttl)

accuracy(autodiffttl)

forecast(autodiffttl,h=12)
autoplot(forecast(autodiffttl,h=12))

nsdiffs(usgdp)
diff <- diff(enplanements, lag =12)
ndiffs(enplanements)
ndiffs(usgdp)
autoplot(enplanements)

auto.arima(uspop)
ndiffs(uspop)
summary(ur.kpss(diff(diff(uspop))))

autoplot(uspop)

ggtsdisplay(ibmclose)
