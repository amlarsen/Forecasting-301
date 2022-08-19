#import packages
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

