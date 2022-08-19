#import packages
library(fpp2)
library(seasonal)

myfile <- melsyd

tsmd <- ts(melsyd[,3],start=c(1987,1),frequency = 52)

beerts <- ts(myfile,start=c(1987,1),frequency = 12)

beer <- window(WWWusage, start=1992)
fc <- snaive(beer)
autoplot(fc)
res <- residuals(fc)
autoplot(res)

checkresiduals(fc)

www <- window(bricksq)

fc <- snaive(www)
autoplot(fc)
res <- residuals(fc)
autoplot(res)

checkresiduals(fc)

autoplot(hsales) +xlab('Time') + ylab('Sales') + ggtitle('Sales of one-family houses')
ggsubseriesplot(hsales)
ggAcf(hsales)
help(hsales)

library(seasonal)
fit <- seas(elecequip, x11="")
autoplot(fit)
