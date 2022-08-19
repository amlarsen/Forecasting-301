library(fpp2)

help(ausgdp)
help(rnorm)
rnorm(36)

autoplot(ts(rnorm(36)))

m = c(ar, ma)
w = arima.sim(m, 120)
w = ts(w)
plot(w)
Box.test(w, type="Ljung-Box")

BoxCox.lambda(elec)

autoplot(gas)
ggAcf(gas)
BoxCox.lambda(gas)

fit<- snaive(gas, lambda = .08262296)
autoplot(fit)

autoplot(arrivals)
View(arrivals)
ggseasonplot(arrivals[,4])
ggseasonplot(arrivals, col=rainbow(12), year.labels=TRUE)

ausbeer
beer <- window(ausbeer,start=1992)
ggseasonplot(a10, polar=TRUE)
ggsubseriesplot(a10)
colnames(arrivals)
autoplot(window(elec, start=1980))
beer <- window(ausbeer, start=1992)
gglagplot(beer)
autoplot(beer)

e <- tsCV(goog200, rwf, drift=TRUE, h=1)
sqrt(mean(e^2, na.rm=TRUE))

sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE))

goog200 %>% tsCV(rwf, drift=TRUE, h=1) -> e
e^2 %>% mean(na.rm=TRUE) %>% sqrt()

goog200 %>% rwf(drift=TRUE) %>% residuals() -> res
res^2 %>% mean(na.rm=TRUE) %>% sqrt()

autoplot(residuals(meanf(hsales))) + ggtitle("Time Plot of Residuals 1") +ylab('residuals')
ggAcf(residuals(meanf(hsales))) + ggtitle("ACF Plot of Residuals 1") +ylab('residuals')

Box.test(residuals(meanf(hsales)),type ="Ljung-Box")
Box.test(residuals(meanf(hsales)))

autoplot(aelec) + ylab("") + ggtitle("Time Plot 2")

BoxCox.lambda(aelec)
BoxCox.lambda(elecsales)
BoxCox.lambda(elec)

aelec <- window(elec, start=1980, end = 1995)
autoplot(aelec) + xlab("Year") + ylab("")

fitm <- meanf(hsales)

x <-tsCV(goog200, rwf, drift=TRUE,h=1)
accuracy(x)
sqrt(mean(e^2, na.rm=TRUE))
forecast(x)
x


beer <- window(ausbeer, start=1992)
fc <- snaive(beer)  
autoplot(fc)
checkresiduals(fc)

e <- tsCV(goog200, rwf, drift = TRUE , h=8)
sqrt(mean(e^2, na.rm=TRUE))
e <- tsCV(goog200, meanf,  h=8)
sqrt(mean(e^2, na.rm=TRUE))
e <- tsCV(goog200, rwf,  h=8)
sqrt(mean(e^2, na.rm=TRUE))

e <- tsCV(goog200, forecastfunction=naive, h=8)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()



stl(elecequip, s.window=7)
stl(elecequip)
