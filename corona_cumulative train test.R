#import packages
library(fpp2)

# Read in data
library(readr)
corona_virus <- read_delim("C:/Data/Forecasting/corona_virus_3.csv", 
                           "|", escape_double = FALSE, trim_ws = TRUE)
View(corona_virus)

#Change data frame to time series
cts  <- ts(corona_virus[,2],start=1,frequency=7)

#Do your plots
autoplot(cts) + xlab('Days') +ylab("total corona virus cases") +
  ggtitle("Time plot of corona virus over time")

ggsubseriesplot(cts)

ggAcf(cts)

#Do you see outliers anywhere?  Clean them if necessary. Talk about pipes
tsoutliers(cts)

ctsc <- cts %>%  tsclean()

#Cleaned? replot, what happened?
autoplot(ctsc)

ggsubseriesplot(ctsc)

ggAcf(ctsc)

#Now we need to introduct Training and Test samples
train <- window(ctsc,start=1,end=6)
test <-  window(ctsc,start=6)

# select your data and forecast periods, better way to code here.
fper <- 8
dtst <- train

#run model on Training data set
model_m <- meanf(dtst, h=fper)
model_n <- rwf(dtst, h=fper)
model_d <- rwf(dtst, drift=TRUE, h=fper)
model_s <- snaive(dtst,h=fper)
autoplot(window(ctsc,start=1)) +
  autolayer(model_m, PI=FALSE, series="Mean") +
  autolayer(model_n, PI=FALSE, series="Naïve") +
  autolayer(model_d, PI=FALSE, series="Drift") +
  autolayer(model_s, PI=FALSE, series='Seasonal Naive')+
  xlab("Days") + ylab("Corona virus cases") +
  ggtitle("Corona virus forecast") +
  guides(colour=guide_legend(title="Forecast"))


#visually inspected. we can throw out the mean as it is way off.
#lets check the residuals for WN
checkresiduals(model_n)
checkresiduals(model_d)

#now lets compare all models and see what are best model is
accuracy(model_m,test)
accuracy(model_n,test)
accuracy(model_d,test)
accuracy(model_s,test)

#Let's do the cross validation to ensure accuracy
e <- tsCV(train, meanf, h=1)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(meanf(train))^2, na.rm=TRUE))

e2 <- tsCV(train, rwf, drift=TRUE, h=1)
sqrt(mean(e2^2, na.rm=TRUE))
sqrt(mean(residuals(rwf(train, drift=TRUE))^2, na.rm=TRUE))

#let's try our new models, remember must be odd for centric ma
ma(ctsc,5)

autoplot(window(ctsc,start=1,series="Data")) +
  autolayer(ma(ctsc,7), series="5-MA") +
  autolayer(model_stl, PI=FALSE, series="Naïve") +
  #autolayer(model_d, PI=FALSE, series="Drift") +
  #autolayer(model_s, PI=FALSE, series='Seasonal Naive')+
  xlab("Days") + ylab("Corona virus cases") +
  ggtitle("Corona virus forecast") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red","Naïve"='blue'),
                      breaks=c("Data","5-MA",'Naïve'))

model_stl <- mstl(train,h=8)
autoplot(model_stl)

model_stl <- stlf(train, method='naive',h=8)
fcast

