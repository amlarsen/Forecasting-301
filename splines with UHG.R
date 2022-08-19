library(fpp2)
library(readr)
library(splines)

uheps <- read_csv("R/United Health EPS.csv", col_types = cols(EPS = col_number()))
View(uheps)

fit_1 <- lm(EPS ~ Order, data=uheps)
summary(fit_1)

predict(fit_1, data.frame(Order=59))

#splines
uhts <- ts(uheps[,2],start = c(2005,1),frequency = 4)
autoplot(uhts)
fc<-splinef(uhts)
summary(fc)
autoplot(fc)
