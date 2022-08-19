library(readr)
library(fpp2)
library(dplyr)
library(mctest)
library(car)


cars_review <- read_csv("D:/Data/forecasting_311/cars_review.csv", 
                        col_types = cols(horsepower = col_double()))

#drop non numberic columns
cars2 <- select(cars_review,-c('car name'))

View(cars2)
#graph non timeseries
scatter.smooth(x=cars2$mpg, y=cars2$cylinders, main="mpg ~ cyl")

scatter.smooth(x=cars2$mpg, y=cars2$horsepower, main="mpg ~ horse")

scatter.smooth(x=cars2$mpg, y=cars2$weight, main="mpg ~ weight")


#run correlation chart
GGally::ggpairs(as.data.frame(cars2))

#create serial correlation
 


# build linear regression model on full data
linearMod <- lm(mpg ~ horsepower, data=cars2) 
summary(linearMod)

linearMod2 <- lm(mpg ~ horsepower + weight, data=cars2) 
summary(linearMod2)

linearMod3 <- lm(mpg ~ horsepower + weight + cylinders, data=cars2) 
summary(linearMod3)

linearMod4 <- lm(mpg ~ cylinders, data=cars2) 
summary(linearMod4)

linearMod5 <- lm(mpg ~ weight, data=cars2) 
summary(linearMod5)

plot(linearMod5)


X<-as.matrix(cars2[,2:3])
mpg2 <-as.matrix(cars2[,1])
omcdiag(X,mpg)

x<-cars2[,-1]
y<-cars2[,1]

## all overall diagnostic measures and eigenvalues with intercept
imc <- imcdiag(x,y)
imc

vif(linearMod3)



