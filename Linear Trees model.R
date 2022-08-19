library(fpp2)
library(GGally)

## access the data from R's datasets package
data(trees)

## look at the first several rows of the data
head(trees) 

str(trees) ## look at the structure of the variables

View(trees)

#check Correlation
ggpairs(data=trees, columns=1:3, title="trees data")


#linear fit model using 1 variable
fit_1 <- lm(Volume ~ Girth, data = trees)

summary(fit_1)

#lets plot but remember can only use for bivarate
ggplot(data = trees, aes(x = Girth, y = Volume)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  ggtitle("Linear Model Fitted to Data")

#lets look at a prediction
predict(fit_1, data.frame(Girth = 18.2))

#model 2 add height also
fit_2 <- lm(Volume ~ Girth + Height, data = trees)
summary(fit_2)

#add extra features
fit_3 <- lm(Volume ~ Girth * Height, data = trees)
summary(fit_3)

predict(fit_3, data.frame(Girth = 18.2, Height = 72))
