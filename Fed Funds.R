library(fpp2)

fed <- read.csv('d:/Data/forecasting_311/FEDFUNDS.csv')

fed_ts <- ts(fed[,2],start=c(2008,12),frequency=12)

autoplot(fed_ts) + ggtitle("My Fed funds graph") + xlab("Date") + ylab("fed %")

naive(fed_ts,h=10)
rwf(fed_ts,h=10,drift = TRUE)

models <- c(naive,rwf)

for (i in models){
  m <- i(fed_ts,h=10)
  print(m)
  
}
models

ecdf(fed_ts)
hist(fed_ts)
