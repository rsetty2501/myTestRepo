require(forecast)

args <- commandArgs(trailingOnly = TRUE)
# The days to forecast is passed as an argument from the Apache spark application
daystoforecast <- as.integer(args[1])

# Perform the forecast on multiple time series and try to save it as a csv file with the forecasted data

stockData = read.csv('~/output/stockData.csv')
stockData <- transform(stockData,newpriced = stockData$priced/1000000 )
# dataseries.ts <- ts(ordata[,4],start=c(2017,9), frequency = 1)
# count the number of unique stocks
split_stockData <- split(stockData,stockData$obShortNameNew)
uniqueStocks <- sort(unique(stockData$obShortNameNew))

# For making Date as x-axis for the stock forecast plot
firstDate <- lapply(split_stockData, function(x) head(sort(x$Date)))
firstDate <- lapply(firstDate, function(x) x[1])
dateLength <- lapply(split_stockData, function(x) length(sort(x$Date)))

# Date sequence function
seqDates <- function(fdate, Datelength){
  seq(as.Date(toString(fdate), format = "%Y-%m-%d"),
      by = "days", length = Datelength + daystoforecast)
}

dates <- mapply(seqDates, firstDate,dateLength)
dates <- lapply(dates, function(x) format(as.Date(x, format = "%Y-%m-%d"), " %b%d"))

# =====================================================================================================
# Working data which is not splitted into train and test
#ArimaModel_NoSplit <- lapply(split_stockData, function(x) auto.arima(x$newpriced))
ArimaModel_NoSplit <- lapply(split_stockData, function(x) auto.arima(x$newpriced,approximation = FALSE, trace = FALSE))

summary_ar <- lapply(ArimaModel_NoSplit, function(x) summary(x))

ForecastArima_NoSplit <- lapply(ArimaModel_NoSplit, function(x) forecast(x,h = daystoforecast))

stockplot <- function(x,y,d,lend){
  plot(x, main = paste("",x[1]), xlab = paste("",y), ylab = "stockPrice", type = "l", xaxt = "n")
  axis(side = 1, at = seq(1,lend + daystoforecast), labels = d)
}

pdf('~/output/stockForecast_NonSplit.pdf')
#pdf(paste(getwd(),'/stockForecast_NonSplit.pdf',sep = ""))
par(mfrow = c(2, 2))
graph <- mapply(stockplot, ForecastArima_NoSplit, uniqueStocks, dates,dateLength)
dev.off()

# =====================================================================================================
# Splitting the data into train and test and check the model

train <- lapply(split_stockData, function(x) head(x,floor(.75*nrow(x))))
# train
test <- lapply(split_stockData, function(x) tail(x,floor(.25*nrow(x))))
# test

#ArimaModel <- lapply(train, function(x) auto.arima(x$newpriced))
ArimaModel <- lapply(train, function(x) auto.arima(x$newpriced,approximation = FALSE, trace = FALSE))

ForecastArima <- lapply(ArimaModel, function(x) forecast(x,h = daystoforecast + 5))

accFunc <- function(x,y){
  accuracy(x,y$newpriced)
}
#accTest <- accuracy(res1$AAPLUSD,test$AAPLUSD$newpriced)
#accTest
#acc <- mapply(accFunc, res1,test)
#acc
# plot(res1$GOOG)
# lines(split_ts$GOOG$priced,col = "red")

stockplot_new <- function(x,y,z,d,lend){
  plot(x, main = paste("",x[1]), xlab = paste("",y), ylab = "stockPrice", xaxt = "n")
  # plot(x, main = paste("",x[1]), xlab = paste("",y), ylab = "stockPrice", xaxt = "n", ylim = c(min(z$newpriced),max(z$newpriced)))
  lines(z$newpriced,col = "red")
  axis(side = 1, at = seq(1,lend+daystoforecast), labels = d)
  # Need to put proper legends in the graph
  legend(lend+6,max(z$newpriced) + 0.6,legend = c("Data","Forecast"), col = c("red","blue"),  lty=1:1, cex=0.8)
}

pdf('~/output/stockForecast.pdf')
#pdf(paste(getwd(),'/stockForecast.pdf',sep = ""))
par(mfrow = c(1, 1))
graph <- mapply(stockplot_new, ForecastArima, uniqueStocks,split_stockData,dates,dateLength)
dev.off()


