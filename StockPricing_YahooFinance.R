# Yahoo Finance 

# Get quantmod

# install.packages("quantmod")
library(quantmod)

start <- as.Date("2018-08-01")
end <- as.Date("2018-10-01")

# Let's get Apple stock data; Apple's ticker symbol is AAPL. We use the quantmod function getSymbols, and pass a string as a first argument to identify the desired ticker symbol, pass 'yahoo' to src for Yahoo! Finance, and from and to specify date ranges
# The default behavior for getSymbols is to load data directly into the global environment, with the object being named after the loaded ticker symbol. This feature may become deprecated in the future, but we exploit it now.

getSymbols("AAPL", src = "yahoo", from = start, to = end)

# What is AAPL?
AAPL
View(AAPL)
class(AAPL)
head(AAPL)
tail(AAPL)
plot(AAPL[, "AAPL.Close"], main = "AAPL")
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")


# Let's get data for Microsoft (MSFT) and Google (GOOG) (actually, Google isheld by a holding company called Alphabet, Inc., which is the company traded on the exchange and uses the ticker symbol GOOG).
getSymbols(c("MSFT", "GOOG"), src = "yahoo", from = start, to = end)

# Create an xts object (xts is loaded with quantmod) that contains closing prices for AAPL, MSFT, and GOOG
stocks = as.xts(data.frame(AAPL = AAPL[, "AAPL.Close"], MSFT = MSFT[, "MSFT.Close"], GOOG = GOOG[, "GOOG.Close"]))
head(stocks)
tail(stocks)

# Create a plot showing all series as lines; must use as.zoo to use the zoo method for plot, which allows for multiple series to be plotted on same plot
plot(as.zoo(stocks), screens = 1, lty = 1:3, xlab = "Date", ylab = "Price")
legend("right", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.5)

plot(as.zoo(stocks[, c("AAPL.Close", "MSFT.Close")]), screens = 1, lty = 1:2,  xlab = "Date", ylab = "Price")
par(new = TRUE)
plot(as.zoo(stocks[, "GOOG.Close"]), screens = 1, lty = 3, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(4)
mtext("Price", side = 4, line = 3)
legend("topleft", c("AAPL (left)", "MSFT (left)", "GOOG"), lty = 1:3, cex = 0.5)

# Get pipe operator!
if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}
stock_return = apply(stocks, 1, function(x) {x / stocks[1,]}) %>%  t %>% as.xts

head(stock_return)

plot(as.zoo(stock_return), screens = 1, lty = 1:3, xlab = "Date", ylab = "Return")
legend("topleft", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.5)

stock_change = stocks %>% log %>% diff
head(stock_change)

plot(as.zoo(stock_change), screens = 1, lty = 1:3, xlab = "Date", ylab = "Log Difference")
legend("topleft", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.5)  


candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")
addSMA(n = 20)

