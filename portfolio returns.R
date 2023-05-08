library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(imputeTS)
library(PortfolioAnalytics)

tickers <- c("AAPL", "MSFT", "AMZN","IBM","INTC","GOOG", "ORCL")
weights <- c(0.59603152, 0.01062591, 0.001187965 , 0.009006962,0.0002122074,0.01454383,0.3683916)

#Get Prices (can be monthly or weekly)
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2020-04-19", periodicity = "daily", auto.assign=FALSE)[,4])

benchmarkPrices <- getSymbols.yahoo("^IXIC", from="2020-04-19", periodicity = "daily", auto.assign=FALSE)[,4]

benchmarkReturns <- na.omit(ROC(benchmarkPrices))

PortfolioReturns <- na.omit(ROC(portfolioPrices))

plot(portfolioPrices, legend = tickers)

dailyReturns <- na.omit(ROC(portfolioPrices, type="discrete"))

portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)

plot(portfolioReturn, legend = tickers)

chart.CumReturns(portfolioReturn)

charts.PerformanceSummary(portfolioReturn)

SharpeRatio(portfolioReturn, Rf = .0344/252, p = 0.95, FUN = "StdDev",
            weights = NULL, annualize = FALSE)

table.AnnualizedReturns(portfolioReturn, Rf=.0344/252, geometric=TRUE)

CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, .035/252)
