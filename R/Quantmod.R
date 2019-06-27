####################################
########## Initialization ##########
####################################

rm(list=ls())

###############################
########## Libraries ##########
###############################

library(quantmod)
library(zoo)
library(xts)
library(moments) 
library(ggplot2)
library(matlib)
library(markdown)
library(knitr)
library(Rsolnp)

###############################
########## Functions ##########
###############################

ReadPortfolio <- function(file_address) {
  # Reads portfolio data from file
  # Returns a list containing many things!
  
  portfolio_data = read.csv(file=file_address, header=TRUE, sep=",")
  
  # tickers
  tickers = colnames(portfolio_data)
  tickers <- tickers[3:length(tickers)]
  for (i in tickers) {
    if(i == 'X.GSPC')
      tickers[tickers == 'X.GSPC'] = "^GSPC"
  }
  
  # dates
  dates = as.Date(levels(portfolio_data[, 1]), "%m/%d/%Y")
  dates = dates[order(dates)]
  
  # xts
  port_tmp <- portfolio_data
  port_tmp$Date <- NULL
  portfolio_xts <- xts(x = port_tmp, order.by = dates)
  
  # adjustment for closed trading days
  fromdate <- dates[1]
  todate <- dates[length(dates)]
  GSPC = getSymbols(
    "^GSPC",
    from = as.character(fromdate),
    to = as.character(todate+1),
    auto.assign = FALSE)
  GSPC_Adj_Close = GSPC[, "GSPC.Adjusted"]
  dates = index(GSPC_Adj_Close)
  portfolio_xts = portfolio_xts[dates]
  
  portfolio <- list("data" = portfolio_data, "xts" = portfolio_xts, "tickers" = tickers, "dates" = dates)
  
  return(portfolio)
}

GetInterestRates <- function(dates) {
  # Returns daily interest rate returns as an array
  
  fromdate <- dates[1]
  todate <- dates[length(dates)]
  
  IRX = getSymbols(
    "^IRX",
    from = as.character(fromdate),
    to = as.character(todate+1),
    auto.assign = FALSE)
  IRX_Adj_Close = data.frame(IRX)$IRX.Adjusted
  IRX_Annual_Returns = IRX_Adj_Close/100
  IRX_Daily_Return = (IRX_Annual_Returns/252)[2:length(IRX_Adj_Close)]
  
  return(IRX_Daily_Return)
  
}

GetPortfolioPrices <- function(portfolio) {
  # Returns portfolio prices as an xts object
  
  fromdate <- portfolio$dates[1]
  todate <- portfolio$dates[length(portfolio$dates)]
  
  adjusted_close_all = data.frame(matrix(NA, nrow = length(portfolio$dates), 0))
  
  for (i in 1 : length(portfolio$tickers)) {
    current_row = portfolio$data[, i+1]
    ticker = portfolio$tickers[i]
    
    symbols = getSymbols(
      ticker,
      from = as.character(fromdate),
      to = as.character(todate+1),
      auto.assign = FALSE)
    
    symbols_Adj_Close = symbols[, paste(ticker, ".Adjusted", sep="")]
    adjusted_close_all <- cbind(adjusted_close_all, symbols_Adj_Close)
  }
  
  return(adjusted_close_all)
}

PortfolioValue <- function (portfolio) {
  # Returns portfolio value as an xts object
  
  adjusted_close_all = GetPortfolioPrices(portfolio)
  portVals = portfolio$xts[, 2:dim(portfolio$xts)[2]] * adjusted_close_all
  portVals$Cash = portfolio$xts$Cash
  portVals = rowSums(portVals)
  portVals = portVals[!is.na(portVals)]
  portVals = xts(x = portVals, order.by = portfolio$dates)
  names(portVals) <- "Value"
  
  return(portVals)
}

CalculateDailyReturns <- function(portfolio) {
  # Calculates daily returns as an array
  
  portValue = PortfolioValue(portfolio)
  portValue = data.frame(portValue)$Value
  DailyReturns = Delt(portValue, type = 'log')[2:length(portValue)]
  
  return(DailyReturns)
}

AnnualizedVolatility <- function(portfolio) {
  # Calculates annualized volatility of daily returns
  
  dailyReturns = CalculateDailyReturns(portfolio)
  stdDev = sd(dailyReturns)
  AnnualizationFactor = sqrt(252)
  annualizedVolatility = stdDev * AnnualizationFactor
  return(annualizedVolatility)
}

AnnualizedVolatility_SP500 <- function(dailyReturns) {
  # Calculates annualized volatility of daily returns for the S&P 500
  
  stdDev = sd(dailyReturns)
  AnnualizationFactor = sqrt(252)
  annualizedVolatility = stdDev * AnnualizationFactor
  return(annualizedVolatility)
}

ExcessReturns <- function(portfolio) {
  # Returns excess daily returns
  
  AssetReturns = CalculateDailyReturns(portfolio)
  IRReturns = GetInterestRates(portfolio$dates)
  
  Excess_Return = AssetReturns - IRReturns
  return(Excess_Return)
}


PlotReturns <- function(Returns) {
  qplot(Returns, geom = 'histogram', binwidth = .1) + xlab('Portfolio Returns Distribution (%)') +
    # Create normal curve, adjusting for number of observations and binwidth
    stat_function( 
      fun = function(x, mean, sd, n, bw){ 
        dnorm(x = x, mean = mean, sd = sd) * n * bw
      }, 
      args = c(mean = mean(Returns), sd = sd(Returns), n = length(Returns), bw = 0.1))
}

SharpeRatio <- function(portfolio, compare=TRUE, print=TRUE) {
  # Calculates annualized daily Sharpe ratio
  
  AssetReturns = CalculateDailyReturns(portfolio)
  IRReturns = GetInterestRates(portfolio$dates)
  
  Excess_Return = AssetReturns - IRReturns
  Sharpe_Ratio = mean(Excess_Return, na.rm = TRUE) / sd(AssetReturns, na.rm = TRUE)
  AnnualizationFactor = sqrt(252)
  Sharpe_Ratio = Sharpe_Ratio * AnnualizationFactor
  
  if (print == TRUE) {
    print("Annualized Daily Sharpe Ratio: ")
    print(Sharpe_Ratio)
  }
  
  if (compare == TRUE) {
    fromdate <- portfolio$dates[1]
    todate <- portfolio$dates[length(portfolio$dates)]
    
    SP500 = getSymbols(
      "^GSPC",
      from = as.character(fromdate),
      to = as.character(todate+1),
      auto.assign = FALSE)
    SP500_Adj_Close = SP500[, "GSPC.Adjusted"]
    SP500_Return = Delt(SP500_Adj_Close, type = 'log')[2:length(SP500_Adj_Close)]
    Excess_Return_SP500 = SP500_Return - IRReturns
    Sharpe_Ratio_SP500 = mean(Excess_Return_SP500, na.rm = TRUE) / sd(SP500_Return, na.rm = TRUE)
    print("SP500 mean:")
    print(mean(Excess_Return_SP500, na.rm = TRUE))
    print("SP500 sd:")
    print(sd(SP500_Return, na.rm = TRUE))
    print("Annualized Market Sharpe Ratio: ")
    print(Sharpe_Ratio_SP500 * AnnualizationFactor)
  }
  
  return(Sharpe_Ratio)
}

SharpeRatioSP500 <- function(AssetReturns, dates, print=TRUE) {
  IRReturns = GetInterestRates(dates)
  Excess_Return = AssetReturns - IRReturns
  Sharpe_Ratio = mean(Excess_Return, na.rm = TRUE) / sd(Excess_Return, na.rm = TRUE)
  AnnualizationFactor = sqrt(252)
  Sharpe_Ratio = Sharpe_Ratio * AnnualizationFactor
  
  if (print == TRUE) {
    print("Annualized Daily Sharpe Ratio: ")
    print(Sharpe_Ratio)
  }
  
  return(Sharpe_Ratio)
}

TotalReturn <- function(portfolio, print=TRUE) {
  # Calculates total return on portfolio
  
  portVals = PortfolioValue(portfolio)
  TotalRet = (coredata(portVals[length(portVals)]) - coredata(portVals[1])) / coredata(portVals[1])
  
  if (print == TRUE) {
    print("Total Return (percent): ")
    print(TotalRet * 100)
  }
  
  return(TotalRet)
}

CalculateBeta <- function(portfolio, print=TRUE) {
  # Calculates portfolio beta with respect to the S&P 500
  
  fromdate <- portfolio$dates[1]
  todate <- portfolio$dates[length(portfolio$dates)]
  
  SP500 = getSymbols(
    "^GSPC",
    from = as.character(fromdate),
    to = as.character(todate+1),
    auto.assign = FALSE)
  SP500_Adj_Close = SP500[, "GSPC.Adjusted"]
  SP500_Return = Delt(SP500_Adj_Close, type = 'log')[2:length(SP500_Adj_Close)]
  
  port_returns = CalculateDailyReturns(portfolio)
  
  beta = cov(port_returns, SP500_Return) / var(SP500_Return)
  
  if (print == TRUE) {
    print("Portfolio Beta: ")
    print(beta)
  }
  
  return(beta)
}

EfficientFrontier <- function(portfolio, n_years=3) {
  EPSILON = 1e-4
  tickers = portfolio$tickers
  cur_pos <- tail(portfolio$xts, 1)
  cur_pos$Cash <- NULL
  tickers = tickers[(cur_pos > EPSILON)]
  print(tickers)
  enddate <- portfolio$dates[length(portfolio$dates)]
  startdate <- enddate - n_years * 365
  
  GSPC = getSymbols(
    "^GSPC",
    from = as.character(startdate),
    to = as.character(enddate+1),
    auto.assign = FALSE)
  GSPC_Adj_Close = GSPC[, "GSPC.Adjusted"]
  dates = index(GSPC_Adj_Close)
  
  adjusted_close_all = data.frame(matrix(NA, nrow=length(dates)-1, 0))
  cur_price = data.frame(matrix(NA, nrow=1, ncol=length(tickers)))
  for (i in 1 : length(tickers)) {
    ticker = tickers[i]
    
    symbols = getSymbols(
      ticker,
      from = as.character(startdate),
      to = as.character(enddate+1),
      auto.assign = FALSE)
    
    symbols_Adj_Close = symbols[, paste(ticker, ".Adjusted", sep="")]
    cur_price[i] <- tail(symbols_Adj_Close, 1)
    names(cur_price)[i] <- ticker
    symbols_returns = Delt(symbols_Adj_Close, type='log')[2:length(symbols_Adj_Close)]
    names(symbols_returns) <- paste(ticker)
    adjusted_close_all <- cbind(adjusted_close_all, symbols_returns)
  }
  
  # Check for NAs
  if (sum(is.na(adjusted_close_all)) > 0) {
    print('Missing Data Error: results are not reliable')
  }
  
  IRReturns = GetInterestRates(dates)
  RF = mean(IRReturns, na.rm=TRUE)
  # adjusted_close_all = sweep(adjusted_close_all, 1, IRReturns)
  
  adjusted_close_all = adjusted_close_all[complete.cases(adjusted_close_all), ]
  
  # Check for NAs again
  if (sum(is.na(adjusted_close_all)) > 0) {
    print('Missing Data Error: results are not reliable')
  }
  
  N = length(tickers)
  Time = length(dates) - 1
  
  sam_cov = cov(adjusted_close_all)
  mu = colMeans(adjusted_close_all)
  mu = matrix(mu, nrow=length(mu), ncol=1)
  
  # Initialization
  w0 = rep(1/N, N)
  w0 = matrix(w0, nrow=length(w0), ncol=1)
  
  obj <- function(w, cov=sam_cov, mean=mu) {
    SR = (t(w) %*% mean) / sqrt((t(w) %*% cov %*% w))
    return(-1 * SR)
  }
  
  sumw <- function(w) {
    return(sum(w))
  }
  
  out <- solnp(w0, fun=obj, eqfun=sumw, eqB=c(1))
  w_opt <- out$pars
  
  # Plot
  obj_mar <- function(w, cov=sam_cov) {
    var = (t(w) %*% cov %*% w)
    return(var)
  }
  eqf <- function(w, mean=mu) {
    return(c(sum(w), t(w) %*% mean))
  }
  
  rho <- seq(0, RF*200, RF)
  sigma_list = rho
  for (i in 1:length(rho)) {
    rho_cur <- rho[i]
    
    out <- solnp(w0, fun=obj_mar, eqfun=eqf, eqB=c(1, rho_cur))
    sigma_list[i] = sqrt(tail(out$values, 1))
  }
  
  # Our Portfolio
  w_ours = w0
  for (i in 1:length(tickers)) {
    w_ours[i] = cur_pos[, tickers[i]] * cur_price[, tickers[i]]
  }
  w_ours = w_ours / sum(w_ours)
  
  # RF
  sigma_list <- c(sigma_list, 0)
  rho <- c(rho, RF)
  
  mark_out <- list(sigma_list, rho)
  names(mark_out) <- c("x", "y")
  
  gplot <- ggplot(data.frame(mark_out), aes(x=x, y=y)) + 
    geom_point() + 
    geom_point(aes(x=sqrt(w_opt %*% sam_cov %*% w_opt), y=w_opt %*% mu), color="blue", size=3) + 
    geom_point(aes(x=sqrt(t(w_ours) %*% sam_cov %*% w_ours), y=t(w_ours) %*% mu), color="green", size=3) + 
    geom_point(aes(x=0, y=RF), color="red", size=3) + 
    geom_segment(aes(x=0, y=RF, xend=sqrt(w_opt %*% sam_cov %*% w_opt), yend=w_opt %*% mu)) + 
    labs(x="sigma", y="r")
  print(gplot)
  print(w_ours)
  print(w_opt)
  
  return(w_opt)
}

KudlaBenchmark <- function(startdate, enddate) {
  SP500 = getSymbols(
    "^GSPC",
    from = as.character(startdate),
    to = as.character(enddate+1),
    auto.assign = FALSE)
  SP500_Adj_Close = SP500[, "GSPC.Adjusted"]
  SP500_Return = Delt(SP500_Adj_Close, type = 'log')[2:length(SP500_Adj_Close)]
  
  EFA = getSymbols(
    "EFA",
    from = as.character(startdate),
    to = as.character(enddate+1),
    auto.assign = FALSE)
  EFA_Adj_Close = EFA[, "EFA.Adjusted"]
  EFA_Return = Delt(EFA_Adj_Close, type = 'log')[2:length(EFA_Adj_Close)]
  
  AGG = getSymbols(
    "AGG",
    from = as.character(startdate),
    to = as.character(enddate+1),
    auto.assign = FALSE)
  AGG_Adj_Close = AGG[, "AGG.Adjusted"]
  AGG_Return = Delt(AGG_Adj_Close, type = 'log')[2:length(AGG_Adj_Close)]
  
  Benchmark_Returns = 0.6 * SP500_Return + 0.2 * EFA_Return + 0.2 * AGG_Return
  
  return(Benchmark_Returns)
}

SharpeRatioBenchmark <- function(AssetReturns, dates, print=TRUE) {
  IRReturns = GetInterestRates(dates)
  Excess_Return = AssetReturns - IRReturns
  Sharpe_Ratio = mean(Excess_Return, na.rm = TRUE) / sd(Excess_Return, na.rm = TRUE)
  print(mean(Excess_Return, na.rm = TRUE) * 25200)
  print(sd(Excess_Return, na.rm = TRUE) * sqrt(252))
  AnnualizationFactor = sqrt(252)
  Sharpe_Ratio = Sharpe_Ratio * AnnualizationFactor
  
  if (print == TRUE) {
    print("Annualized Daily Sharpe Ratio for the benchmark: ")
    print(Sharpe_Ratio)
  }
  
  return(Sharpe_Ratio)
}

##########################
########## Main ##########
##########################

setwd("C:/Python/kudla/kudla_quant")

portfolio = ReadPortfolio("C:/Python/kudla/kudla_quant/portfolio.csv")
print(portfolio)

portValue = PortfolioValue(portfolio)
print(portValue)

returns = periodReturn(portValue, period='yearly', leading=TRUE)
print(returns)

volatility = AnnualizedVolatility(portfolio)
print(volatility)

excessReturns = ExcessReturns(portfolio)

kurtosis = kurtosis(excessReturns)
skewness = skewness(excessReturns)

beta = CalculateBeta(portfolio)

plot(portValue)

PlotReturns(excessReturns)

PlotReturns(CalculateDailyReturns(portfolio) * 100)

CalculateBeta(portfolio)

TotalReturn = TotalReturn(portfolio)

SharpeRatio = SharpeRatio(portfolio)

Benchmark_Returns = KudlaBenchmark(portfolio$dates[1], portfolio$dates[length(portfolio$dates)])
SharpeRatioBenchmark(Benchmark_Returns, portfolio$dates)

portfolio_data = read.csv(file = "C:/Python/Kudla/kudla_quant/portfolio.csv", header=TRUE, sep=",")
dates = as.Date(levels(portfolio_data[, 1]), "%m/%d/%Y")
dates = dates[order(dates)]
fromdate <- dates[1]
todate <- dates[length(dates)]

SP500 = getSymbols(
  "^GSPC",
  from = as.character(fromdate),
  to = as.character(todate+1),
  auto.assign = FALSE)

SP500_Adj_Close = data.frame(SP500)$GSPC.Adjusted
SP500_Return = Delt(SP500_Adj_Close, type = 'log')[2:length(SP500_Adj_Close)]
SP500_AnnualReturn = (SP500_Adj_Close[1] - SP500_Adj_Close[length(SP500_Adj_Close)])/(SP500_Adj_Close[1])
SP500_Sharpe = SharpeRatioSP500(SP500_Return,dates)
SP500_Volatility = AnnualizedVolatility_SP500(SP500_Return)
knit('C:/Python/Kudla/kudla_quant/gng.Rmd')

efficient <- EfficientFrontier(portfolio)
