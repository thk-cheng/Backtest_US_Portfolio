#Load (& Install) all necessary packages

#install.packages("tidyverse")
#install.packages("quantmod")
#install.packages("ellipsis") #prevent error message
#update.packages("ellipsis") #prevent error message
#install.packages("tidyquant")
#install.packages("rugarch")
#install.packages("rmgarch")
library(tidyquant)
library(tidyverse) #ggplot2, tibble, readr, tidyr, dplyr, purrr, stringr, forcats
library(scales)
library(tseries)
library(rugarch)
library(rmgarch)
library(car)
##################################################################################

##################################################################################
##################################################################################

##################################################################################
# ---Backtest my current portfolio---

# Set Backtest time period
begin_date <- "2017-10-08"
end_date <- "2020-10-08"

# Monthly risk-free rate (Taken from 1-month US T-Bill)
Rf_tibble <- tq_get(x    = "^IRX",
                    get  = "stock.prices",
                    from = begin_date,
                    to   = end_date)
Rf_tibble <- Rf_tibble %>%
  slice(length(Rf_tibble$date))

Rf <- ((Rf_tibble$adjusted)*4/13)/100

# Load current portfolio
setwd("/Users/kennethcheng/Desktop/US Portfolio")
current_portfolio <- read.csv("current_portfolio.csv",header=FALSE)

# Get tickers
my_tickers <- as.character(current_portfolio$V1)
baseline_tickers <- c("SPY","DIA","QQQ")

#Set weights
my_weights_normalised <- as.numeric(current_portfolio$V2)
my_weights_normalised <- my_weights_normalised + (1 - sum(my_weights_normalised))/length(my_weights_normalised) #Forcing the sum of weight to be 1
sum(my_weights_normalised) == 1
##my_weights <- rep(1,length(my_tickers)) # For equally weighted portfolio
##my_weights_normalised <- my_weights/sum(my_weights) #Normalising, i.e. sum to 1

# Summary of my current holdings
my_holdings <- tibble(label = my_tickers,
                      shares = percent(my_weights_normalised, accuracy = .01))
my_holdings

# Get all historical price within the appropriate time period
stocks_daily_price <- my_tickers %>%
  tq_get(get  = "stock.prices",
         from = begin_date,
         to   = end_date) %>%
  group_by(symbol)
options(tibble.print_max = 30)
stocks_daily_price %>% slice(1)

# Convert price into monthly log return
stocks_monthly_return <- stocks_daily_price %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               type       = "log",
               col_rename = "monthly.return")
stocks_monthly_return

# Convert individual monthly log return into portfolio monthly log return
portfolio_monthly_return <- stocks_monthly_return %>%
  tq_portfolio(assets_col  = symbol,
               returns_col = monthly.return,
               weights     = my_weights_normalised,
               col_rename  = "Rp")
portfolio_monthly_return

# Get some baseline indices for later comparison
baseline_monthly_return <- baseline_tickers %>%
  tq_get(get  = "stock.prices",
         from = begin_date,
         to   = end_date) %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               type       = "log")  %>%
  spread(key   = symbol,
         value = monthly.returns)
baseline_monthly_return

# Combine portfolio with baseline
RpRb_monthly_return <- left_join(x  = portfolio_monthly_return,
                                 y  = baseline_monthly_return,
                                 by = "date")
RpRb_monthly_return
  
# Examine performance of portfolio
  
  # Summary of basic statistics about the portfolio
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = NULL,
                   performance_fun = table.Stats,
                   ci              = 0.99,
                   digits          = 2)
  
  # Summary of Variability of monthly portfolio returns
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = NULL,
                   performance_fun = table.Variability,
                   digits          = 2)

  # Monthly Historical VaR
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = NULL,
                   performance_fun = VaR,
                   p               = 0.99,
                   method          = "historical")

  # Annualized SharpeRatio
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = NULL,
                   performance_fun = SharpeRatio,
                   Rf              = Rf,
                   p               = 0.99,
                   annualize       = TRUE)
  
  # Evaluate portfolio under CAPM
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = SPY,
                   performance_fun = table.CAPM,
                   Rf              = Rf,
                   digits          = 2)
  
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = DIA,
                   performance_fun = table.CAPM,
                   Rf              = Rf,
                   digits          = 2)
  
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = QQQ,
                   performance_fun = table.CAPM,
                   Rf              = Rf,
                   digits          = 2)

  # Annualized Returns, Sharpe Ratio and Standard Deviation
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = NULL,
                   performance_fun = table.AnnualizedReturns,
                   Rf              = Rf,
                   digits          = 2)
    # Also list the annualized returns for different baseline indices for comparison
    RpRb_monthly_return %>%
      tq_performance(Ra              = SPY,
                     Rb              = NULL,
                     performance_fun = table.AnnualizedReturns,
                     Rf              = Rf,
                     digits          = 2)
    
    RpRb_monthly_return %>%
      tq_performance(Ra              = DIA,
                     Rb              = NULL,
                     performance_fun = table.AnnualizedReturns,
                     Rf              = Rf,
                     digits          = 2)
    
    RpRb_monthly_return %>%
      tq_performance(Ra              = QQQ,
                     Rb              = NULL,
                     performance_fun = table.AnnualizedReturns,
                     Rf              = Rf,
                     digits          = 2)
  
  # Correlation to different baselines and the respective confidence intervals
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = SPY,
                   performance_fun = table.Correlation,
                   conf.level      = 0.99)
  
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = DIA,
                   performance_fun = table.Correlation,
                   conf.level      = 0.99)
  
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = QQQ,
                   performance_fun = table.Correlation,
                   conf.level      = 0.99)
  
  # Summary of Porfolio's Downside Risk
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = NULL,
                   performance_fun = table.DownsideRisk,
                   Rf              = Rf,
                   MAR             = 0.1/12,
                   ci              = 0.99,
                   p               = 0.99,
                   digits          = 2)
  
  # Ratios about Portfolio's Downside Risk
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = NULL,
                   performance_fun = table.DownsideRiskRatio,
                   MAR             = 0.1/12,
                   digits          = 2)

  # Examine Higher Moments b/w portfolio and baseline indices
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = SPY,
                   performance_fun = table.HigherMoments,
                   Rf              = Rf,
                   digits          = 2,
                   method          = "moment")
  
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = DIA,
                   performance_fun = table.HigherMoments,
                   Rf              = Rf,
                   digits          = 2,
                   method          = "moment")
  
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = QQQ,
                   performance_fun = table.HigherMoments,
                   Rf              = Rf,
                   digits          = 2,
                   method          = "moment")
  
  # Information Ratio for different baseline indices
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = SPY,
                   performance_fun = table.InformationRatio,
                   digits          = 2)
  
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = DIA,
                   performance_fun = table.InformationRatio,
                   digits          = 2)
  
  RpRb_monthly_return %>%
    tq_performance(Ra              = Rp,
                   Rb              = QQQ,
                   performance_fun = table.InformationRatio,
                   digits          = 2)

# Gain insight from graphs
  
  # Portfolio return time series plot
  portfolio_monthly_return %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = Rp), size = 1, color = palette_light()[[1]]) +
    geom_hline(yintercept = 0, color = palette_dark()[[1]]) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Portfolio Monthly Returns",
         y = "Monthly Returns", x = "") + 
    theme_tq() + 
    scale_fill_tq() +
    scale_color_tq()
  
  # Empirical distribution of portfolio return
  portfolio_monthly_return %>%
    ggplot(aes(x = Rp)) +
    geom_density(alpha = 0.6, size = 1, fill = palette_light()[[1]]) +
    xlim(-0.2,0.2) +
    labs(title = "Empirical distribution of portfolio monthly return ",
         x = "Monthly log returns", y = "Density") +
    theme_tq() +
    scale_fill_tq() +
    scale_color_tq()
  
  # Wealth perspective
    # Set principal amount (in USD)
    principal = 10000
    
    # Calculuate growth of portfolio and baseline indices
    portfolio_monthly_growth <- stocks_monthly_return %>%
      tq_portfolio(assets_col   = symbol, 
                   returns_col  = monthly.return, 
                   weights      = my_weights_normalised, 
                   col_rename   = "investment.growth",
                   wealth.index = TRUE) %>%
      mutate(investment.growth = principal * investment.growth)
    portfolio_monthly_growth
  
    baseline_monthly_growth <- baseline_monthly_return %>%
      mutate(SPY = principal * cumprod(1 + SPY),
             DIA = principal * cumprod(1 + DIA),
             QQQ = principal * cumprod(1 + QQQ))
    baseline_monthly_growth
    
    # Combine growth series into 1 tibble
    RpRb_monthly_growth <- left_join(x  = portfolio_monthly_growth,
                                     y  = baseline_monthly_growth,
                                     by = "date")
    RpRb_monthly_growth
    
    # Plot the growth of wealth for portfolio and baseline indices
    RpRb_monthly_growth %>%
      ggplot(aes(x = date)) +
      geom_line(aes(y = investment.growth), size = 1, color = palette_dark()[[1]]) +
      geom_line(aes(y = SPY), size = 1, color = palette_dark()[[2]]) +
      geom_line(aes(y = DIA), size = 1, color = palette_dark()[[3]]) +
      geom_line(aes(y = QQQ), size = 1, color = palette_dark()[[4]]) +
      #geom_smooth(aes(y = investment.growth),method="loess", size = 1, color = palette_light()[[1]]) +
      #geom_smooth(aes(y = SPY),method="loess", size = 1, color = palette_light()[[2]]) +
      #geom_smooth(aes(y = DIA),method="loess", size = 1, color = palette_light()[[3]]) +
      #geom_smooth(aes(y = QQQ),method="loess", size = 1, color = palette_light()[[4]]) +
      labs(title     = "Portfolio Growth",
           subtitle  = "Current US Portfolio vs 3 Baseline indices",
           x = "", y = "Portfolio Value (log scale)") +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::dollar)
  