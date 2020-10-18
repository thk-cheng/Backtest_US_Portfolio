#Demonstration for the package tidyquant

#install.packages("tidyverse")
#install.packages("quantmod")
#install.packages("ellipsis") #prevent error message
#update.packages("ellipsis") #prevent error message
#install.packages("tidyquant")
library(tidyquant)
library(tidyverse) #ggplot2, tibble, readr, tidyr, dplyr, purrr, stringr, forcats
##################################################################################

# --- Yahoo Finance ---
tq_get_options()

# One stock
aapl <- tq_get('AAPL', from = "2017-01-01", to = "2018-03-01", get = "stock.prices")
head(aapl)

aapl %>% ggplot(aes(x = date, y = adjusted)) +
  geom_line() + theme_classic() +
  labs(x = 'Date', y = "Adjusted Price", title = "Apple price chart") +
  scale_y_continuous(breaks = seq(0,300,10))

#Multiple stocks
tickers = c("AAPL", "NFLX", "AMZN", "K", "O")
prices <- tq_get(tickers, from = "2017-01-01", to = "2017-03-01", get = "stock.prices")
head(prices)
prices %>% group_by(symbol) %>% slice(1)

prices %>% ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line()

prices %>% ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line() + theme_classic() +
  facet_wrap(~symbol,scales = 'free_y') +
  labs(x = 'Date', y = "Adjusted Price", title = "Price Chart") +
  scale_x_date(date_breaks = "month", date_labels = "%b\n%y")
##################################################################################

# --- Fred (Economic.data) ---
CPIAUCNS <- tq_get('CPIAUCNS', get = "economic.data")
head(CPIAUCNS)

wti_price_usd <- tq_get("DCOILWTICO", get = "economic.data")
wti_price_usd 
##################################################################################

# --- Quandl ---
quandl_api_key('Xky-n7fArqktHbY38-Ym')

#Conduct search
quandl_search(query = "Corporate Bond", database = "ML", per_page = 10)
                                        #Database refers to the publisher

# Energy data from EIA
tq_get("EIA/PET_MTTIMUS1_M", get = "quandl", from = "2010-01-01")

#Stock Market Confidence Indices - US Valuation Index Data - Individual, from Yale University
tq_get("YALE/US_CONF_INDEX_VAL_INDIV", get = "quandl", from = "2010-01-01")

c("WIKI/FB", "WIKI/AAPL") %>% #Stock price for FB and AAPL
  tq_get(get  = "quandl",
         from = "2016-01-01",
         to   = "2016-12-31")

c("WIKI/FB", "WIKI/AAPL") %>% #Annual return for FB and AAPL
  tq_get(get          = "quandl",
         from         = "2007-01-01",
         to           = "2016-12-31",
         column_index = 11, #Specify which column we want to take
         collapse     = "annual", #Specify how frequent do we extract the data  
         transform    = "rdiff") #Perform calculation on values, e.g. rdiff = % difference
##################################################################################

# --- Tiingo ---
tiingo_api_key('27134eb3265839fae0f62823bea70202c6b1ad9b')

# Tiingo Prices (Free alternative to Yahoo Finance!)
tq_get(c("AAPL", "GOOG"), get = "tiingo", from = "2010-01-01")

# Sub-daily prices from IEX ----
tq_get(c("AAPL", "GOOG"),
       get = "tiingo.iex",
       from = "2020-01-01",
       to = "2020-01-15",
       resample_frequency = "5min")

# Tiingo Bitcoin Prices ----
tq_get(c("btcusd", "btceur"),
       get = "tiingo.crypto",
       from = "2020-01-01",
       to = "2020-01-15",
       resample_frequency = "5min")
##################################################################################

# --- Alpha Vantage ---
av_api_key('ZV9TI735CFQJLCAW')

# Daily Time Series
tq_get("AAPL",
       get = "alphavantager",
       av_fun = "TIME_SERIES_DAILY_ADJUSTED", #Check Website Documentation for more options
       outputsize = "full")

# Intraday 15 Min Interval
tq_get("AAPL",
       get = "alphavantage",
       av_fun = "TIME_SERIES_INTRADAY",
       interval = "15min",
       outputsize = "full")

# FX DAILY
tq_get("USD/EUR", get = "alphavantage", av_fun = "FX_DAILY", outputsize = "full")

# FX REAL-TIME QUOTE
tq_get("USD/EUR", get = "alphavantage", av_fun = "CURRENCY_EXCHANGE_RATE")

# Bitcon-USD REAL-TIME QUOTE
tq_get("BTC/USD", get = "alphavantage", av_fun = "CURRENCY_EXCHANGE_RATE")
##################################################################################

# --- Indices ---
tq_index_options()
DOWJONES <- tq_index("DOW")
head(DOWJONES)

DOWJONES_price <- DOWJONES %>% tq_get(get = "stock.prices")
head(DOWJONES_price)
DOWJONES_price %>% group_by(symbol) %>% slice(1)
##################################################################################

# --- Exchanges ---
tq_exchange_options()
NYSE <- tq_exchange("NYSE") #Doesn't work at the moment
##################################################################################

##################################################################################
##################################################################################

##################################################################################
# --- Transmute Function ---
#Only the newly created columns will be returned
#No. of rows returned can be different than the original data frame
data("FANG")
FANG

#OHLC data
FANG %>% 
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = to.monthly, indexAt = "lastof")
                                                            #Show full date (dd/mm/yyyy)
FANG %>% 
    group_by(symbol) %>%    
    tq_transmute(select = adjusted,mutate_fun = dailyReturn) %>%
    ggplot(aes(x = date, y = daily.returns, color = symbol)) +
    geom_line() +
    labs(x = 'Date', y = "Daily Returns", title = "Daily Returns Chart")

#Non-OHLC data
wti_prices <- tq_get("DCOILWTICO", get = "economic.data") 

wti_prices %>%    
  tq_transmute(mutate_fun = to.period, #Need to do it this way to show full date (dd/mm/yyyy)
               period     = "months", #otherwise it will only show mm/yyyy
               col_rename = "WTI Price") #Optional
##################################################################################

# --- Mutate Function ---
#Old dataframe + Newly created columns will be returned
#So, the newly added columns must have same rows with the old dataframe
data("FANG")
FANG

# Easy example
FANG %>%
  group_by(symbol) %>%
  tq_mutate(select     = close, 
            mutate_fun = MACD, 
            col_rename = c("MACD", "Signal"))

# Rolling regression (customized function)
fb_returns <- tq_get("FB", get  = "stock.prices", from = "2016-01-01", to   = "2016-12-31") %>%
  tq_transmute(adjusted, periodReturn, period = "weekly", col_rename = "fb.returns")

xlk_returns <- tq_get("XLK", from = "2016-01-01", to = "2016-12-31") %>%
  tq_transmute(adjusted, periodReturn, period = "weekly", col_rename = "xlk.returns")

(returns_combined <- left_join(fb_returns, xlk_returns, by = "date"))

#install.packages("timetk")
library(timetk)
regr_fun <- function(data) {
  coef(lm(fb.returns ~ xlk.returns, data = tk_tbl(data, silent = TRUE)))
}

returns_combined %>%
  tq_mutate(mutate_fun = rollapply,
            width      = 12, #12 week rolling window
            FUN        = regr_fun,
            by.column  = FALSE, #Important, apply to all data, rather than 1 column
            col_rename = c("coef.0", "coef.1"))

# Example for mutate_xy (works the same for transmute_xy)
FANG %>%
  group_by(symbol) %>%
  tq_mutate_xy(x = close, y = volume, 
               mutate_fun = EVWMA, col_rename = "EVWMA")
##################################################################################

##################################################################################
##################################################################################

##################################################################################
# ---Details about transmute_fun and mutate_fun---
tq_transmute_fun_options() %>% str()
tq_mutate_fun_options() %>% str()

# zoo Functionality
tq_transmute_fun_options()$zoo
  #Roll Apply Functions:
    #A generic function for applying a function to rolling margins.
    fb_price <- tq_get("FB", get  = "stock.prices", from = "2016-01-01", to   = "2016-12-31")
    fb_price
    
      #Form: rollapply(data, width, FUN, ..., by = 1, by.column = TRUE,
      #                fill = if (na.pad) NA, na.pad = FALSE, partial = FALSE,
      #                align = c("center", "left", "right"), coredata = TRUE).
      fb_rollapply <- fb_price %>% 
                      tq_transmute(adjusted,rollapply,width=5,FUN=mean,align="center",col_rename = "5 points MA filter")
      fb_rollapply
      
      #Options include rollmax, rollmean, rollmedian, rollsum, etc.
      #rollsumr: align = "right" version of rollsum, others the same
      #rollsum.default: ignore, use plain rollsum or rollapply instead
      fb_rollmax <- fb_price %>% 
                    tq_transmute(adjusted,rollmax,k=5,col_rename = "rollmax") #align doesn't matter
      fb_rollmax
      
      fb_rollmean <- fb_price %>% 
                      tq_transmute(adjusted,rollmean,k=5,col_rename = "rollmean") #align doesn't matter
      fb_rollmean
      
      fb_rollmedian <- fb_price %>%
                        tq_transmute(adjusted,rollmedian,k=5,col_rename = "rollmedian") #align doesn't matter
      fb_rollmedian
      
      fb_rollsum <- fb_price %>%
                    tq_transmute(adjusted,rollsum,k=5,col_rename = "rollsum") #align doesn't matter
      fb_rollsum

# xts Functionality
tq_transmute_fun_options()$xts
  #Period Apply Functions:
    #Apply a function to a time segment (e.g. max, min, mean, etc).
    #Form: apply.daily(x, FUN, ...).
    #Options include apply.daily, weekly, monthly, quarterly, yearly.
    fb_apply_monthly <- fb_price %>%
      tq_transmute(adjusted,apply.monthly,FUN=max,col_rename = "Monthly max price")
    fb_apply_monthly
    
    fb_apply_weekly <- fb_price %>%
      tq_transmute(adjusted,apply.weekly,FUN=mean,col_rename = "Weekly mean price")
    fb_apply_weekly
    
    #More generally, period.apply with options including max,min,prod,sum
    fb_period_apply <- fb_price %>%
      tq_transmute(adjusted,period.apply,FUN=max,INDEX=seq(1,60,by=5),col_rename = "Non-rolling 5 day maximum")
    fb_period_apply
    
    fb_period_prod <- fb_price %>%
      tq_transmute(adjusted,period.prod,INDEX=seq(1,60,by=5),col_rename = "Non-rolling 5 day product")
    fb_period_prod
    
  #To-Period Functions:
    #Convert a time series to time series of lower periodicity (e.g. convert daily to monthly periodicity).
    #Form: to.period(x, period = 'months', k = 1 (only for min and sec), indexAt, name = NULL, OHLC = TRUE, ...).
    fb_to_period <- fb_price %>%
      tq_transmute(adjusted,to.period,period="months",col_rename = "Monthly price")
    fb_to_period
    
    #Options include to.minutes, hourly, daily, weekly, monthly, quarterly, yearly.
    fb_to_monthly <- fb_price %>%
      tq_transmute(adjusted,to.monthly,col_rename = "Monthly price (Notice the date labelling)")
    fb_to_monthly
    
      #Note 1 (Important): The return structure is different for to.period and the (to.monthly, to.weekly e.t.c.) forms.
      #to.period returns a date, while to.months returns a character "MON YYYY".
      #Best to use to.period if you want to work with time-series via lubridate.
  
  #Periodicity, diff.xts:
    #Periodicity estimates the frequency of data
    fb_periodicity <- fb_price %>%
      tq_transmute(adjusted,periodicity,col_rename = "Periodicity")
    fb_periodicity
    
    #diff.xts computes the difference of desire lag and order of differencing, either arithmetic or log scale
    fb_diff_xts <- fb_price %>%
      tq_transmute(adjusted,diff.xts,lag=3,differencing=2,log=TRUE,col_rename = "Log diff, lag=3, order=2")
    fb_diff_xts
    
# quantmod Functionality
tq_transmute_fun_options()$quantmod
  #Percentage Change (Delt) and Lag Functions:
  fb_price <- tq_get("FB", get  = "stock.prices", from = "2016-01-01", to   = "2016-12-31")
  fb_price
  
    #Delt: Delt(x1, x2 = NULL, k = 0, type = c("arithmetic", "log"))
    fb_log_per_chg <- fb_price %>%
                     tq_transmute(select = adjusted, mutate_fun = Delt,
                                   type = "log",col_rename = "log % chg")
    fb_log_per_chg #log % chg (can choose to be arithmetic)
    
      #Variations of Delt: ClCl, HiCl, LoCl, LoHi, OpCl, OpHi, OpLo, OpOp
      fb_ClCl_per_diff <- fb_price %>%
                         tq_transmute(mutate_fun = ClCl, col_rename = "CLCL % diff")
      fb_ClCl_per_diff #% diff of closed price
      
      fb_OpHi_per_diff <- fb_price %>%
                          tq_transmute(mutate_fun = OpHi, col_rename = "OpHi % diff")
      fb_OpHi_per_diff #% diff b/w open and high
    
    #Lag: Lag(x, k = 1) / Next: Next(x, k = 1) (Can also use dplyr::lag and dplyr::lead)
    fb_Lag_1 <- fb_price %>%
                tq_transmute(select = adjusted, mutate_fun = Lag, k = 1, col_rename = "Lag_1")
    fb_Lag_1 #Shift series k-periods down

    fb_Next_1 <- fb_price %>%
                  tq_transmute(select = adjusted, mutate_fun = Next, k = 1, col_rename = "Next_1")
    fb_Next_1 #Shift series k-periods up
  
  #Period Return Functions:
    #Get the arithmetic or logarithmic returns for various periodicity, which include daily, weekly, monthly, quarterly, and yearly.
    #Form: periodReturn(x, period = 'monthly', subset = NULL, type = 'arithmetic', leading = TRUE, ...)
    fb_period_return <- fb_price %>%
                          tq_transmute(select = adjusted, mutate_fun = periodReturn, 
                                       period = 'monthly', type = 'log',
                                       leading = FALSE, col_rename = "monthly_return")
    fb_period_return #Calculate return of the desired frequency (arithmetic or log)

    fb_all_returns <- fb_price %>%
                      tq_transmute(select = adjusted, mutate_fun = allReturns, 
                                    type = 'log', leading = FALSE)
    fb_all_returns #Calculate return for all available frequency (arithmetic or log)
  
  #Series Functions:
    #Return values that describe the series. Options include describing the increases/decreases, acceleration/deceleration, and hi/low.
    #Forms: seriesHi(x), seriesIncr(x, thresh = 0, diff. = 1L), seriesAccel(x)
    fb_Hi <- fb_price %>%
              tq_transmute(select=adjusted,mutate_fun = seriesHi, col_rename = "Highest")
    fb_Hi #Find the highest value of the price series

    fb_Accel <- fb_price %>%
              tq_transmute(select=adjusted,mutate_fun = seriesAccel, col_rename = "Accelerating?")
    fb_Accel #Measure whether the series is accelerating or not at each time point (~ 2nd deriv of time)

    fb_Incr <- fb_price %>%
                  tq_transmute(select=adjusted,mutate_fun = seriesIncr, col_rename = "Increasing?")
    fb_Incr #Measure whether the series is accelerating or not at each time point (~ 1st deriv of time)

# TTR Functionality
tq_transmute_fun_options()$TTR
  #All kinds of technical indicators
  #Skip first, come back if find something useful + has solid stat to support

#PerformanceAnalytics Functionality
tq_transmute_fun_options()$PerformanceAnalytics
  #The PerformanceAnalytics mutation functions all deal with returns:
  fb_price <- tq_get("FB", get  = "stock.prices", from = "2016-01-01", to   = "2016-12-31")
  fb_period_return <- fb_price %>%
                      tq_transmute(select = adjusted, mutate_fun = periodReturn, 
                      period = 'monthly', type = 'log',
                      leading = FALSE, col_rename = "monthly_return")
  fb_period_return
  
    #Return.annualized and Return.annualized.excess: Takes period returns and consolidates into annualized returns
    fb_Return_annualized <- fb_period_return %>%
                            tq_transmute(monthly_return,Return.annualized,col_rename = "Annualized Return")
    fb_Return_annualized
    
    fb_Return_annualized_excess <- fb_period_return %>%
                            tq_transmute(monthly_return,Return.annualized.excess,Rb=rep(0.0005,length(fb_period_return$monthly_return)),col_rename = "Annualized Return")
    fb_Return_annualized_excess
    
    #Return.clean: Removes outliers from returns
    fb_Return_clean <- fb_period_return %>%
                        tq_transmute(monthly_return,Return.clean,method="geltner",alpha=0.05,col_rename = "Cleaned return")
    fb_Return_clean
    
    #Return.excess: Removes the risk-free rate from the returns to yield returns in excess of the risk-free rate
    fb_Return_excess <- fb_period_return %>%
                        tq_transmute(monthly_return,Return.excess,Rf=0.0005,col_rename = "Excess return")
    fb_Return_excess
    
    #Return.cumulative: Calculates compounded (geometric) cumulative return
    fb_Return_cumulative <- fb_period_return %>%
                        tq_transmute(monthly_return,Return.cumulative,geometric=TRUE,col_rename = "Geometric cumulative return")
    fb_Return_cumulative
    
    #zerofill: Used to replace NA values with zeros
    fb_zerofill <- fb_period_return %>%
                    tq_transmute(monthly_return,zerofill,col_rename = "No NA now!")
    fb_zerofill
##################################################################################
# ---Quantitative Power in Action---
data("FANG")
FANG

#Example 1: Use quantmod periodReturn to Convert Prices to Returns
  #Example 1A: Getting and Charting Annual Returns
  FANG_annual_returns <- FANG %>%
                        group_by(symbol) %>%
                        tq_transmute(select     = adjusted,
                                     mutate_fun = periodReturn,
                                     period     = 'yearly',
                                     type = 'arithmetic')
  FANG_annual_returns
  
  FANG_annual_returns %>%
    ggplot(aes(x = date, y = yearly.returns, col = symbol)) +
    geom_line() +
    geom_hline(yintercept = 0, color = palette_light()[[1]]) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "FANG: Annual Returns",
         subtitle = "Get annual returns quickly with tq_transmute!",
         y = "Annual Returns", x = "") + 
    facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
    theme_tq() + 
    scale_fill_tq()
  
  #Example 1B: Getting Daily Log Returns
  FANG_daily_log_returns <- FANG %>%
                            group_by(symbol) %>%
                            tq_transmute(select     = adjusted,
                                         mutate_fun = periodReturn,
                                         period     = "daily",
                                         type       = "log")
  FANG_daily_log_returns

  FANG_daily_log_returns %>%
    ggplot(aes(x = daily.returns, fill = symbol)) +
    geom_density(alpha = 0.5) +
    labs(title = "FANG: Charting the Daily Log Returns",
         x = "Daily log returns", y = "Density") +
    theme_tq() +
    scale_fill_tq() + 
    facet_wrap(~ symbol, ncol = 2)

#Example 2: Use xts to.period to Change the Periodicity from Daily to Monthly
FANG_monthly <- FANG %>%
                group_by(symbol) %>%
                tq_transmute(select     = adjusted,
                             mutate_fun = to.period,
                             period     = "months")

FANG_monthly %>% #Monthly data is more smooth graphically
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Monthly Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() + 
  scale_color_tq()

FANG %>% #As opposed to daily data, which is more spiky
  group_by(symbol) %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Daily Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() + 
  scale_color_tq()

#Example 3: Use TTR runCor to Visualize Rolling Correlations of Returns
# Asset Returns
FANG_returns_monthly <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn,
               period     = "monthly")
FANG_returns_monthly

# Baseline Returns
baseline_returns_monthly <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2013-01-01", 
         to   = "2016-12-31") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn,
               period     = "monthly")
baseline_returns_monthly

#Join the two returns series
returns_joined <- left_join(FANG_returns_monthly, 
                            baseline_returns_monthly,
                            by = "date")
returns_joined

#Rolling 6-month correlation
FANG_rolling_corr <- returns_joined %>%
  tq_transmute_xy(x          = monthly.returns.x,
                  y          = monthly.returns.y,
                  mutate_fun = runCor,
                  n          = 6,
                  col_rename = "rolling.corr.6")
FANG_rolling_corr

FANG_rolling_corr %>%
  ggplot(aes(x = date, y = rolling.corr.6, color = symbol)) +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(size = 1) +
  labs(title = "FANG: Six Month Rolling Correlation to XLK",
       x = "", y = "Correlation", color = "") +
  facet_wrap(~ symbol, ncol = 2) +
  theme_tq() + 
  scale_color_tq()

#Example 4: Use TTR MACD to Visualize Moving Average Convergence Divergence
FANG_macd <- FANG %>%
  group_by(symbol) %>%
  tq_mutate(select     = close, 
            mutate_fun = MACD, 
            nFast      = 12, 
            nSlow      = 26, 
            nSig       = 9, 
            maType     = EMA) %>%
  mutate(diff = macd - signal) %>%
  select(-(open:volume))
FANG_macd %>% slice(26:36)

FANG_macd %>%
  filter(date >= as_date("2016-10-01")) %>%
  ggplot(aes(x = date)) + 
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(aes(y = macd, col = symbol)) +
  geom_line(aes(y = signal), color = "blue", linetype = 2) +
  geom_bar(aes(y = diff), stat = "identity", color = palette_light()[[1]]) +
  facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
  labs(title = "FANG: Moving Average Convergence Divergence",
       y = "MACD", x = "", color = "") +
  theme_tq() +
  scale_color_tq()

#Example 5: Use xts apply.quarterly to Get the Max and Min Price for Each Quarter
FANG_max_by_qtr <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = apply.quarterly,
               FUN        = max,
               col_rename = "max.close") %>%
  mutate(year.qtr = paste0(year(date), "-Q", quarter(date))) %>%
  select(-date)
FANG_max_by_qtr

FANG_min_by_qtr <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = apply.quarterly,
               FUN        = min,
               col_rename = "min.close") %>%
  mutate(year.qtr = paste0(year(date), "-Q", quarter(date))) %>%
  select(-date)
FANG_min_by_qtr

FANG_by_qtr <- left_join(FANG_max_by_qtr, FANG_min_by_qtr,
                         by = c("symbol"   = "symbol",
                                "year.qtr" = "year.qtr"))
FANG_by_qtr

FANG_by_qtr %>%
  ggplot(aes(x = year.qtr, color = symbol)) +
  geom_point(aes(y = max.close), size = 2) +
  geom_point(aes(y = min.close), size = 2) +
  geom_segment(aes(xend = year.qtr, y = min.close, yend = max.close), size = 1) +
  facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "FANG: Min/Max Price By Quarter", y = "Stock Price", color = "") +
  theme_tq() +
  scale_color_tq() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank())

#Example 6: Use zoo rollapply to visualize a rolling regression
# Get stock pairs
stock_prices <- c("MA", "V") %>%
  tq_get(get  = "stock.prices",
         from = "2015-01-01",
         to   = "2016-12-31") %>%
  group_by(symbol) 

stock_pairs <- stock_prices %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               type       = "log",
               col_rename = "returns") %>%
  spread(key = symbol, value = returns) #**IMPORTANT FUNCTION** Spread different symbols into its own column, essential for portfolio analysis
stock_pairs

stock_pairs %>%
  ggplot(aes(x = V, y = MA)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Visualizing Returns Relationship of Stock Pairs") +
  theme_tq()

lm(MA ~ V, data = stock_pairs) %>%
  summary()

regr_fun <- function(data) { #Custom regression function for rollapply
  coef(lm(MA ~ V, data = timetk::tk_tbl(data, silent = TRUE)))
}

stock_pairs <- stock_pairs %>%
  tq_mutate(mutate_fun = rollapply,
            width      = 90,
            FUN        = regr_fun,
            by.column  = FALSE,
            col_rename = c("coef.0", "coef.1"))
stock_pairs

stock_pairs %>% #Fluctuations of the regression coefficient b/w V and MA over multiple 90-days windows
  ggplot(aes(x = date, y = coef.1)) +
  geom_line(size = 1, color = palette_light()[[1]]) +
  geom_hline(yintercept = 0.8134, size = 1, color = palette_light()[[2]]) +
  labs(title = "MA ~ V: Visualizing Rolling Regression Coefficient", x = "") +
  theme_tq()

stock_prices %>% #Assume principal $100, how will the value evolves over time according to the returns
  tq_transmute(adjusted, 
               periodReturn, 
               period = "daily", 
               type = "log", 
               col_rename = "returns") %>%
  mutate(wealth.index = 100 * cumprod(1 + returns)) %>%
  ggplot(aes(x = date, y = wealth.index, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "MA and V: Stock Prices") +
  theme_tq() +
  scale_color_tq()

#Example 7: Use Return.clean and Return.excess to clean and calculate excess returns
FANG %>%
  group_by(symbol) %>%
  tq_transmute(adjusted, periodReturn, period = "daily") %>%
  tq_transmute(daily.returns, Return.clean, alpha = 0.05) %>%
  tq_transmute(daily.returns, Return.excess, Rf = 0.03 / 252)
##################################################################################

##################################################################################
##################################################################################

##################################################################################
# ---Performance Analysis with tidyquant---

#Quick Example
Ra <- c("AAPL", "GOOG", "NFLX") %>%
  tq_get(get = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Ra")
Ra

Rb <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")
Rb

RaRb <- left_join(Ra, Rb, by = "date")
RaRb

RaRb_capm <- RaRb %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)
RaRb_capm

RaRb_capm %>% select(symbol, Alpha, Beta)

#Standard Workflow for Portfolio Analysis
  #Individual Assets
  stock_returns_monthly <- c("AAPL", "GOOG", "NFLX") %>%
    tq_get(get  = "stock.prices",
           from = "2010-01-01",
           to   = "2015-12-31") %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted,
                 mutate_fun = periodReturn,
                 period     = "monthly",
                 col_rename = "Ra")
  
  stock_returns_monthly %>%
    tq_performance(Ra              = Ra, 
                   Rb              = NULL,
                   performance_fun = SharpeRatio,
                   Rf              = 0.03 / 12,
                   p               = 0.99)

  #Portfolios (Asset Groups)
    #Single Portfolio
    stock_returns_monthly <- c("AAPL", "GOOG", "NFLX") %>%
      tq_get(get  = "stock.prices",
             from = "2010-01-01",
             to   = "2015-12-31") %>%
      group_by(symbol) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn,
                   period     = "monthly",
                   col_rename = "Ra")
    
    baseline_returns_monthly <- "XLK" %>%
      tq_get(get  = "stock.prices",
             from = "2010-01-01",
             to   = "2015-12-31") %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "monthly", 
                   col_rename = "Rb")
    
    wts <- c(0.5, 0.0, 0.5)
    portfolio_returns_monthly_ver1 <- stock_returns_monthly %>%
      tq_portfolio(assets_col  = symbol, 
                   returns_col = Ra, 
                   weights     = wts, 
                   col_rename  = "Ra")
    portfolio_returns_monthly_ver1
    #or we can use a tibble format for passing the weights
    wts_map <- tibble(
      symbols = c("AAPL", "NFLX"), #No need to supply every stocks
      weights = c(0.50, 0.50) #No weight = set to 0 by default
    )
    wts_map
    portfolio_returns_monthly_ver2 <- stock_returns_monthly %>%
      tq_portfolio(assets_col  = symbol, 
                   returns_col = Ra, 
                   weights     = wts_map, 
                   col_rename  = "Ra_using_wts_map")
    portfolio_returns_monthly_ver2
    
    RaRb_single_portfolio <- left_join(x = portfolio_returns_monthly_ver1,
                                       y = baseline_returns_monthly,
                                       by = "date")
    RaRb_single_portfolio
    
    RaRb_single_portfolio_CAPM <- RaRb_single_portfolio %>%
      tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
    RaRb_single_portfolio_CAPM
    
    #Multiple Portfolios
    stock_returns_monthly_multi <- stock_returns_monthly %>%
      tq_repeat_df(n = 3)
    stock_returns_monthly_multi
    
    weights <- c(0.50, 0.25, 0.25,
                 0.25, 0.50, 0.25,
                 0.25, 0.25, 0.50)
    stocks <- c("AAPL", "GOOG", "NFLX")
    weights_table <-  tibble(stocks) %>%
      tq_repeat_df(n = 3) %>%
      bind_cols(tibble(weights)) %>%
      group_by(portfolio)
    weights_table
    
    portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>%
      tq_portfolio(assets_col  = symbol, 
                   returns_col = Ra, 
                   weights     = weights_table, 
                   col_rename  = "Ra")
    portfolio_returns_monthly_multi
    
    RaRb_multiple_portfolio <- left_join(portfolio_returns_monthly_multi, 
                                         baseline_returns_monthly,
                                         by = "date")
    RaRb_multiple_portfolio
    
    RaRb_multiple_portfolio %>%
      tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
    
    RaRb_multiple_portfolio %>%
      tq_performance(Ra = Ra, Rb = NULL, performance_fun = SharpeRatio)
    
#Available Functions
tq_performance_fun_options()
  
  #table.Stats
  RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.Stats)

  #table.CAPM
  RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
  #Note: InformationRatio = (R_a - R_b)/sd(R_a - R_b) [standardized measure of excess return generated by the portfolio against a benchmark]
  #Note: Tracking Error = sd(R_a - R_b) [High indicates portfolio return is volatile -> probably doesn't track very well]
  #Note: TreynorRatio = (R_a - R_f)/beta [Measure how much excess return was generated for each unit of systematic risk taken on by a portfolio]
  
  #table.AnnualizedReturns
  RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.AnnualizedReturns)
  
  #table.Correlation
  RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.Correlation, conf.level = 0.99)
  
  #table.DownsideRisk
  RaRb_multiple_portfolio_DownsideRisk <- RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.DownsideRisk, Rf = 0, MAR = 0.1/12, ci = 0.99, p = 0.99)
  RaRb_multiple_portfolio_DownsideRisk
  #Note: DownsideDeviation = Measures the variability of underperformance below MAR (or R_f, or 0) = sqrt(DownsideVariance)
  #Note: SemiDeviation = Special case of DownsideDeviation when MAR = mean(R_a)
  #Note: GainDeviation (LossDeviation) = Measures the variability of gains (losses)
  #Note: MaximumDrawdown =  Maximum(Any time the cumulative returns dips below the maximum cumulative returns)
  #Note: ModifiedVaR/ES = The Cornish-Fisher asymptotic expansion for the quantile of a non-gaussian distribution is used
  
  #table.DownsideRiskRatio
  RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.DownsideRiskRatio, MAR = 0.1/12)
  #Note: DownsidePotential = L^1 version of DownsideVariance
  
  #table.HigherMoments
  RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.HigherMoments)
  #Note: Check out ?table.HigherMoments for more info, this is used to measure diversification
  
  #table.InformationRatio
  RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.InformationRatio)
  
  #table.Variability
  RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.Variability)
  
  #VaR
  RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = VaR, p = 0.99)
  
  #SharpeRatio
  RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = SharpeRatio, p = 0.99)
  
#Customizing using different options
  #Customizing tq_portfolio (which is a wrapper for Return.portfolio)
  args(Return.portfolio) #All of these options can be used in tq_portfolio
  ?Return.portfolio
  
    #Plotting wealth index for single portfolio vs baseline
    wts <- c(1/3, 1/3, 1/3)
    portfolio_growth_monthly <- stock_returns_monthly %>%
      tq_portfolio(assets_col   = symbol, 
                   returns_col  = Ra, 
                   weights      = wts, 
                   col_rename   = "investment.growth",
                   wealth.index = TRUE) %>%
      mutate(investment.growth = investment.growth * 10000)
    portfolio_growth_monthly
  
    baseline_growth_monthly <- baseline_returns_monthly %>%
      mutate(Rb = 10000 * cumprod(1 + Rb))
    baseline_growth_monthly
  
    RaRb_growth_monthly <- left_join(x  = portfolio_growth_monthly,
                                     y  = baseline_growth_monthly,
                                     by = "date")
    RaRb_growth_monthly
  
    RaRb_growth_monthly %>%
      ggplot(aes(x = date)) +
      geom_line(aes(y = investment.growth), size = 1, color = "darkblue") +
      geom_line(aes(y = Rb), size = 1, color = "darkred") +
      geom_smooth(aes(y = investment.growth),method="loess", size = 1) +
      geom_smooth(aes(y = Rb),method="loess", size = 1, color = "red") +
      labs(title     = "Portfolio Growth",
           subtitle  = "50% AAPL, 0% GOOG, and 50% NFLX vs Baseline",
           caption   = "Now we can really visualize performance!",
           x = "", y = "Portfolio Value (log scale)") +
      theme_tq() +
      scale_color_tq() +
      scale_y_log10(labels = scales::dollar)

    #Plotting wealth index for multiple portfolios
    portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>%
      tq_portfolio(assets_col   = symbol, 
                   returns_col  = Ra, 
                   weights      = weights_table, 
                   col_rename   = "investment.growth",
                   wealth.index = TRUE) %>%
      mutate(investment.growth = investment.growth * 10000)
    portfolio_growth_monthly_multi
    
    portfolio_growth_monthly_multi %>%
      ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) +
      geom_line(size = 1) +
      labs(title = "Portfolio Growth",
           subtitle = "Comparing Multiple Portfolios vs baseline",
           caption = "Portfolio 3 is a Standout!",
           x = "", y = "Portfolio Value",
           color = "Portfolio") +
      geom_smooth(method = "loess") +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::dollar)
##################################################################################
    
##################################################################################
##################################################################################
    
##################################################################################

    