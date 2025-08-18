
# Analyse options portfolio

# Keep options longer


# Definition of strategy:

# If price > sma 200 -> then buy long barrier options with barrier = sma 200
# if price < sma 200 -> liquidate all options and go short

# DO not sell options unless necessary! Barrier options are open-end and can be held as long as possible!

# Determine optimal entry and exit signals
# Entry: 
# 1] Price is x days above sma 200
# 2] Price is x days above sma N and has significant positive slope and momentum

# Exit:
# 1] Price is x days below sma 200 (close vs low)
# 2] Price has significant negative slope and momentum
# 3] Define stop-losses!



# BETTER! Train several models with xgboost on:
# 1] Price changes -> predict optimal buy / sell for next quarters
# -> this model will decide the allocation of the portfolio

# 2] Volatility -> predict volatility by estimateing low vs high model 
# -> this will be the model to determine the VaR of the portfolio
# -> And decide when to hedge or liquidate positions

# 




# Technical requirements:
# 1] Track the portfolio! And liquidate if exit signals are met!




# NOTES:

# Need a better exit signal! Most trades turn a loss as the exit is too late!
# Also, too many knockouts! Why?
# -> could be a coding error!
# -> track date of knockout!!!!









library(tidyquant)
library(tidyverse)
library(ggplot2)

library(RQuantLib)
library(greeks)

library(TTR)

library(tidymodels)
library(timetk)
library(xgboost)
# library(fable)
# library(fabletools)




# Need to train on several indices to avoid overfitting!
idx_list = c(
    "^NDX", # Nasdaq 100 #> profitable

    # "EUR=X" # NOT profitable.

    # "GC=F"

    "^DJI", # Dow Jones
    "^GSPC", # SP 500
    "^GDAXI" # DAX

    # NIKKEI? others?

    # Bitcoin? Gold?
    # -> screen consors for possible index underlyings!

    )



# Analyze next day price changes

# Get price data from yahoo finance
df = tq_get(idx_list, from = "2000-01-01", to = Sys.Date() + 1)


# Calculate 30 day moving average on close
df = df %>%
    drop_na("close") %>%
    group_by(symbol) %>%
    arrange(date, .by_group = T) %>%
    mutate(
        # close = close / head(close, n = 1),
        sma_200_close = TTR::SMA(close, n = 30),
        sma_200_low = TTR::SMA(low, n = 100)
    ) 

tail(df)

# Subset to train and testing
df_train = df %>% filter(date < as.Date("2018-01-01"))
df_test = df %>% filter(date > as.Date("2018-01-01"))




# Develop backtesting framework

df_orders = df_train %>% 
    mutate(

        # Enter positions
        buy_long = if_else(open > sma_200_close, 1, 0),
        buy_short = if_else(open < sma_200_close, 1, 0),

        # Liquidate positions
        liquidate_long = if_else(close < sma_200_close, 1, 0),
        liquidate_short = if_else(close > sma_200_close, 1, 0)

    )

# Infer portfolio
df_portfolio = tibble()

# Long positions
df_portfolio_long = df_orders %>% 
        filter(buy_long == 1) %>%
        select(date_buy = date, close_buy = close, barrier = sma_200_close) %>%
        mutate(type = "long")

# Short positions
df_portfolio_short = df_orders %>% 
        filter(buy_short == 1) %>%
        select(date_buy = date, close_buy = close, barrier = sma_200_close) %>%
        mutate(type = "short")



# For each position, determine if first liquidate condition is met or barrier is hit!

# Dates for liquidation of long positions
date_liquidate_long = df_orders %>% filter(liquidate_long == 1) %>% pull(date)

df_portfolio_long["date_liquidate"] = NA
df_portfolio_long["barrier_crit"] = NA

for (i in 1:nrow(df_portfolio_long)) {

    date_buy_i = df_portfolio_long[i,]$date_buy

    date_liquidate_i = min(date_liquidate_long[date_liquidate_long > date_buy_i])

    low_i = df_train %>%
        filter(date >= date_buy_i & date < date_liquidate_i) %>%
        pull(low) %>% min()

    df_portfolio_long[i, "date_liquidate"] = date_liquidate_i
    df_portfolio_long[i, "barrier_crit"] = low_i

}

date_liquidate_short = df_orders %>% filter(liquidate_short == 1) %>% pull(date)

df_portfolio_short["date_liquidate"] = NA
df_portfolio_short["barrier_crit"] = NA

for (i in 1:nrow(df_portfolio_short)) {

    date_buy_i = df_portfolio_short[i,]$date_buy

    date_liquidate_i = min(date_liquidate_short[date_liquidate_short > date_buy_i])

    high_i = df_train %>%
        filter(date >= date_buy_i & date < date_liquidate_i) %>%
        pull(high) %>% min()

    df_portfolio_short[i, "date_liquidate"] = date_liquidate_i
    df_portfolio_short[i, "barrier_crit"] = high_i

}

# Determine if barrier condition is met
df_portfolio_long = df_portfolio_long %>%
    mutate(knocked_out = if_else(barrier_crit < barrier, 1, 0))

df_portfolio_short = df_portfolio_short %>%
    mutate(knocked_out = if_else(barrier_crit > barrier, 1, 0))


# Calculate holding period
df_portfolio_long = df_portfolio_long %>% mutate(holding_period = date_liquidate - date_buy)
df_portfolio_short = df_portfolio_short %>% mutate(holding_period = date_liquidate - date_buy)



# Calculate profit and loss


df_pl_long = df_portfolio_long %>%
    # Add close price at liquidation date
    left_join(df_train %>% select(date, close_sell = close), by = c("date_liquidate" = "date")) %>%
    # Calculate profit
    mutate(
        cost = -100,
        rev_pct = 1 + (close_sell - close_buy) / close_buy,
        rev_pct = if_else(knocked_out == 1, 0, rev_pct), # If knocked out, no profit
        profit = cost + (rev_pct * 100) # 100 shares
    ) 

df_train %>%
    left_join(df_pl_long, by = c("date" = "date_buy")) %>%
    mutate(profit = if_else(is.na(profit), 0, profit)) %>%
    mutate(profit_cum = cumsum(profit)) %>%
    View()
    ggplot(aes(x = date, y = profit_cum)) +
    geom_line()




# Calculate % profitable trades, knockouts and unprofitable trades
df_pl_long %>%
    mutate(case = case_when(
        knocked_out == 1 ~ "knockout",
        rev_pct > 1 ~ "profitable",
        rev_pct < 1 ~ "loss",
        TRUE ~ "open"
    )) %>%
    #count(case)
    left_join(df_train %>% select(date, close, low, sma_200_close), .,  by = c("date" = "date_buy")) %>%
    filter(year(date) == 2013) %>%
    ggplot(aes(x = date, y = close)) + 
    geom_line() +
    geom_line(aes(y = low), color = "blue", linetype = 2) +
    geom_line(aes(y = sma_200_close), color = "red") +
    geom_jitter(aes(x = date_liquidate, y = close_buy, color = case), size = 3, na.rm = T)

df_pl_long %>%
 filter(year(date_buy) == 2013) %>%
 filter()
 filter(knocked_out == 1) %>%
 head()







