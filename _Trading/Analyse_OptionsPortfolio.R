
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






# Technical requirements:
# 1] Track the portfolio! And liquidate if exit signals are met!













# Notes 

# Determine 100 day vs 30 day
# If 30 day > 100 day -> keep buying long options
# If 30 day < 100 day -> sell long options and buy short options

# Infer relative differenfr from 30 to 100 days ->
# -> Do not buy options any longer


# Inspect moving averages on close / high / low etc.
# -> moving average on high - low, etc.




library(tidyquant)
library(tidyverse)
library(ggplot2)

library(RQuantLib)
library(greeks)

library(TTR)



idx_list = c(
    "^NDX" # Nasdaq 100 #> profitable


    # "EUR=X" # NOT profitable.

    # "GC=F"


    # "^DJI" # Dow Jones
    # "^GSPC" # SP 500
    # "^GDAXI" # DAX

    # NIKKEI? others?

    # Bitcoin? Gold?
    # -> screen consors for possible index underlyings!

    )



# Analyze next day price changes

# Get price data from yahoo finance
df = tq_get(idx_list, from = "2000-01-01", to = Sys.Date() + 1)


# Calculate 30 day moving average on close
df = df %>%
    mutate(
        # close = close / head(close, n = 1),
        close_ma_30 = TTR::SMA(close, n = 30),
        close_ma_100 = TTR::SMA(close, n = 100)
    ) 

tail(df) %>% View()


# Subset to train and testing
df_train = df %>% filter(date < as.Date("2018-01-01"))
df_test = df %>% filter(date > as.Date("2018-01-01"))


# Develop backtesting framework

# 1] Whenever close_ma_30 > close_ma_100 -> buy long options
# 2] Whenever close_ma_30 < close_ma_100 -> sell long options and buy short options




df_orders = df_train %>% 
    mutate(
        buy_long = if_else(close_ma_30 / close_ma_100 > 1.05, 1, 0),
        buy_short = if_else(close_ma_30 / close_ma_100 < 0.95, 1, 0),

        liquidate_long = if_else(close_ma_30 / close_ma_100 <= 0.99, 1, 0),
        liquidate_short = if_else(close_ma_30 / close_ma_100 >= 1.01, 1, 0)

    )

# Infer portfolio
df_portfolio = tibble()

# Long positions
df_portfolio = bind_rows(
    df_portfolio, 
    df_orders %>% 
        filter(buy_long == 1) %>%
        select(date_buy = date, close_buy = close) %>%
        mutate(type = "long")
)

# Short positions
df_portfolio = bind_rows(
    df_portfolio, 
    df_orders %>% 
        filter(buy_short == 1) %>%
        select(date_buy = date, close_buy = close) %>%
        mutate(type = "short")
)

# Assume holding the option for up to 30 days
df_portfolio = df_portfolio %>%
    mutate(
        date_sell = date_buy + 360
    )

# Add closing price at selling date
df_portfolio = df_portfolio %>%
    left_join(df_train %>% select(date, close_sell = close), by = c("date_sell" = "date")) %>%
    # Carry forward last observation of close_sell
    fill(close_sell, .direction = "down")

# Liquidate positions
# Need to check for each position in portfolio, if between date_buy and date_sell, the liquidate condition is met
# -> take the first liquidate condition!

df_liquidate_long = df_orders %>%
    filter(liquidate_long == 1) %>%
    select(date_liquidate_long = date, close_liquidate_long = close)

df_liquidate_short = df_orders %>%
    filter(liquidate_short == 1) %>%
    select(date_liquidate_short = date, close_liquidate_short = close)


df_liquidate_pf = df_portfolio %>%
    rowwise() %>%
    mutate(l_date = list(seq.Date(date_buy, date_sell, by = "days"))) %>%
    select(date_buy, date_sell, l_date) %>%
    unnest(l_date)

df_liquidate_pf_long = df_liquidate_pf %>%    
    left_join(
        df_liquidate_long, by = c("l_date" = "date_liquidate_long")
    ) %>%
    drop_na(close_liquidate_long) %>%
    group_by(date_buy) %>%
    slice_head(n = 1)

df_liquidate_pf_short = df_liquidate_pf %>%    
    left_join(
        df_liquidate_short, by = c("l_date" = "date_liquidate_short")
    ) %>%
    drop_na(close_liquidate_short) %>%
    group_by(date_buy) %>%
    slice_head(n = 1)

df_portfolio = df_portfolio %>%
    left_join(df_liquidate_pf_long %>% select(date_buy, date_liquidate_long = l_date, close_liquidate_long), by = c("date_buy" = "date_buy")) %>%
    left_join(df_liquidate_pf_short %>% select(date_buy, date_liquidate_short = l_date, close_liquidate_short), by = c("date_buy" = "date_buy"))



# TODO: Track cash management! Can only buy if I have cash!
# -> Without cash management, cannot compare to simple buy-and-hold strategy!

cash_init = 10000


# Compare to base line
# Buy shares everyday and hold until today
df_base = df_train %>%
    slice_head(n = 1) %>%
    select(date_buy = date, close_buy = close) %>%
    mutate(n = cash_init / close_buy) 

# Sell at the end -> until then, track the NAV
df_base =
    df_train %>%
    bind_cols(df_base) %>%
        mutate(
            pl_cum = n * (close - close_buy)
        )


# Calculate levered vs unlevered portfolio!
# European vs barrier option!

# Simple long-only unlevered portfolio

df_portfolio %>%
    filter(type == "long") %>%
    arrange(date_buy) %>%
    mutate(
        # Calculate profit / loss
        pl = case_when(
            type == "long" & is.na(date_liquidate_long) ~ close_sell - close_buy,
            type == "long" & !is.na(date_liquidate_long) ~ close_liquidate_long - close_buy,
            type == "short" ~ 0
        )
    ) %>%
    mutate(pl_cum = cumsum(pl)) %>%  
    ggplot(aes(x = date_buy, y = pl_cum)) +
        geom_line() +
        geom_line(data = df_base, aes(x = date), color = "red") +
        theme_minimal() +
        geom_hline(yintercept = 0, color = "black") +
        labs(title = "Cumulative profit / loss of long-only portfolio") +
        scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))






df_orders %>%
filter(date>= "2002-01-08") %>% View()



# Testing!!!! -------------------------------------------------------------------------------------------------------




df %>%
filter(date > as.Date("2018-01-01")) %>%
ggplot(aes(x = date, y = close)) +
    geom_line() + 
    geom_line(aes(y = close_ma_30), color = "red") +
    geom_line(aes(y = close_ma_100), color = "blue") +
    theme_minimal() +
    labs(title = "30 day vs 100 day moving average")



