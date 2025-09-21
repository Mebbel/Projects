


# Notes !!!!!!


# Buy github copilot!!!



# Determine optimal leverage (i.e. knockout / price) for rolling options
# i.e. I want to be invested with 10k € in the Nasdaq 100 with a limited VaR
# How do I need to choose the knockout?

# When to buy sell?
# -> % of +/-4% price changes in the last 30 days -> signla for high vola -> do not buy but sell!
# ---> Put in relation to index vola -> higher vola = higher expected price changes! -> compute with rolling window? self-revealing index info?

# When to go short / long?
# If high value -> then exit all positions -> it is too risky for this strategy!!!
# -> what constitues a high value? close / sma 200? sma 200 on high / low / close?



# Keep securities for max 30 days
# -> determine probability of X days of reaching n% variance -> if knockout is (1-10%) * price -> then take max 5% risk that within X days the knockout is reached

# Sell security early if underlying increased by 10% (take profit)


# How to account for intraday variance? High vs low prices


# Metrics
# Check for consistency of profits -> categorize by origin (early sell, end of holding period, ....)
# -> We do not want a strategy that generates profity only by chance


# Backtesting
# Test on a few indices -> select worldwide


# !Monte carlo simulation!!!
# -> vary assumptions + prices 
# -> create random noise on indices
# -> Create random indices by combining different indices? or changing the order of returns?


# Settings -------------------------------------------------------------------------------

# Set console width
options(width = 250)



# Libraries -------------------------------------------------------------------------

library(tidyquant)
library(tidyverse)
library(ggplot2)

library(RQuantLib)
library(greeks)


# Select index and time period for training vs testing
# -> validation on other indices!


# download data for list of indics
l_indices = c("^NDX", "^GSPC", "^RUT", "^FTSE", "^GDAXI", "^FCHI", "^N225", "^HSI", "^SSEC")

l_data = list()

for (i in 1:length(l_indices)) {
    l_data[[i]] = tq_get(l_indices[i], from = "2010-01-01", to = "2024-06-01", get = "stock.prices")
    Sys.sleep(1)
}

names(l_data) = l_indices



calc_BarrierOption <- function(x, strike, maturity, barrier) {

    if (x < barrier) {
        return(0)
    } else {

    val = BarrierOption(
                barrType = "downout",
                type = "call", 
                underlying = x,
                strike = strike,
                dividendYield = 0,
                riskFreeRate = 0.029190,
                maturity = maturity,
                volatility = 0.198,
                barrier = barrier
            )

    return(val$value)
    }
}

# Calculate expected leverage of a barrier option
val_0 = calc_BarrierOption(x = 21711, strike = 20748, maturity = 99, barrier = 20748)
val_1 = calc_BarrierOption(x = 21711 * 1.01, strike = 20748, maturity = 99, barrier = 20748)

# Expected increase of the price if the underlying increases by 1% -> leverage
leverage = (val_1 / val_0 - 1) / 0.01





# Select single training data set
data = l_data[["^NDX"]]

# Subset to first 5 years
data_train = data %>% filter(date < "2015-01-01")
data_test = data %>% filter(date >= "2015-01-01")   


# Settings -> this will be a grid-search later on

buy_leverage = 5 # desired leverage -> determines knockout
holding_period = 30 # days to hold the option

# TODO: Implement!
risk_n_days_signal = 30
risk_n_days_signal_threshold = 3 # number of days with >2% negative moves in last n days -> do not buy



# Build infrastructure -> buy 1 option every day if conditions are met

data_train = data_train %>% 
    # Determine desired knockout level
    mutate(knockout = (1 - 1 / buy_leverage) * close) %>%
    # Calculate price of the option
    rowwise() %>%
    mutate(buy_price = calc_BarrierOption(x = close, strike = knockout, maturity = 99, barrier = knockout)) %>%
    ungroup()

# Buy n options every day
data_train = data_train %>%
    mutate(buy_n = floor(10000 / buy_price)) %>%
    mutate(buy_value = buy_n * buy_price)


# Risk Managemnt -> if daily change < -2% in last 30 days -> do not buy and liquidate all positions
# TODO: determine relative to index vola -> i.e. quantile of daily changes -> take 1% or something
tmp_risk_signal = data_train %>%
    arrange(date) %>%
    mutate(daily_return = (close - lag(close)) / lag(close)) %>%
    filter(daily_return <= -0.02) %>%
    mutate(risk_signal = 1) %>%
    select(symbol, date, risk_signal)

# Count number of risk signals in last 30 days
data_train = data_train %>%
    left_join(tmp_risk_signal, by = c("symbol", "date")) %>%
    # count number of risk_signals in last 30 days
    mutate(risk_signal_30d = zoo::rollapply(risk_signal, width = 30, FUN = function(x) sum(x, na.rm = TRUE), align = "right", fill = NA, partial = T))
  
# If there are more than n risk signals -> do not buy
data_train = data_train %>%
    mutate(buy_n = ifelse(risk_signal_30d >= 3, 0, buy_n)) %>%
    mutate(buy_value = buy_n * buy_price)


# Update sell-date if risk model triggers -> i.e. if pct_large_neg_moves_30d > 0.2 -> then sell all positions
# On the date of -2% move -> sell all potisions -> meaning, all sell_dates which are -2% + holding_period -> sell at that date

# This will be inefficient, as I need to loop over all rows -> but should be ok for now



# Sell options after holding period
data_train = data_train %>% 
    mutate(sell_date = date + holding_period) %>%
    left_join(data_train %>% select(date, sell_close = close), by = c("sell_date" = "date")) %>%
    # Carry forward na values for sell_close
    fill(sell_close, .direction = "down") %>%
    rowwise() %>%
    mutate(sell_price = calc_BarrierOption(x = sell_close, strike = knockout, maturity = 99 - holding_period, barrier = knockout)) %>%
    ungroup()

# Determine the lowest low price during holdin period -> and check if knockout was reached
tmp_low_roll = data_train %>%
    # Use a rolling window approach
    mutate(low_roll = zoo::rollapply(low, width = holding_period, FUN = min, align = "right", fill = NA)) %>%
    select(date, low_roll)

data_train = data_train %>%
    left_join(tmp_low_roll, by = c("sell_date" = "date")) %>%
     # Carry forward na values for sell_close
    fill(low_roll, .direction = "down")

# Determine profit
data_train = data_train %>%
    mutate(revenue = ifelse(low_roll <= knockout, -buy_value, buy_n * sell_price - buy_value))

# Calculate cumulative profit
data_train = data_train %>%
    drop_na(revenue) %>%
    mutate(cum_revenue = cumsum(revenue))

data_train %>% tail()




# Analyse sources of profit and losses

# 1] Total profit / loss when sold after holding period
# 2] Total loss when knocked out

data_train %>%
    summarise(
        total_revenue = sum(revenue),
        total_knockout_loss = sum(revenue[low_roll <= knockout]),
        total_holding_period_revenue = sum(revenue[low_roll > knockout])
    )


# Analyse entry price vs closeprice -> safety margin
data_train %>%
    mutate(safety_margin = (close - knockout) / close) %>%
    summarise(
        mean_safety_margin = mean(safety_margin),
        min_safety_margin = min(safety_margin),
        max_safety_margin = max(safety_margin)
    )

# Analyse leverage achieved
data_train %>%
    mutate(actual_leverage = (sell_price / buy_price - 1) / (sell_close / close - 1)) %>%
    filter(!is.infinite(actual_leverage) & !is.na(actual_leverage)) %>%
    summarise(
        mean_actual_leverage = mean(actual_leverage, na.rm = TRUE),
        median_actual_leverage = median(actual_leverage, na.rm = TRUE),
        min_actual_leverage = min(actual_leverage, na.rm = TRUE),
        max_actual_leverage = max(actual_leverage, na.rm = TRUE)
    )



# Visualize cumulative revenue
# TODO: add info about portfolio costs / value -> to track when i am not invested due to the risk model
data_train %>%
    ggplot(aes(x = date, y = cum_revenue)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Cumulative Revenue from Index Leverage Strategy",
         x = "Date",
         y = "Cumulative Revenue (€)") +
    theme_minimal()

# Distribution of daily revenues
data_train %>%
    ggplot(aes(x = revenue)) +
    geom_histogram(bins = 100, fill = "darkblue", color = "white", alpha = 0.7) +
    labs(title = "Distribution of Daily Revenues",
         x = "Daily Revenue (€)",
         y = "Frequency") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal()


# Analysze periode of high losses
data_train = data_train %>%
    # Identify periods with losses
    mutate(loss_flag = ifelse(revenue < -1000, 1, 0)) %>%
    # Count consecutive loss periods
    mutate(loss_period = cumsum(c(0, diff(loss_flag)) == 1))


tmp_loss_periods = data_train %>%
    filter(loss_flag == 1) %>%
    group_by(loss_period) %>%
    summarise(
        start_date = min(date),
        end_date = max(date),
        total_loss = sum(revenue),
        n_days = n()
    ) %>%
    arrange(loss_period)

# Visualize loss periods
data_train %>%
    ggplot(aes(x = date, y = revenue)) +
    geom_bar(stat = "identity", fill = ifelse(data_train$revenue < 0, "red", "green"), alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(title = "Daily Revenues with Highlighted Loss Periods",
         x = "Date",
         y = "Daily Revenue (€)") +
    theme_minimal() +
    geom_rect(data = tmp_loss_periods, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
              fill = "orange", alpha = 0.2, inherit.aes = FALSE)

# Identify early warning signs for loss periods
# -> high vola periods? -> high % of days with >4% price changes in last
data_train %>%
    arrange(date) %>%
    mutate(daily_return = (close - lag(close)) / lag(close)) %>%
    mutate(vola_30d = zoo::rollapply(daily_return, width = 30, FUN = sd, align = "right", fill = NA)) %>%
    mutate(pct_large_neg_moves_30d = zoo::rollapply(daily_return <= -0.02, width = 30, FUN = mean, align = "right", fill = NA)) %>%
    filter(date >= "2012-08-08") %>%

    print(n = 100)




