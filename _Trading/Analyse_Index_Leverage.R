


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
options(width = 400)



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


df_grid = expand.grid(

    buy_leverage = seq(5, 10, by = 1), # desired leverage -> determines knockout
    holding_period = seq(30, 60, by = 5), # days to hold the option

    # TODO: Implement!
    risk_n_days_signal = 30,
    risk_n_days_signal_cutoff = seq(-0.04, -0.02, by = 0.01), # pct of days with >2% negative moves in last n days -> do not buy
    risk_n_days_signal_threshold = seq(3, 6, by = 1), # number of days with >2% negative moves in last n days -> do not buy

    # risk_n_days_signal_sma30_d1 =  -0.001, # number of days with negative sma30_deriv1 in last n days -> do not buy
    # risk_n_sma30_deriv1_threshold = seq(20, 40, by = 5)  # number of days with negative sma30_deriv1 in last n days -> do not buy
    risk_local_max_window = 10,
    risk_n_sma30_local_max_pct = seq(0.2, 0.4, by = 0.1)  # pct of days with local max in last n days -> do not buy

)

# Add metrics
df_grid$cum_revenue = NA
df_grid$max_drawdown = NA


data_train_base = data_train


for (i in 1:nrow(df_grid)) {

    print(paste0("Running grid ", i, " of ", nrow(df_grid)))

    data_train = data_train_base

    # Extract parameters
    buy_leverage = df_grid$buy_leverage[i]
    holding_period = df_grid$holding_period[i]
    risk_n_days_signal = df_grid$risk_n_days_signal[i]
    risk_n_days_signal_cutoff = df_grid$risk_n_days_signal_cutoff[i]
    risk_n_days_signal_threshold = df_grid$risk_n_days_signal_threshold[i]
    # risk_n_days_signal_sma30_d1 = df_grid$risk_n_days_signal_sma30_d1[i]
    # risk_n_sma30_deriv1_threshold = df_grid$risk_n_sma30_deriv1_threshold[i]
    risk_n_sma30_local_max_pct = df_grid$risk_n_sma30_local_max_pct[i]
    window_local_extrema = df_grid$risk_local_max_window[i]
    

    # TODO: For each model -> create an indicator whether to use it or not 
    # -> Just in case not using the risk model at all might yield consitently higher returns as we do not miss out on good buying opportunities!


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
        filter(daily_return <= risk_n_days_signal_cutoff) %>%
        mutate(risk_signal = 1) %>%
        select(symbol, date, risk_signal)

    # Count number of risk signals in last 30 days
    data_train = data_train %>%
        left_join(tmp_risk_signal, by = c("symbol", "date")) %>%
        # count number of risk_signals in last 30 days
        mutate(risk_signal_30d = zoo::rollapply(risk_signal, width = risk_n_days_signal, FUN = function(x) sum(x, na.rm = TRUE), align = "right", fill = NA, partial = T))
    
    # If there are more than n risk signals -> do not buy
    data_train = data_train %>%
        mutate(buy_n = ifelse(risk_signal_30d >= risk_n_days_signal_threshold, 0, buy_n)) %>%
        mutate(buy_value = buy_n * buy_price)


    # If sma30_deriv1 is negative -> do not buy!!!
    # Apply a fit! a single day negative derivatve is not enough -> but a longer trend is needed
    # -> use rolling window to determine number of days with negative derivatve in last 30

    # data_train = data_train %>%
    #     arrange(date) %>%
    #     mutate(sma30 = zoo::rollapply(close, width = 30, FUN = mean, align = "right", fill = NA))

    # # -> First + second order derivates of sma 30
    # data_train = data_train %>%
    #     arrange(date) %>%
    #     # mutate(sma30 = zoo::rollapply(close, width = 50, FUN = mean, align = "right", fill = NA)) %>%
    #     mutate(sma30_deriv1 = c(NA, diff(log(sma30)))) %>%
    #     mutate(sma30_deriv2 = c(NA, diff(sma30_deriv1)))


    # tmp_risk_signal_sma30_d1 = data_train %>%
    #     filter(sma30_deriv1 <= risk_n_days_signal_sma30_d1) %>%
    #     mutate(risk_signal_sma30_d1 = 1) %>%
    #     select(symbol, date, risk_signal_sma30_d1)

    # data_train = data_train %>%
    #     left_join(tmp_risk_signal_sma30_d1, by = c("symbol", "date")) %>%
    #     mutate(risk_signal_sma30_d1 = zoo::rollapply(risk_signal_sma30_d1, width = 30, FUN = function(x) sum(x, na.rm = TRUE), align = "right", fill = NA, partial = T))

    # !!!TODO: You can do that better!!!
    # -> fit a polynomial of second degree! if it is a high point for sure -> do not buy
    # -> if it is a low point -> buy!


    # If there are more than n risk signals -> do not buy
    # data_train = data_train %>%
    #     mutate(buy_n = ifelse(risk_signal_sma30_d1 >= risk_n_sma30_deriv1_threshold, 0, buy_n)) %>%
    #     mutate(buy_value = buy_n * buy_price)

    # Identify local high and low points in sma30
    # Fit a quadratic function to a rolling window of 7 days 

    data_train = data_train %>%
        arrange(date) %>%
        mutate(sma30 = zoo::rollapply(close, width = 30, FUN = mean, align = "right", fill = NA)) %>%
        # Identify local high and low points in sma30
        # Fit a quadratic function to a rolling window of 7 days
        mutate(sma30_local_max = zoo::rollapply(sma30, width = window_local_extrema, FUN = function(x) {
            if (length(na.omit(x)) < window_local_extrema) {
                return(NA)
            }
            fit = lm(x ~ poly(1:window_local_extrema, 2, raw = TRUE))
            coef = coefficients(fit)
            # Check if the quadratic term is negative (indicating a local max)
            if (coef[3] < -0.1) {
                # Calculate the vertex of the parabola
                vertex = -coef[2] / (2 * coef[3])
                # Check if the vertex is within the window
                if (vertex > 1 && vertex < window_local_extrema) {
                    return(1)
                } else {
                    return(0)
                }
            } else {
                return(0)
            }
        }, align = "right", fill = NA)) %>%
        mutate(sma30_local_min = zoo::rollapply(sma30, width = window_local_extrema, FUN = function(x) {
            if (length(na.omit(x)) < window_local_extrema) {
                return(NA)
            }
            fit = lm(x ~ poly(1:window_local_extrema, 2, raw = TRUE))
            coef = coefficients(fit)
            # Check if the quadratic term is positive (indicating a local min)
            if (coef[3] > 0.1) {
                # Calculate the vertex of the parabola
                vertex = -coef[2] / (2 * coef[3])
                # Check if the vertex is within the window
                if (vertex > 1 && vertex < window_local_extrema) {
                    return(1)
                } else {
                    return(0)
                }
            } else {
                return(0)
            }
        }, align = "right", fill = NA))


    data_train = data_train %>%
        mutate(sma30_local_max_pct = zoo::rollapply(sma30_local_max == 1, width = window_local_extrema, FUN = function(x) mean(x, na.rm = TRUE), align = "right", fill = NA)) %>%
        mutate(sma30_local_min_pct = zoo::rollapply(sma30_local_min == 1, width = window_local_extrema, FUN = function(x) mean(x, na.rm = TRUE), align = "right", fill = NA))


    # If there are more than n risk signals -> do not buy
    data_train = data_train %>%
        mutate(buy_n = ifelse(sma30_local_max_pct >= risk_n_sma30_local_max_pct, 0, buy_n)) %>%
        mutate(buy_value = buy_n * buy_price)



    # Update sell-date if risk model triggers -> i.e. if pct_large_neg_moves_30d > 0.2 -> then sell all positions
    # On the date of -2% move -> sell all potisions -> meaning, all sell_dates which are -2% + holding_period -> sell at that date

    # This will be inefficient, as I need to loop over all rows -> but should be ok for now
    data_train$sell_date_risk = as.Date(NA)

    for (j in 1:(nrow(data_train) - 30)) {

        tmp_i = data_train[j:(j + 30),]

        # Check all relevant risk models for first stop loss signal!
        min_date_i = min(tmp_i[tmp_i$risk_signal_30d >= risk_n_days_signal_threshold | tmp_i$sma30_local_max_pct >= risk_n_sma30_local_max_pct,]$date)

        data_train[j,"sell_date_risk"] = if_else(is.na(min_date_i), as.Date(NA), as.Date(min_date_i))

    }




    # TODO: Identify stop loss! to identify losses from holding period vs stop loss

    # Sell options after holding period
    data_train = data_train %>% 
        mutate(sell_date = date + holding_period) %>%
        # Sell earlier if risk model triggers
        mutate(sell_date = if_else(!is.na(sell_date_risk) & sell_date_risk < sell_date, sell_date_risk, sell_date)) %>%
        left_join(data_train %>% select(date, sell_close = close), by = c("sell_date" = "date")) %>%
        # Carry forward na values for sell_close
        fill(sell_close, .direction = "down") %>%
        fill(sell_close, .direction = "up") %>%
        rowwise() %>%
        mutate(sell_price = calc_BarrierOption(x = sell_close, strike = knockout, maturity = 99, barrier = knockout)) %>%
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

    # Calculate max drawdown
    data_train = data_train %>%
        mutate(cum_revenue_max = cummax(cum_revenue)) %>%
        mutate(drawdown = cum_revenue - cum_revenue_max) 


    # Collect metrics

    df_grid[i, ]$cum_revenue = tail(data_train$cum_revenue, 1)
    df_grid[i, ]$max_drawdown = min(data_train$drawdown, na.rm = TRUE)


}


stop("continue here")
# Measure how much cash do I need to back up the strategy
# -> i.e. given prolonged periods of losses -> what is the max drawdown I need to cover




# Check distributions of cum_revenue vs max_drawdown!
# -> interpolate grid search to find optimal parameters!
# ????#


df_grid %>% 
    tail()




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
    scale_y_continuous(labels = scales::dollar_format(prefix = "€")) +
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


# I lose a lot when the moment changed
# -> buying at overheated market -> trend changes -> long loss periods! 
# -> Does not need to have high volas! It's just a gradual return to long-term trend!

# -> sma 200?




# Identify early warning signs for loss periods
# -> high vola periods? -> high % of days with >4% price changes in last
# data_train %>%
#     arrange(date) %>%
#     mutate(daily_return = (close - lag(close)) / lag(close)) %>%
#     mutate(vola_30d = zoo::rollapply(daily_return, width = 30, FUN = sd, align = "right", fill = NA)) %>%
#     mutate(pct_large_neg_moves_30d = zoo::rollapply(daily_return <= -0.02, width = 30, FUN = mean, align = "right", fill = NA)) %>%
#     filter(date >= "2011-01-08") %>%

#     print(n = 100)




# Identify significant high points in sma30

# Add close as % of sma50 to the plot as secondary axis
data_train %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = close), color = "blue") +
    geom_line(aes(y = sma30), color = "red") +
    # geom_line(aes(y = close_sma50_pct * max(close)), color = "green") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(title = "Index Closing Price with Highlighted Loss Periods",
         x = "Date",
         y = "Closing Price") +
    theme_minimal() +
    geom_rect(data = tmp_loss_periods, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
              fill = "orange", alpha = 0.2, inherit.aes = FALSE)
    # # highligh tperiods where close_sma50_pc > 1.05
    # geom_rect(data = data_train %>% filter(between(sma30_deriv1, 0, 0.0003)), aes(xmin = date - 0.5, xmax = date + 0.5, ymin = -Inf, ymax = Inf),
    #           fill = "purple", alpha = 0.5, inherit.aes = FALSE)
    # highligh tperiods where close_sma50_pc > 1.05
    # geom_rect(data = data_train %>% filter(between(sma50_deriv2, 0, 0.00005)), aes(xmin = date - 0.5, xmax = date + 0.5, ymin = -Inf, ymax = Inf),
    #           fill = "darkgreen", alpha = 0.1, inherit.aes = FALSE)






# How to identify high and low points?
# -> I want to identify periods right before a trend change
# -> i.e. when we are certain to have passed a local high point, or low point

window_local_extrema = 10

df_plt = data_train %>%
    arrange(date) %>%
    mutate(sma30 = zoo::rollapply(close, width = 30, FUN = mean, align = "right", fill = NA)) %>%
    # Identify local high and low points in sma30
    # Fit a quadratic function to a rolling window of 7 days
    mutate(sma30_local_max = zoo::rollapply(sma30, width = window_local_extrema, FUN = function(x) {
        if (length(na.omit(x)) < window_local_extrema) {
            return(NA)
        }
        fit = lm(x ~ poly(1:window_local_extrema, 2, raw = TRUE))
        coef = coefficients(fit)
        # Check if the quadratic term is negative (indicating a local max)
        if (coef[3] < -0.1) {
            # Calculate the vertex of the parabola
            vertex = -coef[2] / (2 * coef[3])
            # Check if the vertex is within the window
            if (vertex > 1 && vertex < window_local_extrema) {
                return(1)
            } else {
                return(0)
            }
        } else {
            return(0)
        }
    }, align = "right", fill = NA)) %>%
    mutate(sma30_local_min = zoo::rollapply(sma30, width = window_local_extrema, FUN = function(x) {
        if (length(na.omit(x)) < window_local_extrema) {
            return(NA)
        }
        fit = lm(x ~ poly(1:window_local_extrema, 2, raw = TRUE))
        coef = coefficients(fit)
        # Check if the quadratic term is positive (indicating a local min)
        if (coef[3] > 0.1) {
            # Calculate the vertex of the parabola
            vertex = -coef[2] / (2 * coef[3])
            # Check if the vertex is within the window
            if (vertex > 1 && vertex < window_local_extrema) {
                return(1)
            } else {
                return(0)
            }
        } else {
            return(0)
        }
    }, align = "right", fill = NA))

# Calculate rolling percentage of sma_local_max
df_plt = df_plt %>%
    mutate(sma30_local_max_pct = zoo::rollapply(sma30_local_max == 1, width = window_local_extrema, FUN = function(x) mean(x, na.rm = TRUE), align = "right", fill = NA)) %>%
    mutate(sma30_local_min_pct = zoo::rollapply(sma30_local_min == 1, width = window_local_extrema, FUN = function(x) mean(x, na.rm = TRUE), align = "right", fill = NA))


ggplot(df_plt, aes(x = date)) +
    geom_line(aes(y = sma30), color = "blue") +
    # geom_point(data = filter(df_plt, sma30_local_max == 1), aes(y = sma30), color = "red", size = 2) +
    # geom_point(data = filter(df_plt, sma30_local_min == 1), aes(y = sma30), color = "green", size = 2) +
    geom_point(data = filter(df_plt, sma30_local_max_pct >= .3), aes(y = sma30), color = "red", size = 2) +
    geom_point(data = filter(df_plt, sma30_local_min_pct >= .3), aes(y = sma30), color = "green", size = 2) 

