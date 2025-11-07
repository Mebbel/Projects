


# Notes !!!!!!


# Buy github copilot!!!



# Determine optimal leverage (i.e. knockout / price) for rolling options
# i.e. I want to be invested with 10k € in the Nasdaq 100 with a limited VaR
# How do I need to choose the knockout?

# When to buy sell?
# -> % of +/-4% price changes in the last 30 days -> signla for high vola -> do not buy but sell!
# ---> Put in relation to index vola -> higher vola = higher expected price changes! -> compute with rolling window? self-revealing index info?

# On first knockout, or second knockout within n days -> liquiditate all long positions!

# When to go short / long?
# If high value -> then exit all positions -> it is too risky for this strategy!!!
# -> what constitues a high value? close / sma 200? sma 200 on high / low / close?
# -> Can i perform a regression on (expected return vs close / sma200) to determine optimal long/cash/short weights?

# -> Wenn long knockout - go short

# Keep securities for max 30 days
# -> determine probability of X days of reaching n% variance -> if knockout is (1-10%) * price -> then take max 5% risk that within X days the knockout is reached

# Sell security early if underlying increased by 10% (take profit)


# Min x% portfolio = short




# How to account for intraday variance? High vs low prices


# Metrics
# Check for consistency of profits -> categorize by origin (early sell, end of holding period, ....)
# -> We do not want a strategy that generates profity only by chance and only in extreme market conditions!


# Backtesting
# Test on a few indices -> select worldwide


# !Monte carlo simulation!!!
# -> vary assumptions + prices 
# -> create random noise on indices
# -> Create random indices by combining different indices? or changing the order of returns?


# Settings -------------------------------------------------------------------------------

# Set console width
options(width = 400)



# Portfolio settings
liquidity_init = 10000





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
data = l_data[["^NDX"]] # ^RUT


# TODO: Cross-validation! Create several moving windows for training and testing!
# -> NEVER mix time series! -> define legnth of train vs test window -> and shift n times!


# Subset to first 5 years
df_train = data %>% filter(date < "2015-01-01")
df_test = data %>% filter(date >= "2015-01-01")   


# Recode to matrices for faster processing
df_train$date = 1:nrow(df_train)
df_train$symbol = NULL

df_test$date = 1:nrow(df_test)
df_test$symbol = NULL

m_train = as.matrix(df_train)
m_test = as.matrix(df_test)

stop("Implement cross-validation! with moving windows of n (=5?) years and evaluate on next 2 years!")
# Collect metrics for training and test for each window!


# Settings -> this will be a grid-search later on

grid_settings = list(
    buy_leverage = c(5, 10),
    holding_period = c(30, 100),

    liquidity_inv_perc = c(0.05, 0.2), # Percent of available liquidity to invest daily

    # Observe increased clustered vola
    risk_n_days_signal_TF = c(1), #c(TRUE, FALSE), # whether to use the risk signal or not
    risk_n_days_signal = c(10, 30),
    risk_n_days_signal_cutoff = c(-0.03, -0.015), # negative < x% moves
    risk_n_days_signal_threshold = c(2, 10) # number of days with <x% negative moves in last n days -> do not buy

    )

# Check number of elements with two values
grid_n_expand = lapply(grid_settings, function(x) length(x) > 1) %>% unlist() %>% sum()

# Define length of grid
grid_length = 500

# Given the grid_length and the number of elements to expand -> define how many values to sample for each parameter
grid_n_samples = floor(grid_length^(1 / grid_n_expand))

grid_expand = lapply(grid_settings, function(x) if (length(x) > 1) seq(min(x), max(x), length.out = grid_n_samples) else x)

df_grid = expand.grid(grid_expand)

# Transform day values to integers
df_grid = df_grid %>%
    mutate(across(c(holding_period, risk_n_days_signal, risk_n_days_signal_threshold), as.integer))




# # Basic grid -> independent of T/F settings
# df_grid = expand.grid(

#     # General settings
#     buy_leverage = 5, #seq(3, 9, by = 1), # desired leverage -> determines knockout
#     holding_period = seq(50, 100, by = 10), # days to hold the option
#     # liquidity_inv_perc = 0.05, # Percent of available liquidity to invest daily

    

#     # Observe increased clustered vola
#     risk_n_days_signal_TF = TRUE, #c(TRUE, FALSE), # whether to use the risk signal or not
#     risk_n_days_signal = seq(10, 30, by = 10),
#     risk_n_days_signal_cutoff = seq(-0.04, -0.02, by = 0.01), # pct of days with >2% negative moves in last n days -> do not buy
#     risk_n_days_signal_threshold = seq(2, 10, by = 2) # number of days with >2% negative moves in last n days -> do not buy

#     # Observe potential trend changes -> reduce revenues significantly
#     #risk_local_max_TF = c(TRUE, FALSE), # whether to use the risk signal or not
#     #risk_local_max_window = 10,
#     #risk_local_max_sma30 = seq(0.2, 0.3, by = 0.1)  # pct of days with local max in last n days -> do not buy


# )

# Create stochastic grid! i.e. define length of grid and interpolate each parameter given the defined range


# Remove unncessary iterations
df_grid = bind_rows(
    df_grid %>% filter(risk_n_days_signal_TF == 1),
    df_grid %>% 
        filter(risk_n_days_signal_TF == 0) %>%
        group_by(across(-starts_with("risk_n_days_signal"))) %>%
        slice_head(n = 1)
)

# Add metrics

df_grid$liquidity_final = NA



m_grid = as.matrix(df_grid)


# Print size of grid
print(paste0("Grid size: ", nrow(m_grid)))



# Define objects for backtesting -------------------------------------------------


## Portfolio Positions ------------------------------------------------------------

# Holds all portfolio positions

m_positions = matrix(
    0,
    nrow = nrow(data_train) * 2, # Allow for both long + short position each day
    ncol = 14
)


colnames(m_positions) = c("buy_date", "buy_price", "buy_n", "buy_value", "knockout", "type", "sell_date", "sell_price", "sell_value", "sell_n", "default", "status", "profit", "return")


## Portfolio Liquidity ------------------------------------------------------------

# Track the liquidity management, incl. current cash, total invested, portfolio value, total value

m_portfolio = matrix(
    0,
    nrow = nrow(data_train) + 1,
    ncol = 7
)

# bop = beginning of period, eop = end of period

colnames(m_portfolio) = c("date", "cash_bop", "cash_eop", "invested_value", "portfolio_value", "total_value", "period_revenue")




## Date vector ------------------------------------------------------------

# Iterator
v_date = m_train[,"date"]





## Add init values -------------------------------------------------------------
m_portfolio[,"date"] = c(0, v_date)

m_portfolio[1, "cash_bop"] = liquidity_init



for (i in 1:nrow(m_grid)) {

    print(paste0("Running grid ", i, " of ", nrow(m_grid)))

    # Extract parameters
    buy_leverage = m_grid[i, "buy_leverage"]
    holding_period = m_grid[i, "holding_period"]
    liquidity_inv_perc = m_grid[i, "liquidity_inv_perc"]

    risk_n_days_signal_TF = m_grid[i, "risk_n_days_signal_TF"]
    risk_n_days_signal = m_grid[i, "risk_n_days_signal"]
    risk_n_days_signal_cutoff = m_grid[i, "risk_n_days_signal_cutoff"]
    risk_n_days_signal_threshold = m_grid[i, "risk_n_days_signal_threshold"]
    # risk_n_days_signal_sma30_d1 = m_grid[i, "risk_n_days_signal_sma30_d1"]
    # risk_n_sma30_deriv1_threshold = m_grid[i, "risk_n_sma30_deriv1_threshold"]
    # risk_local_max_TF = m_grid[i, "risk_local_max_TF"]
    # risk_local_max_sma30 = m_grid[i, "risk_local_max_sma30"]
    # window_local_extrema = m_grid[i, "window_local_extrema"]


    # Calculate liquidity inv perc in relation to holding period + 10 days buffer
    # liquidity_inv_perc = 1 / (holding_period + 10)


    # Perform vectorized calculations ------------------------------------------------------------

    ## Daily diff in close prices
    m_train = cbind(m_train, d_close = c(NA, (m_train[2:nrow(m_train),"close"] / m_train[,"close"][-nrow(m_train)]-1)))


    # Iterables ------------------------------------------------------------

    # Portfolio positions
    p = 1


    # Iterate over training data

    for (j in v_date) {


        # Set cash at beginning of period ----------------------------------------------------------------------------------------------------------------------------

        if (j > 1) {
            m_portfolio[j,"cash_bop"] = m_portfolio[j - 1,"cash_eop"]
        }
        



        # Risk models ----------------------------------------------------------------------------------------------------------------------------

    
       
        # Risk Managemnt -> if daily change < -2% in last 30 days -> do not buy and liquidate all positions
        # count number of risk_signals in last 30 days

        # Make sure to observe only changes from end of previous period. Close price of period j is not known at time of decision making!
        count_n_days_signal_cutoff = length(which(m_train[pmax(1, j - risk_n_days_signal):pmax(1, (j - 1)), "d_close"] <= risk_n_days_signal_cutoff))


        # If this risk model is enabled
        if (risk_n_days_signal_TF == 1) {

            # Sell all open positions if threshold is exceeded

            if (count_n_days_signal_cutoff >= risk_n_days_signal_threshold) {

                m_positions[m_positions[,"status"] == 1 & m_positions[,"buy_date"] <= j, "sell_date"] = j

            }

        }



        # Buy Positions ----------------------------------------------------------------------------------------------------------------------------

        # Only buy of there is no risk signals

        if (risk_n_days_signal_TF == 1 & count_n_days_signal_cutoff >= risk_n_days_signal_threshold) {
            # Do not buy
        } else {
            # Buy as usual

            # Determine how much to buy in current period
            buy_value_j = m_portfolio[j, "cash_bop"] * liquidity_inv_perc

            # Determine knockout for given leverage and close
            knockout_j = (1 - 1 / buy_leverage) * m_train[j,"close"]

            # Determine buy price
            buy_price_j = calc_BarrierOption(x = m_train[j,"close"], strike = knockout_j, maturity = 99, barrier = knockout_j) / 10^3

            # Buy position
            m_positions[p,"buy_date"] = j
            m_positions[p,"buy_price"] = buy_price_j
            m_positions[p,"knockout"] =knockout_j
            m_positions[p,"status"] = 1

            # Buy as usual
            m_positions[p, "buy_n"] = floor(buy_value_j / m_positions[p,"buy_price"])

            # Calculate value
            m_positions[p, "buy_value"] = m_positions[p,"buy_n"] * m_positions[p,"buy_price"]

            # Increment position iterator
            p = p + 1




        }


        # Check knockout of positions ----------------------------------------------------------------------------------------------------------------------------

        # For all active positions, check if they are knocked out in current period j
        # Assume that I am not lucky enough to sell them in period j before knockout happens!
        # Set default = 1 if knocked out
        m_positions[m_positions[,"status"] == 1 & m_positions[,"buy_date"] <= j & m_train[j, "low"] <= m_positions[,"knockout"], "default"] = 1

        # Set status of all defaulted positions to 0
        m_positions[m_positions[,"default"] == 1 & m_positions[,"status"] == 1, "status"] = 0



        # Sell due to risk models ----------------------------------------------------------------------------------------------------------------------------

        if (risk_n_days_signal_TF == 1 & count_n_days_signal_cutoff >= risk_n_days_signal_threshold) {

            # Sell all open positions
            m_positions[m_positions[,"status"] == 1 & m_positions[,"buy_date"] <= j, "sell_date"] = j

            # Sell at lowest price of the day
            for (s in which(m_positions[,"sell_date"] == j & m_positions[,"status"] == 1)) {

                # Get knockout from buy date
                m_positions[s, "sell_price"] = calc_BarrierOption(x =  m_train[j, "low"], strike = m_positions[s,"knockout"], maturity = 99, barrier = m_positions[s,"knockout"]) / 10^3

                m_positions[s,"sell_n"] = m_positions[s,"buy_n"]
                m_positions[s,"default"] = 0
                m_positions[s,"status"] = 0

                m_positions[s, "sell_value"] = m_positions[s,"sell_n"] * m_positions[s,"sell_price"]

                m_positions[s, "profit"] = m_positions[s,"sell_value"] - m_positions[s,"buy_value"]


            }         


        }




        # Sell due to holding period ----------------------------------------------------------------------------------------------------------------------------


        # Sell all open positions of certain age
        m_positions[m_positions[,"status"] == 1 & m_positions[,"buy_date"] == (j - holding_period), "sell_date"] = j

        # Sell at lowest price of the day
        for (s in which(m_positions[,"sell_date"] == j & m_positions[,"status"] == 1)) {

            # Get knockout from buy date
            m_positions[s, "sell_price"] = calc_BarrierOption(x =  m_train[j, "low"], strike = m_positions[s,"knockout"], maturity = 99, barrier = m_positions[s,"knockout"]) / 10^3

            m_positions[s,"sell_n"] = m_positions[s,"buy_n"]
            m_positions[s,"default"] = 0
            m_positions[s,"status"] = 0


            m_positions[s, "sell_value"] = m_positions[s,"sell_n"] * m_positions[s,"sell_price"]

            m_positions[s, "profit"] = m_positions[s,"sell_value"] - m_positions[s,"buy_value"]


        }         


        # Update portfolio status ----------------------------------------------------------------------------------------------------------------------------

        # Cash at the end of period, is cash at the beginning + sales - buys
        m_portfolio[j,"cash_eop"] = m_portfolio[j,"cash_bop"] + sum(m_positions[m_positions[,"sell_date"] == j, "sell_value"], na.rm = TRUE) - sum(m_positions[m_positions[,"buy_date"] == j, "buy_value"], na.rm = TRUE)

        # Calculate period revenue
        m_portfolio[j,"period_revenue"] = sum(m_positions[m_positions[,"sell_date"] == j, "sell_value"], na.rm = TRUE) 

        # TODO: Track portfolio values, etc. 


    }

    # In last period, sell all assets

    m_positions[m_positions[,"status"] == 1, "sell_date"] = j

    # Sell at lowest price of the day
    for (s in which(m_positions[,"sell_date"] == j & m_positions[,"status"] == 1)) {

        # Get knockout from buy date
        m_positions[s, "sell_price"] = calc_BarrierOption(x =  m_train[j, "low"], strike = m_positions[s,"knockout"], maturity = 99, barrier = m_positions[s,"knockout"]) / 10^3

        m_positions[s,"sell_n"] = m_positions[s,"buy_n"]
        m_positions[s,"default"] = 0
        m_positions[s,"status"] = 0

        m_positions[s, "sell_value"] = m_positions[s,"sell_n"] * m_positions[s,"sell_price"]

        m_positions[s, "profit"] = m_positions[s,"sell_value"] - m_positions[s,"buy_value"]


    }

    # Update final cash
    m_portfolio[j,"cash_eop"] = m_portfolio[j,"cash_bop"] + sum(m_positions[m_positions[,"sell_date"] == j, "sell_value"], na.rm = TRUE) - sum(m_positions[m_positions[,"buy_date"] == j, "buy_value"], na.rm = TRUE)



    # Calculate performance metrics

    # Define the risk free rate
    risk_free_rate = 0.03


    # Sharpe ratio
    # The Sharpe ratio measures the risk-adjusted return of a portfolio by comparing the excess return over the risk-free rate to the standard deviation of the portfolio's return.
        
    # sd(v_liquidity, na.rm = TRUE)


    # Sortino ratio
    # The Sortino ratio is similar to the Sharpe ratio but only considers downside deviation (negative volatility) rather than total volatility.

    # Information Ratio
    # The Information Ratio measures the portfolio's returns relative to a benchmark, adjusted for risk.



    # Collect metrics

    # df_grid[i, ]$cum_revenue = tail(data_train$cum_revenue, 1)
    # df_grid[i, ]$max_drawdown = min(data_train$drawdown, na.rm = TRUE)

    m_grid[i, "liquidity_final"] = m_portfolio[j,"cash_eop"]


}



# plot(log(v_liquidity))


stop("continue here")
# Measure how much cash do I need to back up the strategy
# -> i.e. given prolonged periods of losses -> what is the max drawdown I need to cover

# TODO: Calculate max percentage drawdown! -> 

# Measure portfolio value! Maybe create trigger that cashes in if portfolio value accelerates too much or reached certain level!


# Calculate annualized returns and vola of portfolio
df_grid = as.data.frame(m_grid)


# Compare to base index returns! #> want to have at least 4-5x returns! at not much higher vola! Sharp? and similar







# Calculate year range from date column
y_ = data_train$date %>% range() %>% diff()
y_ = as.numeric(y_) / 365.25

# How much money did I invest????
# Might differ substantilly for each strategy! depending on drawdowns

# -> start with initial cash! Can I sustain the strategy? Or do I default?
# -> what is the default probability given a certain set of parameters?

df_grid$avg_return = (df_grid$liquidity_final / liquidity_init)^(1 / y_) - 1


# Compare to bench mark return
bench_return = (data_train[nrow(data_train),]$close / data_train[1,]$close)^(1 / y_) - 1

# Check distributions of cum_revenue vs max_drawdown!
# -> interpolate grid search to find optimal parameters!
# ????#

# Currently at ~20% yearly return!
df_grid %>% 
    cbind(bench_return = bench_return) %>%
    # Limit drawdowns -> TODO: find more sensible measure!
    # filter(max_drawdown > -10^5) %>%
    mutate(d_ret_bench = avg_return - bench_return) %>%
    # Sort by max profits
    arrange(desc(liquidity_final)) %>%
    head(n = 20)

# The current optimum is just 2% over the bench!!!



# 10x leverage delivers highest returns, but also largest vola of returns
# -> 7-8x leverage seems optimal -> might be different for other indices!
df_grid %>%
    pivot_longer(cols = -c(liquidity_final), names_to = "parameter", values_to = "value") %>%
    ggplot(aes(x = value, y = liquidity_final)) +
    geom_point() +
    facet_wrap(~parameter, scales = "free_x")
    
# Optimal right now:
# 7-8 leverage
# 40 days holding period
# NO risk_local_max
# Yes risk_n_days with
    # ??? -> 

# Run linear regression with y = cum_Revenue and x = all parameters
lm_fit = lm(liquidity_final ~ ., data = df_grid %>% select(-max_drawdown, -cum_revenue, -avg_return) )
summary(lm_fit)


# Train random forest model or similar
# -> Find important variables and predict best set of parameters for maximizing avg_return!
# -> then update the grid -> estimate model -> repeat!
stop("Train the model and dynamically update the grid!")
# Is this reinforcement learning???


# risk_local_max_TFTRUE -> signifcant HIGHLY negabtive impact on cum_revenue

# Calculate median values
df_grid %>%
    group_by(buy_leverage) %>%
    summarise(median_cum_revenue = median(cum_revenue), median_max_drawdown = median(max_drawdown))

df_grid %>%
    group_by(holding_period) %>%
    summarise(median_cum_revenue = median(cum_revenue), median_max_drawdown = median(max_drawdown))



# Analyse in detail
df_grid %>%
    filter(buy_leverage == 8) %>%
     group_by(risk_n_days_signal_threshold) %>%
    summarise(median_cum_revenue = median(cum_revenue), median_max_drawdown = median(max_drawdown))





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

