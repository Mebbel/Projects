# Estimate a model for predicting buying / selling




# Notes
# Model 1] Predict the action (buy / sell / cash) based on the features across all indices
# Model 2] Risk Management: Predict volatility and risk of hitting a barrier
# Model 3] Determine the strike / barrier in relation to SMA 200 

# Agent 1] Train an agent to use all 3 models as tool. The Agent has a treasury model and has limited cash to invest.
# The agent needs to decide on the model outputs what is the optimal action to take.

# For action, use features like:
# Pct Change, moments, etc. on SMA (10, 30, 50, 100, 200) on close / low / high / open / volume









# Load libraries
library(tidyverse)
library(tidyquant)
library(RQuantLib)
library(greeks)
library(TTR)

library(ggplot2)

library(tidymodels)
library(timetk)
library(xgboost)

# Custom functions
calc_BarrierOption <- function(x, strike, maturity, barrier, type = "call") {

    if (x < barrier & type == "call") {
        return(0)
    } else if (x > barrier & type == "put") {
        return(0)
    } else {

        barrType = ifelse(type == "call", "downout", "upout")

    val = BarrierOption(
                barrType = barrType,
                type = type, 
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

# calc_BarrierOption(x = 100, strike = 105, maturity = 99, barrier = 105, type = "call") # Test function




# Get historic index data
idx_list = c(
    "^NDX", # Nasdaq 100
    "^DJI", # Dow Jones
    "^GSPC", # SP 500
    "^GDAXI" # DAX
)

df = tq_get(idx_list, from = "2000-01-01", to = Sys.Date() + 1)

# Calculate some preps

# Close of previous day
df = df %>%
    drop_na("close") %>% # Remove rows with NA in close
    group_by(symbol) %>%
    arrange(date, .by_group = TRUE) %>% # Arrange by date within each symbol
    mutate(
        close_prev = lag(close, n = 1),
        close_lead_30 = lead(close, n = 30) # Close 30 days in the future
        )

# Calculate low and high of the next 30 days for knockout barriers
df = df %>%
    mutate(
        low_prev_30 = zoo::rollapply(low, width = 30, FUN = min, fill = NA, align = "right"), # Minimum low over the next 30 days
        low_lead_30 = lead(low_prev_30, n = 30), # Low 30 days in the future

        high_prev_30 = zoo::rollapply(high, width = 30, FUN = max, fill = NA, align = "right"), # Maximum high over the next 30 days
        high_lead_30 = lead(high_prev_30, n = 30) # High 30 days in the future
    )

# Calculate the 200 day moving average on close
df = df %>% 
    mutate(        
        sma_200_close = TTR::SMA(close, n = 200),
        sma_200_close_prev = lag(sma_200_close, n = 1) # Previous day
    )



# Calculate expected returns over n periods for buying a barrier option at the end of each day

# Parameters:
# call option: barrier in relation to sma_200_close_prev
param_call_barrier = 0.8 # 100% of the sma_200_close_prev
param_put_barrier = 1.2 # 120% of the sma_200_close_prev




# Set period to 30 days
df = df %>% 
    drop_na() %>% # Remove NA rows due to lagging
    rowwise() %>%
    mutate(

        # Calculate the expected return of a barrier option
        price_long_buy = map_dbl(close_prev, ~ calc_BarrierOption(
            x = .x,
            strike = sma_200_close_prev * param_call_barrier, # Example strike price
            maturity = 99, # unlimited
            barrier = sma_200_close_prev * param_call_barrier, # Example barrier
            type = "call" # Call option
        )),

        price_short_buy = map_dbl(close_prev, ~ calc_BarrierOption(
            x = .x,
            strike = sma_200_close_prev * param_put_barrier, # Example strike price
            maturity = 99, # unlimited
            barrier = sma_200_close_prev * param_put_barrier, # Example barrier
            type = "put" # Put option
        )),

        # Calculate the value after 30 days
        value_long_lead_30 = map_dbl(close_lead_30, ~ calc_BarrierOption(
            x = .x,
            strike = sma_200_close_prev * param_call_barrier, # Example strike price
            maturity = 99, # unlimited
            barrier = sma_200_close_prev * param_call_barrier, # Example barrier
            type = "call" # Call option
        )),

        value_short_lead_30 = map_dbl(close_lead_30, ~ calc_BarrierOption(
            x = .x,
            strike = sma_200_close_prev * param_put_barrier, # Example strike price
            maturity = 99, # unlimited
            barrier = sma_200_close_prev * param_put_barrier, # Example barrier
            type = "put" # Put option
        )),

        # Calculate the minimum price over the next 30 days
        # -> check whether the barrier is hit
        barrier_long_crit = ifelse(low_lead_30 < sma_200_close_prev * param_call_barrier, 0, 1), # 0 if barrier is hit, 1 if not
        barrier_short_crit = ifelse(high_lead_30 > (sma_200_close_prev * param_put_barrier), 0, 1), # 0 if barrier is hit, 1 if not

        # Return
        return_long_lead_30 = if_else(barrier_long_crit == 0, -1, (value_long_lead_30 - price_long_buy) / price_long_buy), # Return after 30 days,
        return_short_lead_30 = if_else(barrier_short_crit == 0, -1, (value_short_lead_30 - price_short_buy) / price_short_buy), # Return after 30 days
        # Return of cash -> assume 0%
        return_cash_lead_30 = 0
    )

# summary(df)

# Determine the best option for buy / sell / cash
df = df %>%
    mutate(
        action = case_when(
            return_long_lead_30 > return_short_lead_30 & return_long_lead_30 > 0 ~ "call", # Buy call option
            return_short_lead_30 > return_long_lead_30 & return_short_lead_30 > 0 ~ "put", # Buy put option
            T ~ "cash" # Do nothing
        )
    )


# Plot!

df %>%
    ggplot(aes(x = date, y = close)) +
    geom_line() +
    geom_line(aes(y = sma_200_close), color = "blue") + # 200 day moving average
    geom_point(data = df %>% filter(action == "call"), aes(y = close), color = "green", size = 2) + # Buy call option
    geom_point(data = df %>% filter(action == "put"), aes(y = close), color = "red", size = 2) + # Buy put option
    labs(title = "Barrier Option Strategy", x = "Date", y = "Close Price") +
    theme_minimal() +
    facet_wrap(~ symbol, scales = "free_y") # Facet by symbol


# INspect periods where the close is above the 200 day moving average 
# And the signal is sell!
# -> Distance from 200 sma? if too high, then go short?! :) ?

df_plt = df %>%
    filter(between(year(date), 2006, 2009))

df_plt %>%
    ggplot(aes(x = date, y = close)) +
    geom_line() +
    geom_line(aes(y = sma_200_close), color = "blue") + # 200 day moving average
    # geom_point(data = df %>% filter(action == "call"), aes(y = close), color = "green", size = 2) + # Buy call option
    geom_point(data = df_plt %>% filter(action =="put"& close > sma_200_close), aes(y = close), color = "red", size = 2) + # Buy put option
    labs(title = "Barrier Option Strategy", x = "Date", y = "Close Price") +
    theme_minimal()



# Inspect % of actions in periods above the 200 SMA -> compared to periods where close < 200SMA
df %>%
    filter(between(year(date), 2000, 2024)) %>%
    mutate(position = if_else(close > (sma_200_close), "above", "below")) %>%
    count(symbol, position, action) %>%
    pivot_wider(names_from = action, values_from = n, values_fill = 0) %>%
    mutate(call_perc = call / (call + put + cash) * 100)

# If below SMA 200 -> NEVER go long???
# If above SMA 200 -> > 50% change of calling is the best action
# -> so, if the close approachs the SMA 200 from below -> why not go long? because of the vola in the next days?

# -> It all depends on param_call_barrier and param_put_barrier!
# -> if the barrier is too close to the SMA 200 and the price is NOT above the SMA 200 -> then the chance of hitting the barrier is too high!

# -> Determine the distance to barrier in relation to the SMA 200 ???? if above -> then smaller distance, if below -> larger distance

# Histogram: close / sma_200_close -> call_perc / cash_perc / put_perc
df %>%
    filter(between(year(date), 2000, 2024)) %>%
    mutate(rel_sma = close / sma_200_close) %>%
    ggplot(aes(x = rel_sma)) +
    geom_histogram(bins = 100, aes(fill = action), color = "black", position = position_stack()) +
    theme_bw() +
    facet_wrap(~ symbol, scales = "free_y")


df_buckets = df %>%
    filter(between(year(date), 2000, 2024)) %>%
    mutate(rel_sma = close / sma_200_close) %>%
    mutate(rel_sma_floor = rel_sma %>% 
           cut(breaks = seq(0, 2, by = 0.05), 
               labels = paste0(seq(0, 1.9, by = 0.05), "-", seq(0.05, 2, by = 0.05)))) %>%
    group_by(symbol, rel_sma_floor, action) %>%
    summarise(n = n()) %>%
    group_by(symbol, rel_sma_floor) %>%
    mutate(pct = n / sum(n) * 100)


# CHECK N OBS!!!

df_buckets %>% 
select(symbol, rel_sma_floor, action, pct) %>%
filter(action == "call") %>%
pivot_wider(names_from = symbol, values_from = pct, values_fill = 0) %>%
print(n = 50)
    

df_buckets %>%
    ggplot(aes(x= rel_sma_floor, y = pct)) +
    geom_bar(aes(fill=action), stat = "identity") +
    # geom_text(aes(label = paste0(round(pct, 0),"%")), 
    #         colour="white", size=3.5, angle = 90, 
    #         position = position_stack(vjust = 0.5)) +

    # label bars with percentage in the center
    labs(title = "Action Distribution by Relative SMA 200", 
         x = "Relative SMA 200", 
         y = "Percentage (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~ symbol, scales = "free")










# close / sma_200_close ~ 120% -> appears to make sense to start building cash! -> 50:50
# Otherwise, if close / sma_200_close in (100, 120%) -> DO NOT build cash! -> go long / short 60:40???

# -> each index has a different cut-off!!!!



# Split into training and testing data
df_train = df %>% filter(date < as.Date("2018-01-01"))
df_test = df %>% filter(date > as.Date("2018-01-01"))




# Model training
# 1] Define features
# 2] Run PCA - reduce feature space to relevant signals
# 3] Determine optimal number of components
# 4] Train model on PCA components -> Random Forest






# Train model on predicted action!!!!




