# Estimate a model for predicting buying / selling


# Load libraries
library(tidyverse)
library(tidyquant)
library(RQuantLib)
library(greeks)
library(TTR)

library(ggplot2)

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
    "^NDX" # Nasdaq 100
    # "^DJI" # Dow Jones
    # "^GSPC" # SP 500
    # "^GDAXI" # DAX
)

df = tq_get(idx_list, from = "2000-01-01", to = Sys.Date() + 1)

# Calculate some preps

# Close of previous day
df = df %>%
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


# Set period to 30 days
df = df %>% 
    drop_na() %>% # Remove NA rows due to lagging
    rowwise() %>%
    mutate(

        # Calculate the expected return of a barrier option
        price_long_buy = map_dbl(close_prev, ~ calc_BarrierOption(
            x = .x,
            strike = sma_200_close_prev, # Example strike price
            maturity = 99, # unlimited
            barrier = sma_200_close_prev, # Example barrier
            type = "call" # Call option
        )),

        price_short_buy = map_dbl(close_prev, ~ calc_BarrierOption(
            x = .x,
            strike = sma_200_close_prev * 1.2, # Example strike price
            maturity = 99, # unlimited
            barrier = sma_200_close_prev * 1.2, # Example barrier
            type = "put" # Put option
        )),

        # Calculate the value after 30 days
        value_long_lead_30 = map_dbl(close_lead_30, ~ calc_BarrierOption(
            x = .x,
            strike = sma_200_close_prev, # Example strike price
            maturity = 99, # unlimited
            barrier = sma_200_close_prev, # Example barrier
            type = "call" # Call option
        )),

        value_short_lead_30 = map_dbl(close_lead_30, ~ calc_BarrierOption(
            x = .x,
            strike = sma_200_close_prev * 1.2, # Example strike price
            maturity = 99, # unlimited
            barrier = sma_200_close_prev * 1.2, # Example barrier
            type = "put" # Put option
        )),

        # Calculate the minimum price over the next 30 days
        # -> check whether the barrier is hit
        barrier_long_crit = ifelse(low_lead_30 < sma_200_close_prev, 0, 1), # 0 if barrier is hit, 1 if not
        barrier_short_crit = ifelse(high_lead_30 > (sma_200_close_prev * 1.2), 0, 1), # 0 if barrier is hit, 1 if not

        # Return
        return_long_lead_30 = if_else(barrier_long_crit == 0, -1, (value_long_lead_30 - price_long_buy) / price_long_buy), # Return after 30 days,
        return_short_lead_30 = if_else(barrier_short_crit == 0, -1, (value_short_lead_30 - price_short_buy) / price_short_buy), # Return after 30 days
        # Return of cash -> assume 0%
        return_cash_lead_30 = 0
    )

summary(df)

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
    theme_minimal()


# Split into training and testing data
df_train = df %>% filter(date < as.Date("2018-01-01"))
df_test = df %>% filter(date > as.Date("2018-01-01"))



# Train model on predicted action!!!!




