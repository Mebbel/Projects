



# CLEAN UP!!!! ---------------------------------














# Analyse next day high-open depending on previous day close[t-1] / close[t-2]
# i.e. if there were large price changes at t-1, do we expect larger oppositve price changes at t?


# TODOs

# Test for other indices
    # Nasdaq 100 very profitable
    # Dow jones not profitable
    # SP 500 profitable
    # DAX also profitable
    # -> profitability depends on vola of index?
# Calculate possible cumulative payofss and returns

# Check 2 day performances 
# -> Signal(t) -> max value over periods [t+1, t+2, t+n]
# -> MUCH HIGHER Probability

# Also, if short is triggered only on larger upswings, it also has higher probaility
# -> size of required upswing depends on index!

# Optimize thresholds
# -> separately for long and short
# -> and by index!!!

# Add calculation of optimal option conditions for trades 
    # -> base price diff to current price / days to maturity


# Monitoring of strategy
# -> 5 year trailing average of winning per index? -> decides if an index becomes part or not!


# !!!!!!!!!!!!!!!!!!!! #

# If 5% levered profit expected -> then I need to have a 95% chance of winning
# If 10% levered profit expected -> 91% chance of winning
# 75% chance of winning requires 33% expected profit!

# -0.25 + 0.75 * x = 0
# x = 0.25 / 0.75 = 0.33

# 33% of profit
# If the base value is expected to move by 0.5%
# -> this requires a very high leverage!

# !!!!!!!!!!!!!!!!!!!! #






# !!!! OPTIONS DO NOT DELIVER ENOUGH RETURN FOR THIS STRATEGY !!!! #
# -> Need a signal with larger probability! -> like 90%+
# -> ALTERNATIVES
# -> JUST BUY AND SELL? BUY and sell at high point -> max 0.05% profit -> but limited loss
# -> Knock out certificates -> too risky.



# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #


stop("TODO: Calculate correct expected loss")
# -> use EuropeanOption() from RQuantLib
# -> if there is no 0.05% price upswing on the next day -> I can still close the option
# -> and will not lose 100% -> but maybe only 20-50% -> that could be enough!!!!

# -> Close all options after 5 days -> what is the maximum loss? 
# -> base price -10%? -> calculate from history -> 95% maximum drawdown over 5 days

# -> Run a proper backtesting for this!
# -> observe signal
# -> place trade
# -> calculate option price over next 5 days
# -> either option is sold at expected price
# -> or option is closed at day 6! at the available price

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #





library(tidyquant)
library(tidyverse)
library(ggplot2)

library(RQuantLib)
library(greeks)


stop("TODO: predict next 5 day max price swings depending on historic performance")
# -> setup train + test
# -> train model
# -> check performance on test set -> > 90% accuracy? -> then we can start trading!
# -> let the model also predict the expected vola over the next days -> can place larger bets then!




# Download price history for indices


idx_list = c(
    "^NDX" # Nasdaq 100
    # "^DJI" # Dow Jones
    # "^GSPC" # SP 500
    # "^GDAXI" # DAX

    # NIKKEI? others?

    # Bitcoin? Gold?

    )

# Get price data from yahoo finance
df = tq_get(idx_list, from = "2000-01-01", to = Sys.Date() + 1)


# Define trading strategy
df_signal = df %>%
    mutate(
        # Calculate the spread between close[t-1] and close[t-2]
        signal = lag(close, n = 1L) / lag(close, n = 2L) - 1,
        # Check for two consecutive days of same direction -> increased prob of opposite direction?
        signal_lag = lag(signal, n = 1L),
        # Trade the vola on next day -> buying the option in the evening the previous day
        next_day_spread_long = high / lag(close, n = 1L) - 1,
        spread_long_1 = high / lag(close, n = 1L) - 1,
        spread_long_2 = pmax(high, lead(high, n = 1L)) / lag(close, n = 1L) - 1,
        spread_long_3 = pmax(high, lead(high, n = 1L), lead(high, n = 2L)) / lag(close, n = 1L) - 1,
        spread_long_4 = pmax(high, lead(high, n = 1L), lead(high, n = 2L), lead(high, n = 2L)) / lag(close, n = 1L) - 1,
        spread_long_5 = pmax(high, lead(high, n = 1L), lead(high, n = 2L), lead(high, n = 3L), lead(high, n = 4L)) / lag(close, n = 1L) - 1,
        spread_long_6 = pmax(spread_long_5, lead(high, n = 5L)) / lag(close, n = 1L) - 1,
        spread_long_7 = pmax(spread_long_6, lead(high, n = 6L)) / lag(close, n = 1L) - 1,
        spread_long_8 = pmax(spread_long_7, lead(high, n = 7L)) / lag(close, n = 1L) - 1,
        spread_long_9 = pmax(spread_long_8, lead(high, n = 8L)) / lag(close, n = 1L) - 1,
        spread_long_10 = pmax(spread_long_9, lead(high, n = 9L)) / lag(close, n = 1L) - 1,

        min_long_6 = lead(low, n=5L) / lag(close, n = 1L) - 1,
        min_long_11 = lead(low, n=10L) / lag(close, n = 1L) - 1,

        # For short position take lowest price
        next_day_spread_short = low / lag(close, n = 1L) - 1,
        spread_short_1 =low / lag(close, n = 1L) - 1,
        spread_short_2 = pmin(low, lead(low, n = 1L)) / lag(close, n = 1L) - 1,
        spread_short_3 = pmin(low, lead(low, n = 1L), lead(low, n = 2L)) / lag(close, n = 1L) - 1,
        spread_short_4 = pmin(low, lead(low, n = 1L), lead(low, n = 2L), lead(low, n = 3L)) / lag(close, n = 1L) - 1,
        spread_short_5 = pmin(low, lead(low, n = 1L), lead(low, n = 2L), lead(low, n = 3L), lead(low, n = 4L)) / lag(close, n = 1L) - 1,
        spread_short_6 = pmin(spread_short_5, lead(low, n = 5L)) / lag(close, n = 1L) - 1,
        spread_short_7 = pmin(spread_short_6, lead(low, n = 6L)) / lag(close, n = 1L) - 1,
        spread_short_8 = pmin(spread_short_7, lead(low, n = 7L)) / lag(close, n = 1L) - 1,
        spread_short_9 = pmin(spread_short_8, lead(low, n = 8L)) / lag(close, n = 1L) - 1,
        spread_short_10 = pmin(spread_short_9, lead(low, n = 9L)) / lag(close, n = 1L) - 1,

        max_short_6 = lead(high, n=5L) / lag(close, n = 1L) - 1,
        max_short_11 = lead(high, n=10L) / lag(close, n = 1L) - 1

    ) %>%
    drop_na()

# Calculate more columns
df_signal = df_signal %>%
    # Add date values
    mutate(
        year = year(date)
    )





# Determine MAX price up / down swing given a probability
prob_min = 0.75 
# -> determine price / up down swings
# TODO

# Determine probabilites of price swings conditional on signal
# i.e. if signal is larger than x% -> can I be more certain about my trades?
# TODO


# Decide if index is worthin investing or not!
# -> decide thresholds of signals!
# Select indices for trading --> 5 year trailing average of winning per index?
# TODO


# Calculate position
df_signal = df_signal %>%
    mutate(
        position = case_when(
            signal > 0.01 ~ "short",
            signal < -0.005 ~ "long",
            TRUE ~ "none"
        )
    ) 



# Estimate probabilities of price swing over the next days

# For long positions: Probability of 0.5% / 1% / 2% price upswings

ecdf_long = df_signal %>% filter(position == "long") %>% pull(spread_long_5) %>% ecdf(.)
1 - ecdf_long(0.005) #- 88.5%
1 - ecdf_long(0.01) #- 77.5% that the price will go up by 1% at any point in the next 5 days
1 - ecdf_long(0.02) #- 55.3%

# -> find optimal bets for long positions
plot(1 - ecdf_long(seq(0.005, 0.02, by = 0.001))) # -> linear?
# -> Calculate required payoffs for bet to be profitable
prob = 1 - ecdf_long(seq(0.005, 0.02, by = 0.001))
option_return = ((1 - prob) / prob) 
plot(option_return) # smaller price-swings require lower option returns -> need 12.96% return for 0.5% price swing




# For short positions: Probability of 0.5% / 1% / 2% price downswings

ecdf_short = df_signal %>% filter(position == "short") %>% pull(spread_short_5) %>% ecdf(.)
ecdf_short(-0.005) #- 80%
ecdf_short(-0.01) #- 66.8% that the price will go down by 1% at any point in the next 5 days
ecdf_short(-0.02) #- 48.7%


# How much return does a single option have to yield such that I make a profit over time?


base_return = 0.01 # price change of base value
prob = 1 - ecdf_long(base_return) # probability of option execution

option_return = ((1 - prob) / prob) 
required_leverage = option_return / base_return

print(required_leverage)

# + Costs! Markups, etc.

# Call option ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Latest index price
latest_base_price = df %>% slice_tail(n = 1) %>% pull(close)
expected_base_price_max = latest_base_price * (1 + base_return)

# Calculate required / optimal conditions for the option

# Optimal base_price
# Optimal days to maturity -> within 5 days!

# -> calculate the expected payoff of the option
# TODO

# https://www.tidy-finance.org/r/option-pricing-via-machine-learning.html

black_scholes_price <- function(
    S,  # current stock prices
    K = 70,  # strike price
    r = 0, #  risk-free rates
    T = 1, # times to maturity
    sigma = 0.2 # volatilities 
    ) {
  
  d1 <- (log(S / K) + (r + sigma^2 / 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  
  return(price)
}

# Current value
black_scholes_price(
    S = latest_base_price, # IF this price is reached
    K = latest_base_price * 0.9628, 
    r = 0.029190,
    T = 1 / 252 * 30, #5, # 30, 60, 90 days
    sigma = 0.198 # ESTIMATE!
)


expand_grid(
  S = expected_base_price_max,
  K = latest_base_price * 0.9628, #seq(0.9, 1.1, by = 0.01),
  r = 0.029190,
  T =  1 / 252 * 30, #seq(from = 3 / 12, to = 1, by = 1 / 12), # 3m to 1year
  sigma = 0.198
) |>
  mutate(
    black_scholes = black_scholes_price(S, K, r, T, sigma)
  ) |>
  arrange(desc(black_scholes))

20800 / latest_base_price

# Count the trading days to maturity
weekdays_to_maturity = sum(!weekdays(seq(Sys.Date(), as.Date("2025/02/21"), "days")) %in% c("Saturday", "Sunday", "Samstag", "Sonntag"))


# Tested with onvista rechner!
latest_base_price = 21563

price_today = EuropeanOption(
    type = "call",
    underlying = latest_base_price,
    strike = 20800,
    dividendYield = 0,
    riskFreeRate = 0.029190,
    maturity = 24 / 252,
    volatility = 0.198
)

price_in_5_days = EuropeanOption(
    type = "call",
    underlying = 22000,
    strike = 20800,
    dividendYield = 0,
    riskFreeRate = 0.029190,
    maturity = (24 - 5) / 252,
    volatility = 0.198
)

price_in_5_days$value / price_today$value - 1

# Determine optimal conditions for the option

# 1] Optimize strike price
warning("MAX 4% profit for 0.5% price change! But I need 13%!!!")
expand_grid(
  S_0 = latest_base_price,
  S_1 = latest_base_price * 1.005,
  K = 20800 * seq(0.9, 1.1, by = 0.01),
  r = 0.029190,
  T =  weekdays_to_maturity / 252, #seq(from = 3 / 12, to = 1, by = 1 / 12), # 3m to 1year
  sigma = 0.198
) |>
  rowwise() |>
  mutate(
    price_today = EuropeanOption(type = "call", underlying = S_0, strike = K, riskFreeRate = r, maturity = T, volatility = sigma, dividendYield = 0)$value,
    price_in_5_days = EuropeanOption(type = "call", underlying = S_1, strike = K, riskFreeRate = r, maturity = T - 5 / 252, volatility = sigma, dividendYield = 0)$value,
  ) %>%
  mutate(
    price_d = price_in_5_days / price_today - 1
  ) %>%
  arrange(desc(price_d)) %>%
  ggplot(aes(x = K, y = price_d)) + geom_point()


# 2] Optimize time to maturity -> the younger, the better! -> max 1 month to maturity!
expand_grid(
  S_0 = latest_base_price,
  S_1 = expected_base_price_max,
  K = 20800,
  r = 0.029190,
  T =  seq(from = 1 / 12, to = 1, by = 1 / 12), # 3m to 1year
  sigma = 0.198
) |>
    rowwise() |>
  mutate(
    price_today = EuropeanOption(type = "call", underlying = S_0, strike = K, riskFreeRate = r, maturity = T, volatility = sigma, dividendYield = 0)$value,
    price_in_5_days = EuropeanOption(type = "call", underlying = S_1, strike = K, riskFreeRate = r, maturity = T - 5 / 252, volatility = sigma, dividendYield = 0)$value,
  ) %>%
  mutate(
    price_d = price_in_5_days / price_today - 1
  ) %>%
  arrange(desc(price_d)) %>%
  ggplot(aes(x = T, y = price_d)) + geom_point()


# !!!!!!!!!!!! SUPER IMPORTANT BELOW !!!!!!!!!!!!!!!!! #

# Calculate expected loss and wins

# If the trading signal does not appear within 5 days, i will close the position
# for call positions: spread_short_5 -> will be maximum drawdown event

# Worst case event
spread_short_5_max = df_signal %>% 
    filter(position == "long") %>%
    pull(spread_short_5) %>%
    min()

# 90% probability
spread_short_5_90 = df_signal %>% 
    filter(position == "long") %>%
    pull(spread_short_5) %>%
    quantile(probs = 0.1)


S_0 = latest_base_price
K = 20800
r = 0.029190
sigma = 0.198

price_today = EuropeanOption(type = "call", underlying = latest_base_price, strike = K, riskFreeRate = r, maturity = T, volatility = sigma, dividendYield = 0)$value
price_max_execute = EuropeanOption(type = "call", underlying = latest_base_price * 0.005, strike = K, riskFreeRate = r, maturity = T, volatility = sigma, dividendYield = 0)$value
price_close_fallback_worst = EuropeanOption(type = "call", underlying = latest_base_price * (1 + spread_short_5_max), strike = K, riskFreeRate = r, maturity = T, volatility = sigma, dividendYield = 0)$value
price_close_fallback_90 = EuropeanOption(type = "call", underlying = latest_base_price * (1 + spread_short_5_90), strike = K, riskFreeRate = r, maturity = T, volatility = sigma, dividendYield = 0)$value

# Max loss of 87.5%
price_close_fallback_worst / price_today - 1

# 90% chance that loss is limited to 37%
price_close_fallback_90 / price_today -1


# !! THIS HAS TO BE PART OF SELECTION OF OPTIMAL CONDITIONS FOR DERIVATIVE !!!! 
# -> sensitivity of option can go in both directions!
# -> this is the event that i cannot close my order as expected -> this event has a 20% probability!
# -> Can I set this up with "one cancels the other"-orders?


stop("Continue here!")
# Determin optimal option! -> expected payoff!

# Training vs testing period!

# Define general settings
r = 0.029190
sigma = 0.198

# Determine probabilities

# Probability of trade execution within 5 days at expected price
prob_trade = df_signal %>%
    mutate(
        win_5 = if_else(
                    (position == "long" & spread_long_5 > 0.005) | 
                    (position == "short" & spread_short_5 < -0.005), 
                    1, 
                    0
                )
    ) %>% 
    filter(position != "none") %>%
    pull(win_5) %>% mean(na.rm = T)

# Probability of trade execution by days from buying date! -> maturity has too much impact
df_prob_trade = df_signal %>% 
    select(date, position, starts_with("spread_long"), starts_with("spread_short")) %>%
    pivot_longer(-c(date, position)) %>%
    separate(name, into = c("dummy", "position_", "time"), sep = "_") %>%
    filter(position == position_) %>%
   mutate(
        win = if_else(
                    (position == "long" & value > 0.005) | 
                    (position == "short" & value < -0.005), 
                    1, 
                    0
                )
    ) %>%
    # long signal is much stronger!
    group_by(time, position) %>%
    summarise(win_prob = mean(win))

# Cumulative probs
df_prob_trade = df_prob_trade %>%
    arrange(position, time) %>%
    group_by(position) %>%
    mutate(win_prob = win_prob - lag(win_prob, default = 0))



# Probability of no trade execution
prob_fallback = (1 - prob_trade)


# Determine 90% worst scenario for price if order is not closed
spread_short_5_90 = df_signal %>% 
    filter(position == "long") %>%
    pull(spread_short_5) %>%
    quantile(probs = 0.1)

spread_long_5_90 = df_signal %>% 
    filter(position == "short") %>%
    pull(spread_long_5) %>%
    quantile(probs = 0.9)



# Take a single observation
df_signal %>% 
    slice(1) #%>%
    # Buy option at close

# Optimization only requires close price



determine_optim_option_conditions <- function(
    close,
    option_type = "call"
    ) {

        # base_price_buy = close # close price
        # base_price_sell = base_price_buy * (1 + 0.005)
        # base_price_sell_loss = base_price_buy * (1 + spread_short_5_90)

        # K = base_price_buy * seq(0.8, 1.2, by = 0.1)
        # T =  30 / 252  # assume 1 month to maturity # TODO: There is usually fixed dates for option maturity dates -> can i use that for a perfect recommendation?

        # use df_prob_trade for probabilities!
        prob_trade = df_prob_trade %>% filter(position == if_else(option_type == "call", "long", "short")) %>% filter(time == 1) %>% pull(win_prob)

        # prob_trade = 1
        

        expand_grid(
            base_price_buy = close,
            base_price_sell = base_price_buy * (1 + 0.01),
            base_price_sell_loss =base_price_buy * (1 + spread_short_5_90),
            K = base_price_buy * seq(0.9, 1.1, by = 0.01),
            r = 0.029190,
            T = seq(from = 3 / 12, to = 1, by = 1 / 12), # 3m to 1year
            sigma = 0.198
        ) |>
        rowwise() |>
        mutate(
            # Optimize over K and T

            # Calculate buy price
            option_price_buy = EuropeanOption(type = option_type, underlying = base_price_buy, strike = K, riskFreeRate = r, maturity = T, volatility = sigma, dividendYield = 0)$value,

            # Calculate expected payoff -> within 5 days -> assume selling at T + 5 for conservative pricing
            # probability of >= + 0.005% and expected option price for + 0.005%


            # Add probabilities by day!

            # option_price_sell_expected = EuropeanOption(type = option_type, underlying = base_price_sell, strike = K, riskFreeRate = r, maturity = T - 5/252, volatility = sigma, dividendYield = 0)$value,
            # Next day price execution
            option_price_sell_expected = EuropeanOption(type = option_type, underlying = base_price_sell, strike = K, riskFreeRate = r, maturity = T - 1/252, volatility = sigma, dividendYield = 0)$value,

            # Calculate expected loss -> i.e. limit potential loss after 6 days
            # probability of < +0.005% and expected option price for -spread_short_5_90
            # For long positions assume: base_price_buy * (1 + spread_short_5_90)
            # For short positions assume: base_price_buy * (1 + spread_long_5_90)

            spread_5_90 = if_else(option_type == "call", spread_short_5_90, spread_long_5_90),
            
            option_price_sell_loss = EuropeanOption(type = option_type, underlying = base_price_buy * (1 + spread_5_90), strike = K, riskFreeRate = r, maturity = T - 6/252, volatility = sigma, dividendYield = 0)$value,

            # Calculate expected value
            expected_value = prob_trade * (option_price_sell_expected / option_price_buy -1) + (1 - prob_trade) * (option_price_sell_loss / option_price_buy - 1)

        ) %>% 
        ungroup() %>%
        select(K, T, expected_value) %>%
        arrange(desc(expected_value)) %>%
        # filter(T == 0.25) %>% ggplot(aes(x  = K, y = expected_value)) + geom_line() + geom_vline(xintercept = 100)
        slice(1) %>%
        as.list() %>% 
        return()

    }






# Adjust: I want to sell the option at +20% compared to base price!
# Need to implement probabilities that this price will be reached!


# TODO !!!!!!!!!!!!!!!!!!!!!

# What is the probability that I can sell an option at +20% compared to the base price?














# !!!!!!!!!!!! SUPER IMPORTANT ABOVE!!!!!!!!!!!!!!!!! #


# Determine sell limit for the next 5 days! What is realistic????


# Assume an underlying valued at 100
# -> determine optimal Option
determine_optim_option_conditions(100, "call")


  expand_grid(
            # base_price_buy = close,
            # base_price_sell = base_price_buy * (1 + 0.005),
            # base_price_sell_loss =base_price_buy * (1 + spread_short_5_90),
            K = 120,
            r = 0.029190,
            T = 1:5
        ) |>
        rowwise() |>
        mutate(
            option_price_buy = EuropeanOption(type = option_type, underlying = 100, strike = K, riskFreeRate = r, maturity = 0.25, volatility = sigma, dividendYield = 0)$value,
            option_price_sell = EuropeanOption(type = option_type, underlying = 100 * 1.005, strike = K, riskFreeRate = r, maturity = 0.25 - T/252, volatility = sigma, dividendYield = 0)$value,
        )



df_option_prices = df_signal %>%
    select(date, close, starts_with("spread_long")) %>%
    rowwise() |>
    mutate(

        K = 120,
        r = 0.029190,

        option_price_buy = EuropeanOption(type = option_type, underlying = close, strike = close * 1.2, riskFreeRate = r, maturity = 0.25, volatility = sigma, dividendYield = 0)$value,

        option_price_1 = EuropeanOption(type = option_type, underlying = close * (1 + spread_long_1), strike = close * 1.2, riskFreeRate = r, maturity = 0.25 - 1/252, volatility = sigma, dividendYield = 0)$value,
        option_price_2 = EuropeanOption(type = option_type, underlying = close * (1 + spread_long_2), strike = close * 1.2, riskFreeRate = r, maturity = 0.25 - 2/252, volatility = sigma, dividendYield = 0)$value,
        option_price_3 = EuropeanOption(type = option_type, underlying = close * (1 + spread_long_3), strike = close * 1.2, riskFreeRate = r, maturity = 0.25 - 3/252, volatility = sigma, dividendYield = 0)$value,
        option_price_4 = EuropeanOption(type = option_type, underlying = close * (1 + spread_long_4), strike = close * 1.2, riskFreeRate = r, maturity = 0.25 - 4/252, volatility = sigma, dividendYield = 0)$value,
        option_price_5 = EuropeanOption(type = option_type, underlying = close * (1 + spread_long_5), strike = close * 1.2, riskFreeRate = r, maturity = 0.25 - 5/252, volatility = sigma, dividendYield = 0)$value

    ) %>%
    mutate(
        option_d_1 = (option_price_1 / option_price_buy) - 1,
        option_d_2 = (option_price_2 / option_price_buy) - 1,
        option_d_3 = (option_price_3 / option_price_buy) - 1,
        option_d_4 = (option_price_4 / option_price_buy) - 1,
        option_d_5 = (option_price_5 / option_price_buy) - 1
    )

df_option_prices %>%
    select(date, starts_with("option_d")) %>%
    pivot_longer(cols = -c(date)) %>%
    filter(value < 1) %>%
    ggplot(aes(x = value, color = name)) + 
    stat_ecdf() +
    geom_hline(yintercept = 0.5) +
    geom_vline(xintercept = 0)


df_option_prices %>%
    select(date, starts_with("option_d")) %>%
    pull(option_d_5) %>%
    quantile(probs = c(0.4, 0.5, 0.6, 0.7, 0.75))

# For 5 day trades 
# -> I have a 25% chance of 75% return!
# -> I have a 40% chance of 38% return

# Return doubles at -10% chance!
# -> I have a 50% chance of 23% return
# -> I have a 60% chance of 12.4% return





## START BACKTESTING ######################################

# Assumptions: Sigma, R, FX-Rates, etc. remain unchanged

df_bt = df_signal %>% 
    filter(position != "none") %>% #nrow()
    filter(position == "long") %>%
    # filter(date >= as.Date("2013-01-01")) %>%
    mutate(option_type = if_else(position == "long", "call", "put")) %>%
    # For each position -> calculate optimal option
    # slice(3000:4000) %>%
    rowwise() %>%
    mutate(
        # Limits losses!
        # setes K = 0.9 for call and 1.1 * close for put -> + T = 1 year -> least sensitive too losses!
        # option_conditions = list(determine_optim_option_conditions(close, option_type))

        option_conditions = if_else(
            # position == "long", list(list(K = close * 0.9, T = 1)), list(list(K = close * 1.1, T = 1))
            position == "long", list(list(K = close * 0.9, T = .75)), list(list(K = close * 1.1, T = .75))
        )

        # Too aggressive?
        # option_conditions = list(list(K = close * 1.2, T = 0.25))
    ) %>%
    unnest_wider(option_conditions) %>%
    # mutate(K_ = K/close) %>%
    # View()
    rowwise() %>%
    # calculate approximate buying price of option
    # TODO: Increase by x% -> conservative
    mutate(
        option_price_buy = EuropeanOption(type = option_type, underlying = close, strike = K, riskFreeRate = r, maturity = T, volatility = sigma, dividendYield = 0)$value,
    ) %>%


    # calculate price of option for spread_long_5 and spread_short_5 at T + 5 and T + 6
    # Reduce by x% -> conservative

    # Current signal must be there on the next day!! -> otherwise, I lose money -> different case if the time to maturity is higher -> but then the potential payoff is decreased
    # How to set the sell limit for the option?
    mutate(
        option_price_1 = EuropeanOption(type = option_type, underlying = close * (1 + if_else(position == "long", spread_long_1, spread_short_1)), strike = K, riskFreeRate = r, maturity = T - 1/252, volatility = sigma, dividendYield = 0)$value,
        option_price_2 = EuropeanOption(type = option_type, underlying = close * (1 + if_else(position == "long", spread_long_2, spread_short_2)), strike = K, riskFreeRate = r, maturity = T - 2/252, volatility = sigma, dividendYield = 0)$value,
        option_price_3 = EuropeanOption(type = option_type, underlying = close * (1 + if_else(position == "long", spread_long_3, spread_short_3)), strike = K, riskFreeRate = r, maturity = T - 3/252, volatility = sigma, dividendYield = 0)$value,
        option_price_4 = EuropeanOption(type = option_type, underlying = close * (1 + if_else(position == "long", spread_long_4, spread_short_4)), strike = K, riskFreeRate = r, maturity = T - 4/252, volatility = sigma, dividendYield = 0)$value,
        option_price_5 = EuropeanOption(type = option_type, underlying = close * (1 + if_else(position == "long", spread_long_5, spread_short_5)), strike = K, riskFreeRate = r, maturity = T - 5/252, volatility = sigma, dividendYield = 0)$value,

        option_price_6 = EuropeanOption(type = option_type, underlying = close * (1 + if_else(position == "long", spread_long_5, spread_short_5)), strike = K, riskFreeRate = r, maturity = T - 6/252, volatility = sigma, dividendYield = 0)$value,
        option_price_7 = EuropeanOption(type = option_type, underlying = close * (1 + if_else(position == "long", spread_long_5, spread_short_5)), strike = K, riskFreeRate = r, maturity = T - 7/252, volatility = sigma, dividendYield = 0)$value,
        option_price_8 = EuropeanOption(type = option_type, underlying = close * (1 + if_else(position == "long", spread_long_5, spread_short_5)), strike = K, riskFreeRate = r, maturity = T - 8/252, volatility = sigma, dividendYield = 0)$value,
        option_price_9 = EuropeanOption(type = option_type, underlying = close * (1 + if_else(position == "long", spread_long_5, spread_short_5)), strike = K, riskFreeRate = r, maturity = T - 9/252, volatility = sigma, dividendYield = 0)$value,
        option_price_10 = EuropeanOption(type = option_type, underlying = close * (1 + if_else(position == "long", spread_long_5, spread_short_5)), strike = K, riskFreeRate = r, maturity = T - 10/252, volatility = sigma, dividendYield = 0)$value
    ) %>%

    # Calculate whether I succeeded < T+5 OR I had to close the position at T+6

     mutate(
        option_price_6 = EuropeanOption(
            type = option_type, 
            # This is too pessimistic
            # underlying = close * (1 + if_else(position == "long", spread_short_5, spread_long_5)), # Assume worst case
            underlying = close * (1 + if_else(position == "long", min_long_6, max_short_6)), # slightly better case -> vola could change though.
            strike = K, 
            riskFreeRate = r, 
            maturity = T - 6/252, # 6 days less to maturity
            volatility = sigma, 
            dividendYield = 0)$value,
        option_price_11 = EuropeanOption(
            type = option_type, 
            # This is too pessimistic
            # underlying = close * (1 + if_else(position == "long", spread_short_5, spread_long_5)), # Assume worst case
            underlying = close * (1 + if_else(position == "long", min_long_11, max_short_11)), # slightly better case -> vola could change though.
            strike = K, 
            riskFreeRate = r, 
            maturity = T - 11/252, # 6 days less to maturity
            volatility = sigma, 
            dividendYield = 0)$value,
    ) %>%

    # Calculate Revenue for 100€ investment at each trade
    ungroup() %>%

    # Define sell order as option_price_buy * 1.2 -> that is a sure loss game!
    mutate(
        sell_limit = option_price_buy * 1.1
    ) %>%

    mutate(
        # If can sell, depends on my sellorder!
        sell_limit_trigger = (
            (option_price_1 >= sell_limit) |
            (option_price_2 >= sell_limit) |
            (option_price_3 >= sell_limit) |
            (option_price_4 >= sell_limit) |
            (option_price_5 >= sell_limit) |

            (option_price_6 >= sell_limit) |
            (option_price_7 >= sell_limit) |
            (option_price_8 >= sell_limit) |
            (option_price_9 >= sell_limit) |
            (option_price_10 >= sell_limit)
        )
    ) %>% 
    mutate(
        sell_price = if_else(sell_limit_trigger == TRUE, sell_limit, option_price_11),
        revenue = 100 * sell_price / option_price_buy - 100
    ) %>% 

    # Calculate missed out profits because of my sell-limit
    mutate(
        option_price_max = pmax(option_price_1, option_price_2, option_price_3, option_price_4, option_price_5, option_price_6, option_price_7, option_price_8, option_price_9, option_price_10),
        sell_price_max = if_else(sell_limit_trigger == TRUE, option_price_max, option_price_11),
        revenue_max = 100 * sell_price_max / option_price_buy - 100
    ) %>%


    # Calculate Costs + Taxes
    mutate(
        profit = revenue - 0
    ) %>%
    # Calculate expected Profit
    mutate(
        profit_cum = cumsum(profit),
        profit_cum_max = cumsum(revenue_max),

        profit_cum_rel = profit_cum / (row_number() * 100)
    ) 


df_bt %>% summary()

df_bt %>%
    group_by(position) %>%
    summarise_at(vars(profit, revenue_max), sum)

# sell_limit is too restrictive !!!! -> missing out on too many upswings!
    
# Strategy only works with EXTREME Vola -> 2010 - 2020 -> no profits.
df_bt %>%
    ggplot(aes(x = date, y = profit_cum)) + 
    geom_line(color = "red") +
    # geom_line(aes(y = profit_cum_max)) +
    geom_hline(yintercept = 0, color = "red")



# 3103 trades -> -3.27% loss per trade
# 3103 * 100 * (-0.03270)


# Inspect problems in backtesting -> select training set only!
# -> I need a stronger signal!!


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #

# -> wrap the entire logic? such that I can train a model on it?
# -> need to predict selling price of options!

# Long trades become profitable if I hold the options longer -> at least 10 days.
# -> but that depends on starting date. > they need put options to generate revenus in downward trending markets, like dotcom bust and financial crisis + covid
# -> When to close the position? Do not close it at all? -> optimize!

# -> Consider trend in signal! DO NOT bet big against the trend! If the market is down-ward trending -> overweight put options!

# -> Train model on NASDAQ100 -> test on other indices!


# train / test split
# -> train: from 2000 - 2015 -> includes two recessions
# -> test from 2015 - 2025 -> also includes recessions and increased vola

# For each day -> predict changes in option prices for the next 10-30 days -> both for put and call options
# Determine optimal decision at each date: put / call / none
# -> let the model predict

# next: Determinal optimal sell_limit -> separate for put/call?
# -> let the model predict the optimal


# add explanatority factors like:
# -> increase / decrease in volatility
# -> upward / downward trending -> smas?
# -> lagged intraday vola 
# -> lagged prices




# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #






## END BACKTESTING ######################################

expand_grid(
  S_0 = latest_base_price,
  S_1 = expected_base_price_max,
  K = 20800,
  r = 0.029190,
  T = seq(from = 1 / 12, to = 1, by = 1 / 12), # 3m to 1year
  sigma = 0.198
) |>
    rowwise() |>
  mutate(
    price_today = EuropeanOption(type = "call", underlying = S_0, strike = K, riskFreeRate = r, maturity = T, volatility = sigma, dividendYield = 0)$value,
    price_in_5_days = EuropeanOption(type = "call", underlying = S_1, strike = K, riskFreeRate = r, maturity = T - 5 / 252, volatility = sigma, dividendYield = 0)$value,
  ) %>%
  mutate(
    price_d = price_in_5_days / price_today - 1
  )













# Trading strategy
# If spread on day t is observed to be larger than x%
# Then place a bet that the maximum opposite direction the next day will be 1%
# Use options near in the money with around < 1month to experiation
# Set selling price = buying price * (1 + 1% * leverage / 2)

# 2nd idea: include previous n lags -> cumulative signals of same direction!
# 3rd idea: go long only? win prob by strategy!

df_signal %>%
    # Add date values
    mutate(
        year = year(date)
    ) %>%
    
    # Translate signal to position
    mutate(
            position = case_when(
                signal > 0.01 ~ "short",
                signal < -0.005 ~ "long",
                TRUE ~ "none"
            )
    ) %>%
    mutate(
        next_day_spread = if_else(position == "long", next_day_spread_long, next_day_spread_short),
        next_2_day_spread = if_else(position == "long", spread_long_2, spread_short_2)
    ) %>%
    # Check number of wins
    mutate(
        win = if_else(
            (position == "long" & next_day_spread > 0.005) | 
            (position == "short" & next_day_spread < -0.005), 
            1, 
            0
        ),
        win_2 = if_else(
            (position == "long" & next_2_day_spread > 0.005) | 
            (position == "short" & next_2_day_spread < -0.005), 
            1, 
            0
        ),
        win_5 = if_else(
            (position == "long" & spread_long_5 > 0.005) | 
            (position == "short" & spread_short_5 < -0.005), 
            1, 
            0
        )
    ) %>%
    filter(position != "none") %>%
    # 41% chance of win -> next day 1%
    # 62% change of win -> next day 0.5%
    # -> signal lost after 2010???

    # Remove 2025 -> not enough data for prob analysis
    filter(year < 2025) %>%

    # Long positions make more money! Index is generall upward trending!
    # -> people tend to BUY THE DIP!? if it is sufficiently large
    # -> EVERY year the probability of win is > 50%
    group_by(year, position) %>%
    summarise(
        win = mean(win),
        win_2 = mean(win_2),
        win_5 = mean(win_5),
        n = n()
        
        )     %>%
    ggplot(aes(x = year, y = win_5)) +
    geom_col(aes(fill = position), position = position_dodge()) +
    geom_hline(yintercept = 0.5, color = "red") 

    # Number of days with trades -> long only
    # -> roughly 25% of trading days -> 1 trade a week!
    filter(position == "long") %>%
    ggplot(aes(x = year, y = n / 252)) +
    geom_point()
    



stop("End of script")



# Inspect probabilities

# Single day signal
df_signal %>%
    # Filter to minimum size of signal -> will not place a trade otherwise
    filter(abs(signal) > 0.005) %>%

    # Translate signal to position
    mutate(
        position = if_else(signal > 0, "short", "long")
    ) %>%
    mutate(
        next_day_spread = if_else(position == "long", spread_long_5, spread_short_5)
    ) %>%

    # Display distributions by position # -> > 75% probability to be correct!!
    ggplot(aes(x = next_day_spread)) + 
    stat_ecdf(aes(color = position)) +
    geom_vline(xintercept = 0, color = "red") +
    # highlight min required next day spread
    geom_vline(xintercept = c(0.01, -0.01), color = "blue", linetype = 2)

# Two day signal
df_signal %>%
    # Filter to minimum size of signal -> will not place a trade otherwise
    filter(abs(signal) > 0.005, abs(signal_lag) > 0.005) %>%

    # Translate signal to position
    mutate(
        position = if_else(signal > 0 & signal_lag > 0, "short", "long")
    ) %>%
    mutate(
        next_day_spread = if_else(position == "long", next_day_spread_long, next_day_spread_short)
    ) %>%

    # Display distributions by position # -> > 75% probability to be correct!!
    ggplot(aes(x = next_day_spread)) + 
    stat_ecdf(aes(color = position)) +
    geom_vline(xintercept = 0, color = "red") +
    # highlight min required next day spread
    geom_vline(xintercept = c(0.01, -0.01), color = "blue", linetype = 2)
