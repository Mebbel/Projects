
library(tidyquant)
library(tidyverse)
library(ggplot2)

library(RQuantLib)
library(greeks)



idx_list = c(
    # "^NDX" # Nasdaq 100 #> profitable


    # "EUR=X" # NOT profitable.

    # "GC=F"


    # "^DJI" # Dow Jones
    # "^GSPC" # SP 500
    "^GDAXI" # DAX

    # NIKKEI? others?

    # Bitcoin? Gold?

    )



# Analyze next day price changes

# Get price data from yahoo finance
df = tq_get(idx_list, from = "2000-01-01", to = Sys.Date() + 1)



df_sel = df %>%
    group_by(symbol) %>%
    arrange(date, .by_group = T) %>%
    mutate(
        # Buy in the evening
        price_t0 = close,
        # Max loss or knockout level the next day
        low_t1 = lead(low, 1),
        # Max possible gain
        high_t1 = lead(high, 1),
        # Limit loss in the evening of next day
        close_t1 = lead(close, 1)
    ) %>%
    select(symbol, date, ends_with("t0"), ends_with("t1"))

# Calculaterelative changes
df_sel = df_sel %>%
    mutate(
        # Price change
        price_change = (close_t1 - price_t0) / price_t0,
        # Max loss
        max_loss = (low_t1 - price_t0) / price_t0,
        # Max gain
        max_gain = (high_t1 - price_t0) / price_t0,
        # Limit loss
        limit_loss = (close_t1 - price_t0) / price_t0
    ) %>%
    select(symbol, date, ends_with("t0"), ends_with("t1"), price_change, max_loss, max_gain, limit_loss)

    
# Subset to train and testing
df_train = df_sel %>% filter(date < as.Date("2018-01-01"))
df_test = df_sel %>% filter(date > as.Date("2018-01-01"))



# Extract quantiles

# 95% chance of less than 3.6% price drop -> symmetric +/- 1.5%
knockout_long = df_train %>% drop_na(max_loss) %>% pull(max_loss) %>% quantile(probs = 0.2) %>% .[[1]]
knockout_short = df_train %>% drop_na(max_gain) %>% pull(max_gain) %>% quantile(probs = 1-0.2) %>% .[[1]]

# 60% chance of at least 0.5% price increase -> symmetric! +/- 2.5%
limit_order_sell_long = df_train %>% drop_na(max_gain) %>% pull(max_gain) %>% quantile(probs = 0.9) %>% .[[1]]
limit_order_sell_short = df_train %>% drop_na(max_loss) %>% pull(max_loss) %>% quantile(probs = 1-0.9) %>% .[[1]]




# Replicate on test dataset!

# Count days of knockout + limit loss!

df_test %>% 
    mutate(
        knockout = ifelse(low_t1 <= (1 + knockout) * price_t0, 1, 0),
        limit_loss = ifelse(high_t1 < (1 + limit_order_sell) * price_t0, 1, 0)
    ) %>%
# Count days of knockout + limit loss
    summarise(
        knockout = sum(knockout, na.rm = T),
        limit_loss = sum(limit_loss, na.rm = T)
    ) %>%
    mutate(
        knockout = knockout / nrow(df_test),
        limit_loss = limit_loss / nrow(df_test)
    )
# 3% chance of being knocked-out
# 41% chance of having to limit losses

# Calculate actual profits

# 1] Without knockout

calc_EuropeanOption <- function(x, strike, maturity) {

val = EuropeanOption(
            type = "call", 
            underlying = x,
            strike = strike,
            dividendYield = 0,
            riskFreeRate = 0.029190,
            maturity = maturity,
            volatility = 0.198
        )

return(val$value)
}


# !!!! ADjust maturity!!!!
# 

df_backtest = df_test %>%
    drop_na() %>%
    rowwise() %>%
    mutate(
        # Calculate strike price
        strike = price_t0 * 0.95,
        # Buy the option
        price_buy =  calc_EuropeanOption(price_t0, strike = strike, maturity = 24/252),
        # Place a sell order
        price_limit_sell = calc_EuropeanOption(price_t0 * (1 + limit_order_sell), strike = strike, maturity = 23/252),
        # Check if sell order can be executed
        execute_limit_sell = ifelse(high_t1 > (1 + limit_order_sell) * price_t0, 1, 0),
        # Calculate loss if sold in the evening of the next day
        price_limit_loss = calc_EuropeanOption(close_t1, strike = strike, maturity = 23/252)
) 

# Calculate revenue per day
df_backtest %>%
    ungroup() %>%
    mutate(
        cost = -price_buy, 
        revenue = if_else(execute_limit_sell == 1, price_limit_sell, price_limit_loss),
        profit = revenue + cost
    ) %>% 
    mutate(profit_cum = cumsum(profit)) %>%
    ggplot(aes(x = date, y = profit_cum)) +
    geom_line()
# Substract transaction costs + taxes

# Calculate cumulative profit



# With knockout


# Calculate actual profits

# 1] Without knockout

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

# Do a grid-search on:
# -> strike_price
# -> knockout
# -> limit_order_sell

# Optimize long-term average relative profit / loss

# IDea -> how does this perform wihtout a limit sell order -> just sell every evening?
# -> that is automatically POSITIVE returns!!!
# > SO I JUST BUY AT whatever price every evning a 0DTE
# And sell it at whatever price the next evening? WHAT?
# TEST THIS!!

# Calculate actual RoI -> 
# Assume 1.000€ Investment
# -> Always invest 30% of cash each evening?

# Infer 6month trend -> DO NOT bet against the trend!
# -> Suspend buyorders if there is no clear direction and / or not enough volatility in the index

# THEN! How to increase profit? Tactial sell orders?
# -> Put ludicrous sell-orders -> close + 3%?
# Infer from history!

# Adjust maturity? -> how does this behave with almost 0DTE orders?
# -> i.e. 3-5 days to maturity?
# -> THIS IS Not always available! -> use max leverage of 20/30 -> that's more safe for backtesting!




# Adjust holding period?! Hold 1,2,3,4,5,... days?
# -> Hold to maturity? Just roll the option? 
# -> Or sell them always 5-10 days BEFORE maturity?


# Compare! To buy and hold
# Underlying index
# Buy and hold options longer term!


stop("Optimize on trainig data!!!!")




df_backtest = df_test %>%
    drop_na() %>%
    rowwise() %>%
    mutate(
        # Calculate strike price
        strike = price_t0 * 0.95,
        barrier = price_t0 * (1 + knockout_long),

        # Buy the option
        price_buy =  calc_BarrierOption(price_t0, strike = strike, maturity = 24/252, barrier = barrier),
        # Place a sell order
        price_limit_sell = calc_BarrierOption(price_t0 * (1 + limit_order_sell_long), strike = strike, maturity = 23/252, barrier = barrier),
        # Check if sell order can be executed
        execute_limit_sell = ifelse(high_t1 > (1 + limit_order_sell_long) * price_t0, 1, 0),
        # Calculate loss if sold in the evening of the next day
        price_limit_loss = calc_BarrierOption(close_t1, strike = strike, maturity = 23/252, barrier = barrier)
) 

# Profitable? How?
df_backtest %>%
    ungroup() %>%
    mutate(
        cost = -price_buy, 
        # Calculat barrier
        barrier = ifelse(low_t1 < barrier, 1, 0),
        # revenue = if_else(barrier == 1, 0,  if_else(execute_limit_sell == 1, price_limit_sell, price_limit_loss)),
        revenue = if_else(barrier == 1, 0, price_limit_loss),
        profit = revenue + cost
    ) %>% 
    mutate(
        profit_cum = cumsum(profit), cost_cum = cumsum(cost),

        # Initial cash
        cash = if_else(row_number() == 1, 1000, 0),
        cash = cash + cumsum(profit)
        
        ) %>%
    mutate(profit_rel = profit_cum / -cost_cum) %>%
    ggplot(aes(x = date, y = cash)) +
    geom_line() + 
    geom_hline(yintercept = 0, color = "red")

# made 30k after 6 years on 1.000 init cash???
(30000 / 100)^(1/6) - 1 # 158% annualized return ????


# TODO: Compare basic vs barrier option
# -> Why does the barrier option not have a downfall in 2022 as the basic optino?



# Calculate moving average of knockout + limit_loss probability
# -> do these chance signifcantlyin times of elevated market insecurity?











underlying = 3402

underlying * (1 + knockout_long)

lever = 35.38



8.150* (1 + (limit_order_sell_long * lever))
