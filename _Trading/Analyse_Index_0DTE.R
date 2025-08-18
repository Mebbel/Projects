
# Notes 

# TODOs.
# Predict probability of positive returns on next trading day
# 1] Consider current market trend -> do not go long in a bear market
# 2] Consider consecutive positive returns -> how long can a streak last?

# Check price changes and actual price changes in barrier options
# 1] Check if I calculate price changes correctly - do I need an adjustment factor < 1?







library(tidyquant)
library(tidyverse)
library(ggplot2)

library(RQuantLib)
library(greeks)



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




# Define functions ------------------------------------------------------------------


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


calc_Backtest_barrier <- function(

    df_train,

    q_knockout_long,
    q_limit_order_sell_long

) {


    knockout_long = df_train %>% drop_na(max_loss) %>% pull(max_loss) %>% quantile(probs = q_knockout_long) %>% .[[1]]
    limit_order_sell_long = df_train %>% drop_na(max_gain) %>% pull(max_gain) %>% quantile(probs = q_limit_order_sell_long) %>% .[[1]]

df_backtest = df_train %>%
    drop_na() %>%
    rowwise() %>%
    mutate(
        # Calculate strike price
        # strike = price_t0 * param_i$strik_price_long_disc,
        barrier = price_t0 * pmin(1, (1 + knockout_long)),
        strike = barrier,

        # Buy the option -> knockouts are usually open-end
        price_buy =  calc_BarrierOption(price_t0, strike = strike, maturity = 10, barrier = barrier),

        n_buy = floor(100 / price_buy),

        # Place a sell order
        price_limit_sell = calc_BarrierOption(price_t0 * (1 + limit_order_sell_long), strike = strike, maturity = 10, barrier = barrier),

        # Check if sell order can be executed
        execute_limit_sell = ifelse(high_t1 > (1 + limit_order_sell_long) * price_t0, 1, 0),

        # Calculate loss if sold in the evening of the next day
        price_limit_loss = calc_BarrierOption(close_t1, strike = strike, maturity = 10, barrier = barrier)

    ) 

    # Profitable? How?
    df_backtest = df_backtest %>%
    ungroup() %>%
    mutate(
        cost = -n_buy * price_buy, 

        # Indicator for barrier breach
        barrier = ifelse(low_t1 < barrier, 1, 0),

        # Indicator for execution of limit order -> execute_limit_sell        

        # Calculate revenue
        # -> if barrier breached -> 0 revenue
        # -> if limit order executed -> price_limit_sell
        # -> if limit order not executed -> price_limit_loss
        revenue = if_else(barrier == 1, 0,  if_else(execute_limit_sell == 1, n_buy * price_limit_sell, n_buy * price_limit_loss)),

        # revenue = if_else(barrier == 1, 0, price_limit_loss),
        profit = revenue + cost,

        cash = cumsum(profit),

        # Remove profit if cash + init_cash < 0 -> these are the points where the strategy runs out of cash!
        #cash = ifelse(cash + param_i$cash_init < 0, NA, cash),
        #profit = ifelse(cash + param_i$cash_init < 0, NA, profit)
    ) 


    return(df_backtest)


}


# Analyses
# Distribution of returns when index increased by +2% on two consecutive days

# 50% chance of -1.5% max price drop during the day.
# 50% chance of +0.8% increase during the day
# df_train %>% 
#     filter(lag(price_change, 1) > 0.02 & lag(price_change, 2) > 0.02) %>%
#     summarise(
#         long_barrier = quantile(max_loss, probs = 0.05, na.rm = T),
#         long_limit = quantile(max_gain, probs = 0.50, na.rm = T),
#         short_barrier = quantile(max_gain, probs = 0.95, na.rm = T),
#         short_limit = quantile(max_loss, probs = 0.50, na.rm = T)
#     ) 

# # If short: q_gain = barrier 

# underlying = 18787
# underlying * (1 + -0.0526)

# lever = 18.38



# 9 * (1 + (0.01 * lever))


stop("CHeck price changes in barrier optinos -> they same execessive")
# They imply a 200+ leverage! How would I get that?

df_train %>% drop_na(max_loss) %>% pull(max_loss) %>% quantile(probs = c(.2,.3, 0.4, 0.5, 0.6))

strike =  18635 * (1 + -0.005)
option_buy = calc_BarrierOption(10000, strike = strike, maturity = 10, barrier =strike)
option_sell = calc_BarrierOption(10000 * 1.02, strike = strike, maturity = 10, barrier = strike)

# Calculate implied lever -> 232???
(option_sell / option_buy - 1) / 0.02

# -> FOR THESE TYPES OF OPTIONS; I HAVE A 50% spread!!!

# -> if i limit the options to actually possible bets
# -> THen I really need to tackle negative trends! Dot com bust + Great financial crisis ruins the portfolio!!!


stop("Implement trend! go short if trend is negative!???")





# Do a grid-search on:
# -> strike_price
# -> knockout
# -> limit_order_sell

df_optim_grid = expand.grid(

    q_knockout_long = seq(0.05, 0.3, by = 0.05),
    q_limit_order_sell_long = seq(0.85, 0.95, by = 0.05)#,

    # Use short-term trend
    # use_short_term_trend = c(0, 1), # 0 = no, 1 = yes
    # short_term_trend_calc_days = c(30, 60, 90), # 30, 60, 90 days
    # short_term_trend_diff_days = c(30, 60, 90) # 30, 60, 90 days

    # Usually, strike = barrier!
    #strik_price_long_disc = seq(0.8, 0.95, by = 0.05)
)

stop("Implement new parameters!")

df_optim_grid$profit = NA

df_optim_grid$barrier = NA
df_optim_grid$limit_sell = NA

df_optim_grid$cash_init = 1000

df_optim_grid$max_conseq_barrier = NA




#  sma_100 = rollmean(price_t0, k = 90, fill = NA, align = "right"),
#         # If > 0 -> then invest long - else do nothing.
#         sma_100_90 = sma_100 / lag(sma_100, n = 30) - 1,


# TODO: Add bid-ask spread
# TODO: Add transaction costs
# TODO: Optimize holding periods! Maybe hold options 2+ days!

# Iterate grid and collect results
for (i in 1:nrow(df_optim_grid)) {

    print(i)

    param_i = df_optim_grid[i,]  

    # TODO: Put into function!!! takes parameters and df_train/test as argument
    df_backtest = calc_Backtest_barrier(
        df_train,
        param_i$q_knockout_long,
        param_i$q_limit_order_sell_long
    )
   
    
    # Calculate required cash for strategy! -> when does it run out of cash!, i.e. how much commitment do i need initially!
    # -> then I can calculate actual returns! and compare.

    df_optim_grid[i,"cash"] = min(df_backtest$cash, na.rm = T) + param_i$cash_init

    # Add profit
    df_optim_grid[i,"profit"] = sum(df_backtest$profit, na.rm = T)

    # Calculate days with
    # 1] Barrier brocken
    df_optim_grid[i,"barrier"] = sum(df_backtest$barrier, na.rm = T) / nrow(df_backtest)
    # 2] Executed limit order
    df_optim_grid[i,"limit_sell"] = sum(df_backtest$execute_limit_sell, na.rm = T) / nrow(df_backtest)

    # 3] Limit loss in the next evening
    # ...

    # 4] Calculate cumulative days of barrier breached
    max_conseq_barrier = rle(df_backtest$barrier)

    df_optim_grid[i,"max_conseq_barrier"] = max(max_conseq_barrier$lengths[max_conseq_barrier$values == 1])

    

}





# Calculate returns! profit / initial cash
df_optim_grid = df_optim_grid %>%
    filter(profit > 0) %>%
    mutate(
        profit_rel = pmax(0, profit / cash_init - 1),
        profit_rel = ifelse(is.infinite(profit_rel), 0, profit_rel),
        profit_pd = (1 + profit_rel)^(1/as.integer(max(df_train$date) - min(df_train$date))) - 1,
        profit_pa = (1 + profit_pd)^(252) - 1
    ) %>%
    arrange(desc(profit_pa))

head(df_optim_grid)

# Note: in optimal strategy - the barrier can be breached in up to 22 consecutive trading days!!! -> that is an entire month!



stop("3D-plot? find combinations of parameters that are profitable")

df_optim_grid %>%
select(starts_with("q_"), profit) %>%
    pivot_longer(cols = -c(profit)) %>%
ggplot(aes(x = value, y = profit)) +
geom_point() +
geom_hline(yintercept = 0, color = "red") +
facet_wrap(.~name, scales = "free_x")





stop("Continue here: Analyze periods of profitability and losses! Do we need a trend in the strategy?")

# When do I lose the money?

 df_backtest = calc_Backtest_barrier(
        df_train = df_train,
        q_knockout_long = df_optim_grid[1, "q_knockout_long"],
        q_limit_order_sell_long = df_optim_grid[1, "q_limit_order_sell_long"]
    )

df_backtest %>%
    ggplot(aes(x = date, y = price_t0)) + 
    geom_line() +
    geom_line(aes(y = cash), color = "red")


# I lose money when I hit the barrier!
# When does this happen more frequently?
# -> rolling 30 day average of barrier breaches?


# I want a 60% barrier breach -> but during times of crises, this increases to 80%!!!

# Stop buying if price remains flat? indicator for price decrease?
# Stop long if price drops? maybe go short?

df_backtest %>%
    mutate(
        barrier_mean = rollmean(barrier, k = 90, fill = NA, align = "right"),
        sma_100 = rollmean(price_t0, k = 90, fill = NA, align = "right"),
        # If > 0 -> then invest long - else do nothing.
        sma_100_90 = sma_100 / lag(sma_100, n = 30) - 1,
    ) %>% 
    ggplot(aes(x = date, y = barrier_mean)) +
    geom_line() +
    geom_line(aes(y = -sma_100_90), color = "blue") + 
    geom_hline(yintercept = 0.5, color = "red") +
    geom_hline(yintercept = 0, color = "orange") +
    theme_bw()

    df_test %>%
    mutate(
        # barrier_mean = rollmean(barrier, k = 90, fill = NA, align = "right"),
        sma_100 = rollmean(price_t0, k = 90, fill = NA, align = "right"),
        # If > 0 -> then invest long - else do nothing.
        sma_100_90 = sma_100 / lag(sma_100, n = 30) - 1,
    ) %>% 
    ggplot(aes(x = date, y = sma_100_90)) +
    geom_line() +
    # geom_line(aes(y = -sma_100_90), color = "blue") + 
    geom_hline(yintercept = 0.5, color = "red") +
    geom_hline(yintercept = 0, color = "orange") +
    theme_bw()





# Out of Sample testing!









# Extract quantiles

# 95% chance of less than 3.6% price drop -> symmetric +/- 1.5%
knockout_long = df_train %>% drop_na(max_loss) %>% pull(max_loss) %>% quantile(probs = 0.5) %>% .[[1]]
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
(10000 / 100)^(1/6) - 1 # 158% annualized return ????


# TODO: Compare basic vs barrier option
# -> Why does the barrier option not have a downfall in 2022 as the basic optino?



# Calculate moving average of knockout + limit_loss probability
# -> do these chance signifcantlyin times of elevated market insecurity?











underlying = 3402

underlying * (1 + knockout_long)

lever = 35.38



8.150* (1 + (limit_order_sell_long * lever))
