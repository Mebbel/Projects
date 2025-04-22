# Can I outperform indices with options?

# i.e. I expected that the Nasdaq 100 will increase in value over the next 100 years

# -> Is there an option strategy to significantly beat this performance?


# Determine current trend of the index
# -> last 200 days -> mesaure average daily change
# -> if positive -> buy call option
    # -> Compare potential loss of value in option per day vs average daily change
    # -> if loss < average daily change -> buy option!
# Do not buy put options -> too risky.



# Grid-search for optimal values
# -> minimum trend + window
# -> type of optione - in / at / out of the money
# -> time to maturity
# etc.


library(tidyquant)
library(tidyverse)
library(ggplot2)

library(RQuantLib)
library(greeks)



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


head(df)

# Calculate average daily change over the last 200 days with linear regression

# Use a loop

w = 100

df["d"] = 0

for (i in w:(nrow(df))) {

    df_i = df[i:(i - w),]

    d = lm(formula = log(adjusted) ~ date, df_i)$coef

    df[i, "d"] = d["date"] 

}


# Inspect 

df %>% 
ggplot(aes(x = date, y = d)) + 
geom_line() + 
geom_hline(yintercept = 0, linetype = "dashed") + theme_minimal()

View(tail(df, n = 200))




# Calculate daily loss in option value

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
p_t0 = black_scholes_price(
    S = 100, # IF this price is reached
    K = 100, 
    r = 0.029190,
    T = 1 / 252 * 30, #5, # 30, 60, 90 days
    sigma = 0.198 # ESTIMATE!
)

p_t10 = black_scholes_price(
    S = 100, # IF this price is reached
    K = 100, 
    r = 0.029190,
    T = 1 / 252 * (30 - 10), #5, # 30, 60, 90 days
    sigma = 0.198 # ESTIMATE!
)

# If nothing changes, the option loses 20% over 10 days, i.e. 2% per day
p_t10 / p_t0 - 1



# Asssume that index changes with 0.0008439 per day
# Which is 0.08439% per day
# -> 0.08439% * 100 = 8.439% over 100 days

p_t30 = black_scholes_price(
    S = 100, # IF this price is reached
    K = 100 * ((1 + 0.0008439)^29), 
    r = 0.029190,
    T = 1, #5, # 30, 60, 90 days
    sigma = 0.198 # ESTIMATE!
)

# 180% profit over 30 days
p_t30 / p_t0 - 1

