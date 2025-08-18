
# For proper backtesting, we need to create synthetic series for prices


# General Notes.
# NO assumptions about normality of price returns!!! price returns follow a heavy-tailed distribution, e.g., Cauchy distribution.

# Options:
# -> fractal geometry -> too complex and computationally expensive
# -> Hidden Markov Models (HMMs) -> good for modeling time series with regime changes
# -> Levy Processes -> good for modeling heavy-tailed distributions -> generalization of brownian motion
# -> Jump Diffusion Models -> good for modeling price jumps and volatility clustering


# -1] Query data

library(tidyquant)
library(tidyverse)
library(ggplot2)

library(RQuantLib)
library(greeks)

library(MASS)
library(tseries)

library(rugarch)



idx_list = c(
    # "^NDX" # Nasdaq 100 #> profitable



    # "EUR=X" # NOT profitable.

    # "GC=F"


    # "^DJI" # Dow Jones
    "^GSPC" # SP 500
    # "^GDAXI" # DAX

    # NIKKEI? others?

    # Bitcoin? Gold?
    # -> screen consors for possible index underlyings!

    )



# Analyze next day price changes

# Get price data from yahoo finance
df_price = tq_get(idx_list, from = "2000-01-01", to = Sys.Date() + 1)

save(df_price, file = "df_price.RData")

# 0] Measure cauchy distribution of price returns

param_dist = fitdistr(diff(log(df_price$close)), "cauchy")


# Determine min and max for cauchy -> need to limit
range_dist = range(diff(log(df_price$close)), na.rm = TRUE) * 1.2



# 1] Levy process !!! -> cauchy + Auto regression + Fractional Brownian Motion (fBM) dependencies


# Fit garch model - required for modelling of volatility clustering

price_diff = diff(log(df_price$close))

# Define the GARCH model specification
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "norm"  # We will replace this with Lévy noise later
)

# Fit the GARCH model
garch_fit <- ugarchfit(spec = garch_spec, data = price_diff)


# AIC: lower value = better fit
# BIC: lower value = better fit




mu = garch_fit@fit$coef[["mu"]]
omega = garch_fit@fit$coef[["omega"]]
alpha = garch_fit@fit$coef[["alpha1"]]
beta = garch_fit@fit$coef[["beta1"]]


# Iterate and backfeed at each step!

# 1] Draw levy returns









# 4] Check if the returns are of the correct scale of if we need to adjust 






# Levy
# Load necessary libraries


# Function to generate a Lévy Cauchy process
generate_cauchy_process <- function(x0,  N) {

  # dt <- T / N
  W <- numeric(N)
  W[1] <- x0

  # Vola
  vola <- numeric(N)

  # Draw all random values at once
  logdiff = rcauchy(N - 1, location = param_dist$estimate["location"], scale = param_dist$estimate["scale"])

  # Draw new values for values which are oustide the range
  outliers = which(logdiff < range_dist[1] | logdiff > range_dist[2])

  while (length(outliers) > 0) {
    logdiff[outliers] = rcauchy(length(outliers), location = param_dist$estimate["location"], scale = param_dist$estimate["scale"])
    outliers = which(logdiff < range_dist[1] | logdiff > range_dist[2])
  }

  for (t in 2:N) {
      
    # 2] Model volalitily
    vola[t] = omega + alpha * (logdiff[t-1] - mu)^2 + beta * vola[t-1]

    # 3] Adjust levy returns by model volatility
    # apply this volitality to the returns drawn from the cauch distribution
    # returns[t] <- sqrt(volatility[t]) * levy_process[t]
    logdiff[t] = sqrt(vola[t]) * logdiff[t]


    W[t] <- W[t-1] * (1 + logdiff[t])
  }

  return(W)
}

# Parameters
x0 <- df_price$close[1]  # Initial value
N <- 2000  # Number of steps 

# Generate the Cauchy process
# cauchy_process <- lapply(1:100, function(x) generate_cauchy_process(x0, N)) %>% bind_rows(.id = "Simulation")
set.seed(1234)
results_matrix = replicate(1000, generate_cauchy_process(x0, N), simplify = FALSE) %>%
  Reduce(cbind, .)

stacked_values <- as.vector(results_matrix)

# Create an ID column
id_column <- rep(1:ncol(results_matrix), each = nrow(results_matrix))

# Combine the ID column and the stacked values into a data frame
results_long <- data.frame(
  id = id_column,
  time = rep(1:nrow(results_matrix), times = ncol(results_matrix)),
  value = stacked_values
)

# TO assess validoty of the synth series, we can use the following metrics:

# 0] Compare moments

# mean(df_price[1:N,]$close)
# var(df_price[1:N,]$close)
# skewness(df_price[1:N,]$close)

# # Calc moments of synth series
# df_moments = results_long %>%
#   group_by(id) %>%
#   arrange(time, .by_group = T) %>%
#   summarise(
#     mean = mean(value, na.rm = T),
#     median = median(value, na.rm = T),
#     var = var(value, na.rm = T),
#     skewness = skewness(value, na.rm = T)
#   )

# # Identify largest diffs from base series
# df_moments = df_moments %>%
#   mutate(
#     mean_diff = abs(mean - mean(df_price[1:N,]$close)),
#     var_diff = abs(var - var(df_price[1:N,]$close)),
#     skewness_diff = abs(skewness - skewness(df_price[1:N,]$close))
#   )

# # For each measure, drop top 10 largest differences
# results_long = results_long %>%
#   anti_join(df_moments %>% arrange(desc(mean_diff)) %>% slice_head(n = 10), by = "id") %>%
#   anti_join(df_moments %>% arrange(desc(var_diff)) %>% slice_head(n = 10), by = "id") %>%
#   anti_join(df_moments %>% arrange(desc(skewness_diff)) %>% slice_head(n = 10), by = "id")


# 1] Compare the distributions of price returns -> make sure that the potentially small sample creates an entirely different distribution
# Kolmogorov-Smirnov
# H0: the distributions are the same

price_diff = diff(log(df_price[1:N,]$close))

df_ks_test = results_long %>%
    group_by(id) %>%
    mutate(diff = log(value) - lag(log(value))) %>%
    filter(!is.na(diff)) %>%
  
    summarise(
      ks_statistic = list(ks.test(diff, price_diff)$statistic),
      ks_p_value = list(ks.test(diff, price_diff)$p.value)
    ) %>%
    unnest(ks_statistic, ks_p_value) %>% 
    filter(ks_p_value >= 0.025) # WE DO NOT want to reject the H0
    # arrange(desc(ks_p_value))

results_long = results_long %>%
  semi_join(df_ks_test, by = "id")


# 2] Autocorrealation
# Durbin-Watson Test: tseries:dwtest
# Ljung-Box Test: stats:Box.test # H0 = no autocorrelation

# Check results for results of base series


test_result_base <- Box.test(price_diff, lag = 10, type = "Ljung-Box")


df_box_test = results_long %>%
  group_by(id) %>%
   mutate(diff = log(value) - lag(log(value))) %>%
    filter(!is.na(diff)) %>%
  summarise(
    p_value = list(Box.test(diff, lag = 10, type = "Ljung-Box")$p.value),
    statistic = list(Box.test(diff, lag = 10, type = "Ljung-Box")$statistic)
  ) %>%
  unnest(statistic, p_value) %>%
  # remove values where the test staitstic deviates by more than 10% from the base series
  filter(abs(statistic - test_result_base$statistic) / test_result_base$statistic < 0.1) 

results_long = results_long %>%
  semi_join(df_box_test, by = "id")



# 3] Volaility clustering
# Autocorrelation of Squared Returns:
# Calculate the autocorrelation function (ACF) of the squared returns. High autocorrelation in the squared returns indicates strong volatility clustering.

test_result_base <- Box.test(price_diff**2, lag = 10, type = "Ljung-Box")

df_box_test = results_long %>%
  group_by(id) %>%
  mutate(diff = log(value) - lag(log(value))) %>%
  filter(!is.na(diff)) %>%
  summarise(
    p_value = list(Box.test(diff**2, lag = 10, type = "Ljung-Box")$p.value),
    statistic = list(Box.test(diff**2, lag = 10, type = "Ljung-Box")$statistic)
  ) %>%
  unnest(statistic, p_value) %>% 
  # remove values where the test staitstic deviates by more than 10% from the base series
  filter(abs(statistic - test_result_base$statistic) / test_result_base$statistic < 0.5) 

results_long = results_long %>%
  semi_join(df_box_test, by = "id")



# 4] Power Spectrum







# Plot the Cauchy process
ggplot(results_long, aes(x = time, y = value, color = factor(id))) +
  geom_line() +
  # geom_line(data = df_price[1:N,], aes(x = 1:N, y = close), color = "black", size = 0.5) +
  ggtitle('Lévy Cauchy Process') +
  xlab('Time') +
  ylab('Value') +
  guides(color = "none")




# Save scenario results for input to model
write.table(results_long, file = "synth_series.tsv")






# 1] Probability of +-4% returns in next 30 days conditional on +-4% returns in last 30 days
# -> Is this just a markov chain????
# -> This is volitatlity clustering!












stop("End of script!")




# Backup for more complex series generation --------------------------------------




# 2] Compare the autocorrelation of price returns

# Fit an AR(1) model to the log-returns
ar_model <- auto.arima(actual_returns)
summary(ar_model)

# Extract the AR coefficients
ar_coef <- coef(ar_model)
print(ar_coef)


# Function to generate a Lévy Cauchy process with AR(1) dependencies
generate_cauchy_process_with_ar <- function(x0, gamma, ar_coef, T, N) {
  dt <- T / N
  W <- numeric(N)
  W[1] <- x0

  for (t in 2:N) {
    W[t] <- W[t-1] + ar_coef[1] * (W[t-1] - W[t-2]) + rcauchy(1, location = 0, scale = gamma * sqrt(dt))
  }

  return(W)
}



# 3] Calculate the Hurst exponent to assess long-term memory

# Load necessary libraries
library(quantmod)
library(pracma)

# Download actual S&P 500 data
getSymbols("^GSPC", src = "yahoo", from = "2020-01-01", to = "2023-01-01")
sp500_data <- Cl(GSPC)

# Calculate log-returns
actual_returns <- diff(log(sp500_data))

# Calculate the Hurst exponent
hurst_exponent <- hurst(actual_returns)
print(paste("Hurst Exponent (Actual):", hurst_exponent))

# Function to generate fractional Brownian motion (fBM)

## !!!! Attention! This has an assumption of normality !!!!

generate_fbm <- function(H, T, N) {
  dt <- T / N
  W <- numeric(N)
  W[1] <- 0

  for (t in 2:N) {
    W[t] <- W[t-1] + rnorm(1, mean = 0, sd = sqrt(dt^(2*H)))
  }

  return(W)
}

# Function to generate a Lévy Cauchy process with fBM dependencies
generate_cauchy_process_with_fbm <- function(x0, gamma, H, T, N) {
  dt <- T / N
  W <- numeric(N)
  W[1] <- x0

  fbm <- generate_fbm(H, T, N)

  for (t in 2:N) {
    W[t] <- W[t-1] + fbm[t] + rcauchy(1, location = 0, scale = gamma * sqrt(dt))
  }

  return(W)
}

# Parameters
x0 <- sp500_data[1]  # Initial value
gamma <- 0.01  # Scale parameter (adjust based on variance of log-returns)
H <- hurst_exponent  # Hurst exponent
T <- length(sp500_data)  # Time horizon
N <- length(sp500_data)  # Number of steps

# Generate the synthetic price series with fBM dependencies
set.seed(123)  # For reproducibility
synthetic_prices <- generate_cauchy_process_with_fbm(x0, gamma, H, T, N)

# Plot the actual and synthetic price series
df_actual <- data.frame(Date = index(sp500_data), Value = coredata::as.zoo(sp500_data))
df_synthetic <- data.frame(Date = index(sp500_data), Value = synthetic_prices)

ggplot() +
  geom_line(data = df_actual, aes(x = Date, y = Value), color = "blue") +
  geom_line(data = df_synthetic, aes(x = Date, y = Value), color = "red") +
  ggtitle('Actual vs Synthetic S&P 500 Price Series with fBM Dependencies') +
  xlab('Date') +
  ylab('Price') +
  theme_minimal()








