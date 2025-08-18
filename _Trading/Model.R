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


# Get historic index data for explanary variables
idx_list = c(
    "^VIX" # VIX Index
)

df_exp = tq_get(idx_list, from = "2000-01-01", to = Sys.Date() + 1)



# Calculate some preps

# Close of previous day
df = df %>%
    drop_na("close") %>% # Remove rows with NA in close
    group_by(symbol) %>%
    arrange(date, .by_group = TRUE) %>% # Arrange by date within each symbol
    mutate(
        close_prev = lag(close, n = 1),
        close_prev_30 = lag(close, n = 30), # Close 30 days in the past
        close_lead_30 = lead(close, n = 30) # Close 30 days in the future
        )
       

# Calculate low and high of the next 30 days for knockout barriers
df = df %>%
    mutate(
        low_prev = lag(low, n = 1), # Minimum low over the next 30 days
        low_prev_30 = zoo::rollapply(low, width = 30, FUN = min, fill = NA, align = "right"), # Minimum low over the next 30 days
        low_lead_30 = lead(low_prev_30, n = 30), # Low 30 days in the future

        high_prev = lag(high, n = 1), # Maximum high over the next 30 days
        high_prev_30 = zoo::rollapply(high, width = 30, FUN = max, fill = NA, align = "right"), # Maximum high over the next 30 days
        high_lead_30 = lead(high_prev_30, n = 30) # High 30 days in the future
    ) %>%
    # intraday spread
    mutate(
        high_low_spread_prev = high_prev - low_prev, # Spread between high and low of previous day
        high_low_spread_rel_prev = high_low_spread_prev / close_prev, # Relative spread between high and low of previous day
        high_low_spread_prev_30 = high_prev_30 - low_prev_30, # Spread between high and low
        high_low_spread_rel_prev_30 = high_low_spread_prev_30 / close_prev_30 # Relative spread between high and low of previous 30 days
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
# Require minimum abs(1%) return for call / put option
df = df %>%
    mutate(
        action = case_when(
            return_long_lead_30 > return_short_lead_30 & return_long_lead_30 > 0.01 ~ "call", # Buy call option
            return_short_lead_30 > return_long_lead_30 & return_short_lead_30 > 0.01 ~ "put", # Buy put option
            T ~ "cash" # Do nothing
        )
    )




# Plot!

# df %>%
#     ggplot(aes(x = date, y = close)) +
#     geom_line() +
#     geom_line(aes(y = sma_200_close), color = "blue") + # 200 day moving average
#     geom_point(data = df %>% filter(action == "call"), aes(y = close), color = "green", size = 2) + # Buy call option
#     geom_point(data = df %>% filter(action == "put"), aes(y = close), color = "red", size = 2) + # Buy put option
#     labs(title = "Barrier Option Strategy", x = "Date", y = "Close Price") +
#     theme_minimal() +
#     facet_wrap(~ symbol, scales = "free_y") # Facet by symbol


# # INspect periods where the close is above the 200 day moving average 
# # And the signal is sell!
# # -> Distance from 200 sma? if too high, then go short?! :) ?

# df_plt = df %>%
#     filter(between(year(date), 2006, 2009))

# df_plt %>%
#     ggplot(aes(x = date, y = close)) +
#     geom_line() +
#     geom_line(aes(y = sma_200_close), color = "blue") + # 200 day moving average
#     # geom_point(data = df %>% filter(action == "call"), aes(y = close), color = "green", size = 2) + # Buy call option
#     geom_point(data = df_plt %>% filter(action =="put"& close > sma_200_close), aes(y = close), color = "red", size = 2) + # Buy put option
#     labs(title = "Barrier Option Strategy", x = "Date", y = "Close Price") +
#     theme_minimal()



# # Inspect % of actions in periods above the 200 SMA -> compared to periods where close < 200SMA
# df %>%
#     filter(between(year(date), 2000, 2024)) %>%
#     mutate(position = if_else(close > (sma_200_close), "above", "below")) %>%
#     count(symbol, position, action) %>%
#     pivot_wider(names_from = action, values_from = n, values_fill = 0) %>%
#     mutate(call_perc = call / (call + put + cash) * 100)

# # If below SMA 200 -> NEVER go long???
# # If above SMA 200 -> > 50% change of calling is the best action
# # -> so, if the close approachs the SMA 200 from below -> why not go long? because of the vola in the next days?

# # -> It all depends on param_call_barrier and param_put_barrier!
# # -> if the barrier is too close to the SMA 200 and the price is NOT above the SMA 200 -> then the chance of hitting the barrier is too high!

# # -> Determine the distance to barrier in relation to the SMA 200 ???? if above -> then smaller distance, if below -> larger distance

# # Histogram: close / sma_200_close -> call_perc / cash_perc / put_perc
# df %>%
#     filter(between(year(date), 2000, 2024)) %>%
#     mutate(rel_sma = close / sma_200_close) %>%
#     ggplot(aes(x = rel_sma)) +
#     geom_histogram(bins = 100, aes(fill = action), color = "black", position = position_stack()) +
#     theme_bw() +
#     facet_wrap(~ symbol, scales = "free_y")


# df_buckets = df %>%
#     filter(between(year(date), 2000, 2024)) %>%
#     mutate(rel_sma = close / sma_200_close) %>%
#     mutate(rel_sma_floor = rel_sma %>% 
#            cut(breaks = seq(0, 2, by = 0.05), 
#                labels = paste0(seq(0, 1.9, by = 0.05), "-", seq(0.05, 2, by = 0.05)))) %>%
#     group_by(symbol, rel_sma_floor, action) %>%
#     summarise(n = n()) %>%
#     group_by(symbol, rel_sma_floor) %>%
#     mutate(pct = n / sum(n) * 100)


# # CHECK N OBS!!!

# df_buckets %>% 
# select(symbol, rel_sma_floor, action, pct) %>%
# filter(action == "call") %>%
# pivot_wider(names_from = symbol, values_from = pct, values_fill = 0) %>%
# print(n = 50)
    

# df_buckets %>%
#     ggplot(aes(x= rel_sma_floor, y = pct)) +
#     geom_bar(aes(fill=action), stat = "identity") +
#     # geom_text(aes(label = paste0(round(pct, 0),"%")), 
#     #         colour="white", size=3.5, angle = 90, 
#     #         position = position_stack(vjust = 0.5)) +

#     # label bars with percentage in the center
#     labs(title = "Action Distribution by Relative SMA 200", 
#          x = "Relative SMA 200", 
#          y = "Percentage (%)") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#     facet_wrap(~ symbol, scales = "free")






# Calculate more features
df = df %>%
    ungroup() %>%
    mutate(
        close_rel_sma_200_prev = close_prev / sma_200_close_prev, # Relative close to SMA 200 of previous day
        high_rel_sma_200_prev = high_prev / sma_200_close_prev, # Relative high to SMA 200 of previous day
        low_rel_sma_200_prev = low_prev / sma_200_close_prev
    ) %>%
    group_by(symbol) %>%
    # Calculate several lags
    mutate(
        close_rel_sma_200_prev_5 = lag(close_rel_sma_200_prev, n = 5), # 10 days lag
        close_rel_sma_200_prev_10 = lag(close_rel_sma_200_prev, n = 10), # 10 days lag
        close_rel_sma_200_prev_20 = lag(close_rel_sma_200_prev, n = 20), # 10 days lag
        close_rel_sma_200_prev_30 = lag(close_rel_sma_200_prev, n = 30), # 30 days lag
        close_rel_sma_200_prev_40 = lag(close_rel_sma_200_prev, n = 40), # 30 days lag
        close_rel_sma_200_prev_50 = lag(close_rel_sma_200_prev, n = 50), # 50 days lag
        close_rel_sma_200_prev_60 = lag(close_rel_sma_200_prev, n = 60), # 50 days lag
        close_rel_sma_200_prev_70 = lag(close_rel_sma_200_prev, n = 70), # 50 days lag
        close_rel_sma_200_prev_80 = lag(close_rel_sma_200_prev, n = 80), # 50 days lag
        close_rel_sma_200_prev_90 = lag(close_rel_sma_200_prev, n = 90), # 50 days lag
        close_rel_sma_200_prev_100 = lag(close_rel_sma_200_prev, n = 100), # 100 days lag

        low_close_rel_sma_200_prev_5 = lag(low_rel_sma_200_prev, n = 5), # 10 days lag
        low_close_rel_sma_200_prev_10 = lag(low_rel_sma_200_prev, n = 10), # 10 days lag
        low_close_rel_sma_200_prev_20 = lag(low_rel_sma_200_prev, n = 20), # 10 days lag  
        low_close_rel_sma_200_prev_30 = lag(low_rel_sma_200_prev, n = 30), # 30 days lag
        low_close_rel_sma_200_prev_40 = lag(low_rel_sma_200_prev, n = 40), # 30 days lag
        low_close_rel_sma_200_prev_50 = lag(low_rel_sma_200_prev, n = 50), # 50 days lag
        low_close_rel_sma_200_prev_60 = lag(low_rel_sma_200_prev, n = 60), # 50 days lag
        low_close_rel_sma_200_prev_70 = lag(low_rel_sma_200_prev, n = 70), # 50 days lag
        low_close_rel_sma_200_prev_80 = lag(low_rel_sma_200_prev, n = 80), # 50 days lag
        low_close_rel_sma_200_prev_90 = lag(low_rel_sma_200_prev, n = 90), # 50 days lag
        low_close_rel_sma_200_prev_100 = lag(low_rel_sma_200_prev, n = 100), # 100 days lag

        high_close_rel_sma_200_prev_5 = lag(high_rel_sma_200_prev, n = 5), # 10 days lag
        high_close_rel_sma_200_prev_10 = lag(high_rel_sma_200_prev, n = 10), # 10 days lag
        high_close_rel_sma_200_prev_20 = lag(high_rel_sma_200_prev, n = 20), # 10 days lag
        high_close_rel_sma_200_prev_30 = lag(high_rel_sma_200_prev, n = 30), # 30 days lag
        high_close_rel_sma_200_prev_40 = lag(high_rel_sma_200_prev, n = 40), # 30 days lag
        high_close_rel_sma_200_prev_50 = lag(high_rel_sma_200_prev, n = 50), # 50 days lag
        high_close_rel_sma_200_prev_60 = lag(high_rel_sma_200_prev, n = 60), # 50 days lag
        high_close_rel_sma_200_prev_70 = lag(high_rel_sma_200_prev, n = 70), # 50 days lag
        high_close_rel_sma_200_prev_80 = lag(high_rel_sma_200_prev, n = 80), # 50 days lag
        high_close_rel_sma_200_prev_90 = lag(high_rel_sma_200_prev, n = 90), # 50 days lag
        high_close_rel_sma_200_prev_100 = lag(high_rel_sma_200_prev, n = 100) # 100 days lag


    )


# Add explanatory variables

df_exp = df_exp %>% 
    group_by(symbol) %>%
    arrange(date, .by_group = TRUE) %>% # Arrange by date within each symbol
    mutate(

        # Lags of VIx
        vix_prev_1 = lag(close, n = 1), # Previous day VIX
        vix_prev_5 = lag(close, n = 1), # Previous day VIX
        vix_prev_10 = lag(close, n = 1), # Previous day VIX
        vix_prev_20 = lag(close, n = 1), # Previous day VIX
        vix_prev_30 = lag(close, n = 1), # Previous day VIX

        # Rel diffs
        vix_d_rel_5 = (vix_prev_1 - lag(close, n = 5)) / lag(close, n = 5), # Month on month change of VIX
        vix_d_rel_10 = (vix_prev_1 - lag(close, n = 10)) / lag(close, n = 10), # Month on month change of VIX
        vix_d_rel_30 = (vix_prev_1 - lag(close, n = 30)) / lag(close, n = 30) # Month on month change of VIX
       

    ) %>% ungroup()

df = df %>%
    left_join(df_exp %>% select(date, vix_prev_1, vix_prev_5, vix_prev_10, vix_prev_20, vix_prev_30, vix_d_rel_5, vix_d_rel_10, vix_d_rel_30), 
              by = c("date")) 




# close / sma_200_close ~ 120% -> appears to make sense to start building cash! -> 50:50
# Otherwise, if close / sma_200_close in (100, 120%) -> DO NOT build cash! -> go long / short 60:40???

# -> each index has a different cut-off!!!!



# Split into training and testing data
df_train = df %>% filter(date < as.Date("2014-01-01"))
df_test = df %>% filter(date > as.Date("2014-01-01"))




# Model training
# 1] Define features
# 2] Run PCA - reduce feature space to relevant signals
# 3] Determine optimal number of components
# 4] Train model on PCA components -> XGBOOST






# Train model on predicted action!!!!

library(tidymodels)
library(xgboost)
library(butcher)
library(future)
#
library(vip)

# Use bagging for predicting error bands
y_iter = 1

# Collect bagging models
l_models = list()
length(l_models) = length(y_iter)

# Start multisession
# future::plan(multisession, workers = 4) # -> probs with renv?
# plan(sequential)

# Iterate over bagged models
for (i in seq_along(y_iter)) {

    print(i)

    # l_models[[i]] <- future({

        # Subset to data for current iteration, i.e. bagged data
        df_train_i = df_train %>% 
            mutate(slide_idx = date) %>%
            arrange(slide_idx, symbol)

        # Create slider for cross validation

        # Expanding window is too much for my laptop
        # resamples = sliding_period(df_train_i, slide_idx, period = "year", lookback = Inf)

        # Look 3 years back and assess 1-3 years ahead
        resamples = sliding_period(df_train_i, slide_idx, period = "year", lookback = 3, assess_start = 1, assess_stop = 3)

        # Define recipe
        rec = recipe(
            action ~ ., 
            data = df_train_i %>%
            ungroup() %>%
            select(
                action,
                close_prev,

                low_prev,
                high_prev,

                low_prev_30,
                high_prev_30,
                sma_200_close_prev,

                close_rel_sma_200_prev_5,
                close_rel_sma_200_prev_10,
                close_rel_sma_200_prev_20,
                close_rel_sma_200_prev_30,
                close_rel_sma_200_prev_40,  
                close_rel_sma_200_prev_50,
                close_rel_sma_200_prev_60,
                close_rel_sma_200_prev_70,
                close_rel_sma_200_prev_80,
                close_rel_sma_200_prev_90,
                close_rel_sma_200_prev_100,

                low_close_rel_sma_200_prev_5,
                low_close_rel_sma_200_prev_10,
                low_close_rel_sma_200_prev_20,
                low_close_rel_sma_200_prev_30,
                low_close_rel_sma_200_prev_40,
                low_close_rel_sma_200_prev_50,
                low_close_rel_sma_200_prev_60,
                low_close_rel_sma_200_prev_70,
                low_close_rel_sma_200_prev_80,
                low_close_rel_sma_200_prev_90,
                low_close_rel_sma_200_prev_100,

                high_close_rel_sma_200_prev_5,
                high_close_rel_sma_200_prev_10,
                high_close_rel_sma_200_prev_20,
                high_close_rel_sma_200_prev_30,
                high_close_rel_sma_200_prev_40,
                high_close_rel_sma_200_prev_50,
                high_close_rel_sma_200_prev_60,
                high_close_rel_sma_200_prev_70,
                high_close_rel_sma_200_prev_80,
                high_close_rel_sma_200_prev_90,
                high_close_rel_sma_200_prev_100,

                # Explanatory variables

                # VIX
                vix_prev_1, vix_prev_5, vix_prev_10, vix_prev_20, vix_prev_30, vix_d_rel_5, vix_d_rel_10, vix_d_rel_30

                )
                ) %>%
            update_role(action, new_role = "outcome") %>% # Set action as outcome
            # step_other(strategy, threshold = 0.01) %>% # Convert low frequency categories to "other"
            step_novel(all_nominal_predictors()) %>% # Handle novel categories
            step_unknown(all_nominal_predictors()) %>% # Handle unknown categories
            step_dummy(all_nominal_predictors()) # Convert categorical variables to dummy variables
        
        # Define hyperparameter tuning
        grid = grid_space_filling(
            trees(range = c(100, 300)),
            tree_depth(range = c(3,10)),
            learn_rate(),
            min_n(),
            loss_reduction(),
            # sample_size = sample_prop(range = c1), #c(0.9, 1.0)
            finalize(mtry(), df_train_i %>% select(-action)),
            size = 10,
            type = "any",
            iter = 1000
        )

        # Define model
        mod_tune = boost_tree(
            mode = "classification",
            engine = "xgboost",
            trees = tune(),
            tree_depth = tune(),
            learn_rate = tune(),
            min_n = tune(),
            loss_reduction = tune(),
            # sample_size = tune(),
            mtry = tune()
        )

        # Define workflow
        wf = workflow() %>%
            add_recipe(rec) %>%
            add_model(mod_tune)

        # Perform cross-validation
        res = wf %>%
            tune_grid(
                resamples = resamples,
                grid = grid,
                metrics = metric_set(accuracy, roc_auc),
                control = control_grid(save_pred = TRUE, verbose = TRUE)
            )

        # Select best hyperparameters
        best_params = select_best(res, metric = "roc_auc")

        # FInalize
        wf_final = finalize_workflow(wf, best_params)

        # Create final fit
        fit_final = wf_final %>%
            fit(data = df_train_i)

        # Reduce size of model with butcher
        fit_final = butcher(fit_final, verbose = FALSE)

        # Store model results
        fit_final

    # })

}

l_models[[1]] = fit_final

vip(fit_final, num_features = 20) # Variable importance plot for first model

# # Extract models from multisession objects
# l_models_test = future::value(l_models)

# # Catch potential errors
# for (i in seq_along(l_models)) {
#     if (inherits(l_models[[i]], "error")) {
#         message(paste("Error in model", i, ":", l_models[[i]]))
#         l_models[[i]] <- NULL # Remove error models
#     }
# }


# Calculate metrics in training data
df_pred = predict(l_models[[1]], new_data = df_train) %>%
    bind_cols(df_train) 

df_pred$action = factor(df_pred$action, levels = levels(df_pred$.pred_class)) # Ensure action is a factor
   
# Accuracy of 61%
# With Vix: 66.2% + expanding window
df_pred %>%
    metrics(truth = action, estimate = .pred_class)

# In training, the general pattern was learned.
conf_mat(df_pred %>% ungroup(), truth = action, estimate = .pred_class)


# Perform bagged prediction
df_pred = lapply(l_models, function(mod_i){
    cbind(df_test, predict(mod_i, new_data = df_test))
})

df_pred = bind_rows(df_pred, .id = "model_id") 

df_pred$action = factor(df_pred$action, levels = levels(df_pred$.pred_class)) # Ensure action is a factor
   
df_pred %>%
   metrics(truth = action, estimate = .pred_class)

head(df_pred)

df_pred %>% ungroup() %>% count(.pred_class)
df_pred %>% ungroup() %>% count(action)

# In test -> check distribution and wrong directions of actions
conf_mat(df_pred %>% ungroup(), truth = action, estimate = .pred_class)

# put if should be call in % && call if should be put in %

# Percent of predicted put options when call would have been better
df_pred %>% filter(.pred_class == "put" & action == "call") %>% 
    summarise(pct = n() / nrow(df_pred %>% filter(action == "call")) * 100)

# Percent of predicted call options, when put would have been better
# in 13-25% of market downturns, the model predcits to go long!
df_pred %>% filter(.pred_class == "call" & action == "put") %>%
    summarise(pct = n() / nrow(df_pred %>% filter(action == "put")) * 100)



# Inspect by symbol!


df_pred %>%
    ggplot(aes(x = date, y = close)) +
    geom_line() +
    geom_line(aes(y = sma_200_close), color = "blue") + # 200 day moving average
    geom_point(data = df_pred %>% filter(.pred_class == "call"), aes(y = close), color = "green", size = 2) + # Buy call option
    geom_point(data = df_pred %>% filter(.pred_class == "put"), aes(y = close), color = "red", size = 2) + # Buy put option
    labs(title = "Barrier Option Strategy", x = "Date", y = "Close Price") +
    theme_minimal() +
    facet_wrap(~ symbol, scales = "free_y") # Facet by symbol




# !!! NEXT STEP !!!
# -> PREDICT ALLOCAION! AND COMPARE!
# -> 50% call + 30% put might not be bad and better than 100% call in put scenario!

