# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#
# Predict next lottery numbers
# PL - 03.01.2025 - V 0.0.1
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Notes ---------------------------------------------------------------------
# https://tensorflow.rstudio.com/guides/tensorflow/


# Train two models -> 1 for the first 6 numbers and 1 for the last number



# 2] Fill out more than 1 field! -> Setup system -> spend 100€ per week

# 3] Hypertune the layers!

# 4] Save the model and apply it to the actual lottery numbers! and predict next set of numbers to play!
# 5] Create blog and track spendings and payouts! -> show my commitment to the system



# Settings ---------------------------------------------------------------------



# Libraries --------------------------------------------------------------------

library(dplyr)
library(tidyr)

library(tidyverse)
library(tensorflow)
library(keras3) # library keras has a bug -> needs to be removed!



# Data ---------------------------------------------------------------------


## Create random lottery samples -------------------------------------------------
# German lottery -> 6 numbers from 1 to 49 + 1 number from 0 to 9

n_periods_future = 16 # 2 months = 16 games
n_periods_past = n_periods_future * 5
n_scens = 100000


# Create output and input matrices to collect simulation results
m_output = lapply(1:n_periods_future, function(x) matrix(data = 0, nrow = n_scens, ncol = 49))
m_input = lapply(1:n_periods_past, function(x) matrix(data = 0, nrow = n_scens, ncol = 49))

m_output_sz = lapply(1:n_periods_future , function(x) matrix(data = 0, nrow = n_scens, ncol = 10))
m_input_sz = lapply(1:n_periods_past , function(x) matrix(data = 0, nrow = n_scens, ncol = 10))

for (i in 1:n_scens) {

  # Fill the past ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## numbers in fields

  # Create single lottery sample for n_periods
  m_sample = matrix(
      lapply(1:n_periods_past, function(i) sample(1:49, 6, replace = F)) %>% do.call("rbind",.),
      nrow = n_periods_past,
  )

  # input = all previous draws
  for (j in 1:n_periods_past) {
    m_input[[j]][i,m_sample[j,]] = 1
  }


  ## Superzahl
  m_sample_sz = matrix(
    lapply(1:n_periods_past, function(i) sample(0:9, 1) + 1) %>% do.call("rbind",.),
    nrow = n_periods_past,
  )

  for (j in 1:n_periods_past) {
    m_input_sz[[j]][i,m_sample_sz[j,]] = 1
  }



  # Fill the future ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Create single lottery sample for n_periods
  m_sample = matrix(
      lapply(1:n_periods_future, function(i) sample(1:49, 6, replace = F)) %>% do.call("rbind",.),
      nrow = n_periods_future,
  )

  # input = all previous draws
  for (j in 1:n_periods_future) {
    m_output[[j]][i,m_sample[j,]] = 1
  }


  ## Superzahl
  m_sample_sz = matrix(
    lapply(1:n_periods_future, function(i) sample(0:9, 1) + 1) %>% do.call("rbind",.),
    nrow = n_periods_future,
  )

  for (j in 1:n_periods_future) {
    m_output_sz[[j]][i,m_sample_sz[j,]] = 1
  }
  
}

# Cbind all input matrices

m_input = do.call("cbind", m_input)
m_input_sz = do.call("cbind", m_input_sz)

m_output = do.call("cbind", m_output)
m_output_sz = do.call("cbind", m_output_sz)



## Train neural network ----------------------------------------------------------

## split into train and test
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(m_input), replace=TRUE, prob=c(0.7,0.3))

x_train = m_input[sample,]
x_test = m_input[!sample,]

# Superzahl
x_train_sz = m_input_sz[sample,]
x_test_sz = m_input_sz[!sample,]

y_train = m_output[sample,]
y_test = m_output[!sample,]

# Superzahl
y_train_sz = m_output_sz[sample,,drop = F]
y_test_sz = m_output_sz[!sample,,drop = F]





# Train model

model <- keras_model_sequential() %>%
  layer_dense(units = ncol(y_train), activation = "softmax")

model_sz <- keras_model_sequential() %>%
  layer_dense(units = ncol(y_train_sz), activation = "softmax")


model %>% compile(
  optimizer = "adam",
  loss =  "mse",
  metrics = "accuracy"
)

model_sz %>% compile(
  optimizer = "adam",
  loss =  "categorical_crossentropy",
  metrics = "accuracy"
)


model %>% fit(x_train, y_train, epochs = 5)
model %>% evaluate(x_test, y_test)

model_sz %>% fit(x_train_sz, y_train_sz, epochs = 5)
model_sz %>% evaluate(x_test_sz, y_test_sz)






# Check number of correctly predicted numbers -> account for tuples!
m_predict = model %>% predict(x_test)
m_predict_sz = model_sz %>% predict(x_test_sz)


# Account for multiple future rounds -> n_periods_future
# -> split into subgames

idx = rep(1:n_periods_future, each = 49)
idx_sz = rep(1:n_periods_future, each = 10)

m_predict_split = lapply(1:n_periods_future, function(i) {
  m_predict[,idx == i]
})

m_y_test_split = lapply(1:n_periods_future, function(i) {
  y_test[,idx == i]
})

m_predict_sz_split = lapply(1:n_periods_future, function(i) {
  m_predict_sz[,idx_sz == i]
})

m_y_test_sz_split = lapply(1:n_periods_future, function(i) {
  y_test_sz[,idx_sz == i]
})


# Introduce possibliity to field out more than 1 field!
# -> in fact, fill out all 15 fields!
# -> which method to use? 
# 1] -> take the 7 most likely numbers and create all combinations of 6 numbers!
# 2] maximize the number of possible combinations of 4!

m_predict_split[[1]][1,]





# Translate to actual set of 6 numbers
m_predict_trans = lapply(m_predict_split, function(m) {apply(m, 1, function(x) order(x, decreasing = T)[1:6]) %>% t()})
m_predict_sz_trans = lapply(m_predict_sz_split, function(m) {apply(m, 1, which.max)})

y_test_trans = lapply(m_y_test_split, function(m) {apply(m, 1, function(x) order(x, decreasing = T)[1:6]) %>% t()})
y_test_sz_trans = lapply(m_y_test_sz_split, function(m) {apply(m, 1, which.max)})


m_correct_numbers = lapply(1:n_periods_future, function(i) {

  lapply(1:nrow(m_predict_trans[[i]]), function(j) {
    length(intersect(m_predict_trans[[i]][j,], y_test_trans[[i]][j,]))
  }) %>% unlist() 

})

m_correct_numbers = do.call("cbind", m_correct_numbers)


m_correct_numbers_sz = lapply(1:n_periods_future, function(i) {

  lapply(1:length(m_predict_sz_trans[[i]]), function(j) {
    length(intersect(m_predict_sz_trans[[i]][j], y_test_sz_trans[[i]][j]))
  }) %>% unlist() 

})

m_correct_numbers_sz = do.call("cbind", m_correct_numbers_sz)



# Assign pay-offs to each number of correct numbers

# https://www.onlinelotto.net/lottoquoten-und-gewinne/
# 6 -> 575000 -> + SZ -> 9000000
# 5 -> 3300 -> + SZ -> 10000
# 4 -> 40 -> + SZ -> 190
# 3 -> 10 -> + SZ -> 21
# 2 -> 0 -> + SZ -> 5
# 1 -> 0
# 0 -> 0

# [pay_off, pay_off_sz]
v_pay_off = c(0, 0, 0, 10, 40, 3300, 575000, 0, 0, 5, 21, 190, 10000, 9000000)

m_correct_numbers_comb = m_correct_numbers + m_correct_numbers_sz * 7

m_pay_off = apply(m_correct_numbers_comb, 2, function(x) v_pay_off[x + 1])

# Calculate the costs for playing
# https://www.lotto-bayern.de
# 15 fields => 18€ + 0.5€ fees = 18.5€
# 1.2€ for each field + 0.5€ basic fees




#m_pay_off = v_pay_off[m_correct_numbers + 1]
sprintf("Expected payoff: %.2f", mean(rowSums(m_pay_off) - 1.7 * n_periods_future) %>% round(.,2))


# What is the mean if we limit the payoffs from getting a 6! -> simulate a constant revenue stream
sprintf("Expected payoff without jackpots: %.2f", mean(pmin(m_pay_off, 3300) - 1.7 * n_periods_future) %>% round(.,2))


# Calculate median value! and inspect distribution!
# -> probability to make a profit! -> expected value might be misleading
sprintf("Median payoff: %.2f", median(rowSums(m_pay_off) - 1.7 * n_periods_future) %>% round(.,2))



stop("End of script")


# % of correct 4 numbers
prob_4 = lapply(1:nrow(m_predict_trans), function(i) {
  length(intersect(m_predict_trans[i,], y_test_trans[i,]))
}) %>% unlist() %>% table() %>% as.numeric() %>% `[`(4) / nrow(m_predict)

# % of corret 5 numbers, 3 numbers, etc. -> exclude superzahl!




# Next steps:
# -> play 3 set with 3 most likely Superzahls
# -> on each set, fill out all 6 fields -> combination of most likely numbers! -> take 7 most likely -> create all combinations!
# -> OR!!!! with the most likely numbers -> create the most possible combination 4 correct values!

# -> when does my expected value and median value become positive?



stop("Rethink the testing metric!")
# Each proposition of 6 numbers -> is this just one field?
# 1] Better to create more fields by rounding differentely?
# 2] Or better to predict more set of 6 numbers?


# Number of fields I need to play to get 4 numbers correct
1 / prob_4

# 4 correct -> 50€
50 - (1 / prob_4) * 1.8

# current loss around 50 € -> getting closer



# How do decide number and type of layers? 
# -> play around? hyperparameter tuning? or just try as many as possible?




# Back up ---------------------------------------------------------------------

# Penalize
# -> negative numbers
# -> duplicates -> does not make sense for floats
# -> define range! Especially for last column! OR! Split into two separate networks!

# # Define the custom loss function
# custom_loss <- function(y_true, y_pred) {

#   # Calculate the mean squared error between the true and predicted values
#   mse <- tf$math$reduce_mean(tf$math$square(y_pred - y_true))
  
#   # Calculate the penalty term for negative values
#   penalty_neg_values <- tf$math$reduce_mean(tf$math$square(tf$math$minimum(y_pred, 0)))


#   # Apply a custom scaling factor to the loss
#   loss <- (
#     mse
#     + penalty_neg_values * 20
#   )
  
#   # Return the custom loss value
#   return(loss)
# }


# Work with characters????

# Need to make sure that test and training data set have the same dimensions!

# -> n_periods * 0:49



# x_train = matrix(to_categorical(as.character(x_train)), nrow = nrow(x_train), byrow = T)
# x_test = matrix(to_categorical(as.character(x_test)), nrow = nrow(x_test), byrow = T)

# x_train_sz = matrix(to_categorical(as.character(x_train_sz)), nrow = nrow(x_train_sz), byrow = T)
# x_test_sz = matrix(to_categorical(as.character(x_test_sz)), nrow = nrow(x_test_sz), byrow = T)

# y_train = matrix(to_categorical(as.character(y_train)), nrow = nrow(y_train), byrow = T)
# y_test = matrix(to_categorical(as.character(y_test)), nrow = nrow(y_test), byrow = T)

# y_train_sz = matrix(to_categorical(as.character(y_train_sz)), nrow = nrow(y_train_sz), byrow = T)
# y_test_sz = matrix(to_categorical(as.character(y_test_sz)), nrow = nrow(y_test_sz), byrow = T)