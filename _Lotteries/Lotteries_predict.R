
# Settings ---------------------------------------------------------------------



# Libraries --------------------------------------------------------------------

library(dplyr)
library(tidyr)

library(ggplot2)

library(openxlsx)
# library(readxl)

library(tidyverse)
library(tensorflow)
library(keras3) # library keras has a bug -> needs to be removed!



# Load models ------------------------------------------------------------------

loaded_model = load_model("models/model_lotto.keras")
loaded_model_sz = load_model("models/model_lotto_sz.keras")

# Load hist data ---------------------------------------------------------------

file_hist = "C:/Users/Lang_/OneDrive/Portfolio/Lottery.xlsx"
df_hist = openxlsx::read.xlsx(file_hist, sheet = 2) # Needs to be closed

# Translate date from excel to actual date
df_hist$Date = as.Date(df_hist$Date, origin = "1899-12-30")

n_periods_past = 80

df_hist = df_hist %>%
    arrange(desc(Date)) %>%
    slice(1:n_periods_past)

# Transform to matrix format
df_hist_sz = df_hist %>% select(Date, SZ)
df_hist = df_hist %>% select(-SZ)

# Transform to correct matrix dummy format
df_hist = df_hist %>% 
    mutate(id = row_number()) %>%
    select(-Date) %>%
    pivot_longer(cols = -c(id), names_to = "names", values_to = "number") %>%
    mutate(value = 1) %>%
    select(id, number, value)

df_complete = data.frame(id = 1:n_periods_past, number = rep(1:49, n_periods_past))

x_hist = df_complete %>%
    left_join(df_hist, by = c("id", "number")) %>%
    mutate(value = if_else(is.na(value), 0, 1)) %>%
    pivot_wider(names_from = number, values_from = value) %>%
    select(-id) %>%
    as.matrix()



x_hist = unlist(split(x_hist, 1:nrow(x_hist)), recursive = F, use.name = F)
x_hist = matrix(x_hist, nrow = 1, byrow = T)


df_hist_sz = df_hist_sz %>% 
    # Model was trained on numbers 1:10 -> add 1 for model prediction, then subtract 1
    mutate(SZ = SZ + 1) %>%
    mutate(id = row_number()) %>%
    select(-Date) %>%
    pivot_longer(cols = -c(id), names_to = "names", values_to = "number") %>%
    mutate(value = 1) %>%
    select(id, number, value)

df_complete = data.frame(id = 1:n_periods_past, number = rep(1:10, n_periods_past))

x_hist_sz = df_complete %>%
    left_join(df_hist_sz, by = c("id", "number")) %>%
    mutate(value = if_else(is.na(value), 0, 1)) %>%
    pull(value)



x_hist_sz = matrix(x_hist_sz, nrow = 1, byrow = T)



# Predict ----------------------------------------------------------------------

m_predict = loaded_model %>% predict(x_hist)
m_predict_sz = loaded_model_sz %>% predict(x_hist_sz)

n_periods_future = 16

# Select prediction for next period
# m_predict[1:49]

# calc_fields <- function(m) {
  
#   # 8 most likely numbers
#   top_numbers = order(m, decreasing = T)[1:8]

#   # Take the 8 most likely numbers -> and split into 6 numbers
#   top_numbers_comb = combn(top_numbers, 6)

#   # Calculate the likelihood of each set
#   top_comb = apply(top_numbers_comb, 2, function(m_) {m[m_]}) %>%
#     colSums() %>%
#     order(., decreasing = T)

#   # Select top 15 sets
#   top_numbers_comb[,top_comb[1:15]]

# }

# calc_fields(m_predict[1:49])
# -190.95

# VS! Play the fields of the next 15 periods!


idx = rep(1:n_periods_future, each = 49)
idx_sz = rep(1:n_periods_future, each = 10)

m_predict_split = lapply(1:n_periods_future, function(i) {
  order(m_predict[,idx == i], decreasing = T)[1:6] %>% sort()
}) %>% do.call("rbind",.)


# m_predict_sz_split = lapply(1:n_periods_future, function(i) {
#   order(m_predict_sz[,idx_sz == i], decreasing = T)[1]
# }) %>% unlist()

sz = order(m_predict_sz[,idx_sz == 1], decreasing = T)[1]

# Can play only 1 Superzahl! but 15 fields!
# Subtract 1 from SZ -> see previous steps
# print(paste0("SZ: ", sz - 1))
print(paste0("SZ: ", 7))
cbind(m_predict_split)[1:15,] %>% print()

stop("TODO: SELECT A SZ AND STICK TO IT! It's do difficult to predict the SZ. Take one to be sure about 10% probability!")
# Or just take the number that was the least frequent in the last 50 draws?

# Most probable numbers across all future periods
# -> sum probabilities!
# -> would have been a worse fit!
# lapply(1:n_periods_future, function(i) {
#  m_predict[,idx == i]
# }) %>% do.call("rbind",.) %>%
# colSums() %>%
# order(., decreasing = T)


