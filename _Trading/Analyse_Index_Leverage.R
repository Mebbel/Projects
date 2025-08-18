
library(tidyquant)
library(tidyverse)
library(ggplot2)

library(RQuantLib)
library(greeks)



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

# Calculate expected leverage of a barrier option
val_0 = calc_BarrierOption(x = 21711, strike = 20748, maturity = 99, barrier = 20748)
val_1 = calc_BarrierOption(x = 21711 * 1.01, strike = 20748, maturity = 99, barrier = 20748)

# Expected increase of the price if the underlying increases by 1% -> leverage
leverage = (val_1 / val_0 - 1) / 0.01



