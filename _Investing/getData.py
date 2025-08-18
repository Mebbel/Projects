import yfinance as yf
dat = yf.Ticker("MSFT")


dat = yf.Ticker("MSFT")
dat.info
dat.calendar
dat.analyst_price_targets
dat.quarterly_income_stmt


print(dat.revenue_estimate)

# earnings_estimate
# growth_estimates