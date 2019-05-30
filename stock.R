install.packages("stocks")
library(stocks)
beta_trailing50("TLT")
beta_trailing50("GOOG")
beta_trailing50("AAPL")

convert_gain(gain = 0.08, units.in = 70, units.out = 252)
convert_gain(gain = 2, units.in = 13, units.out = 1)

growth_graph(c("VFINX", "BRK-B"))
growth_graph(c("NFLX", "AAPL","AMZN","GOOG"))
growth_graph(c("NFLX", "AAPL","AMZN","GOOG","MSFT"))
growth_graph(c("NFLX", "AAPL","AMZN","GOOG","MSFT","FB"))
growth_graph(c("AAPL","AMZN","GOOG","MSFT"))

growth_graph(c("MSFT","AAPL","ORCL"))
growth_graph(c("GOOG","CRM"))
growth_graph(c("NFLX","CRM","AMZN"))

growth_graph(c("T","TMUS","VZ","S"))

growth_graph(c("MSFT","AAPL","ORCL","HPE"))
FB

load_prices(c("NFLX", "AMZN"))

highyield_etfs

metrics(tickers = c("SPY", "SSO", "UPRO"))
metrics(tickers = c("NFLX", "AAPL","AMZN","GOOG"))