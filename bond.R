ini <- 2500000
rate <- 112.5
maintenance <- 0.0025
  
### read.csv
setwd("~/pj/fsim/")
df_p <- read.csv("portfolio.csv")
df_p <- df_p %>%
  mutate(yield = allocation * (coupon/100)) %>%
  mutate(amount = allocation * (price/100))
df_p2 <- df_p %>%
  filter(cf == "y") %>%
  mutate(tax = yield * 0.2)

df_l <- read.csv("loan.csv")
df_l <-df_l %>%
  mutate(pay = allocation * (interest/100))

cf_ttl <- sum(df_p2$yield)
yield_ttl <- (sum(df_p$yield) 
              - (sum(df_l$pay) / rate) 
              - sum(df_p2$tax)
              - (sum(df_p$amount) * maintenance))

yield_rate <- yield_ttl / ini * 100

cf_ttl
yield_ttl
yield_rate