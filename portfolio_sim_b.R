ini <- 2500000
ins_v <- 836965 * 0.75
rate <- 112.5
maintenance <- 0.0025
  
### read.csv
setwd("~/pj/fsim/")
df_p <- read.csv("portfolio.csv")
df_p <- df_p %>%
  mutate(yield = allocation * (coupon/100)) %>%
  mutate(amount = allocation * (price/100)) %>%
  mutate(capacity = allocation * (surrender/100)) %>%
  mutate(net_y = coupon / price * 100 )

df_p2 <- df_p %>%
  filter(cf == "y") %>%
  mutate(tax = yield * 0.2)

cf_ratio <- sum(df_p2$yield) / sum(df_p$yield) * 100

df_l <- read.csv("loan_sim.csv")
df_l <-df_l %>%
  mutate(pay = allocation * (interest/100))

loan_ttl <- sum(df_l$allocation) / rate 
max_capa <- sum(df_p$capacity) + ins_v
cf_ttl <- sum(df_p2$yield)
yield_ttl <- (sum(df_p$yield) 
              - (sum(df_l$pay) / rate) 
              - sum(df_p2$tax)
              - (sum(df_p$amount) * maintenance))

yield_rate <- yield_ttl / ini * 100

yield_ttl_sg <- (sum(df_p$yield) 
              - (sum(df_l$pay) / rate) 
              - (sum(df_p$amount) * maintenance))

yield_rate_sg <- yield_ttl_sg / ini * 100

df_summary <- data.frame(
  item = c("Net Asset","Yield TTL","Yield Ratio",
           "Yield TTL(SG)","Yield Ratio(SG)","Cash Income Ratio"),
  value = c(round(ini/1000,digits = 1),round(yield_ttl/1000,digits = 1),
            round(yield_rate,digits = 1),
            round(yield_ttl_sg/1000, digits = 1),
            round(yield_rate_sg,digits = 1),
            round(cf_ratio,digits=1))
)

