install.packages("readxl")
install.packages("highcharter")
install.packages("tidyquant")
install.packages("timetk")
install.packages("tibbletime")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("scales")
install.packages("flexdashboard")
install.packages("broom")

library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)
library(broom)

#symbols <- c("SPY","EFA","IJS","EEM","AGG")
symbols <- c("AAPL","GOOG","FB","AMZN","MSFT")

today <- Sys.Date()
today_char <- format(today, "%Y-%m-%d")

prices <-
  getSymbols(symbols,
             src ='yahoo',
             from = "2014-12-31",
             to = today_char,
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

prices[,symbols[1]]

prices_monthly <- to.monthly(prices,
                             indexAt = "lastof",
                             OHLC = FALSE)

prices_monthly %>% head(3)

asset_returns_xts <-
  Return.calculate(prices_monthly,
                   method = "log") %>%
  na.omit()

asset_returns_xts_discrete <-
  Return.calculate(prices_monthly,
                   method = "discrete") %>%
  na.omit()

asset_returns_dplyr_byhand <-
  prices %>%
  to.monthly(prices,indexAt = "lastof",OHLC = FALSE) %>%
  data.frame(date = index(.)) %>%
  remove_rownames() %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  mutate(returns = (log(prices) - log(lag(prices)))) %>%
  select(-prices) %>%
  spread(asset, returns) %>%
  select(date, symbols) %>%
  na.omit()

asset_returns_tq_builtin <-
  prices %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  gather(asset,prices, -date) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn,
               period = "monthly",
               type = "log") %>%
  spread(asset, monthly.returns) %>%
  select(date,symbols) %>%
  slice(-1)

asset_returns_tq_tbltime <-
  prices %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  as_tbl_time(index = date) %>%
  as_period(period = "month",
            side = "end") %>%
  gather(asset,prices, -date) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn,
               type = "log") %>%
  spread(asset, monthly.returns) %>%
  select(date,symbols) %>%
  slice(-1)

index(asset_returns_xts)

asset_returns_long <-
  asset_returns_dplyr_byhand %>%
  gather(asset, returns, -date) %>%
  group_by(asset)

highchart(type = "stock") %>%
  hc_title(text = "MGAFA Price") %>%
  hc_add_series(asset_returns_xts[,symbols[1]], name = symbols[1]) %>%
  hc_add_series(asset_returns_xts[,symbols[2]], name = symbols[2]) %>%
  hc_add_series(asset_returns_xts[,symbols[3]], name = symbols[3]) %>%
  hc_add_series(asset_returns_xts[,symbols[4]], name = symbols[4]) %>%
  hc_add_series(asset_returns_xts[,symbols[3]], name = symbols[5]) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

hc_hist <- hist(asset_returns_xts[,symbols[1]],
                breaks = 50,
                plot = FALSE)

hchart(hc_hist, color = "cornflowerblue") %>%
  hc_title(text =
             paste(symbols[1],
                   "Log Returns Distribution",
                   sep = ":")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE)

hc_hist_fun <- function(n = 1, object, color){
  hist(asset_returns_xts[,symbols[n]],
       breaks = 50,
       plot = FALSE)
  
  hchart(hc_hist, color = color) %>%
    hc_title(text =
               paste(symbols[n],
                     "Log Returns Distribution",
                     sep = ":")) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = FALSE)
  
}

hc_hist_fun(1, asset_returns_xts, "cornflowerblue")
hc_hist_fun(2, asset_returns_xts, "green")
hc_hist_fun(3, asset_returns_xts, "pink")
hc_hist_fun(4, asset_returns_xts, "purple")
hc_hist_fun(5, asset_returns_xts, "yellow")

map(1:5, hc_hist_fun, asset_returns_xts, "blue")

asset_returns_long %>%
  ggplot(aes(x = returns, fill=asset)) +
  geom_histogram(alpha=0.45, binwidth = .005) +
  ggtitle("Monthly Returns 5 years")

asset_returns_long %>%
  ggplot(aes(x = returns, fill=asset)) +
  geom_histogram(alpha=0.45, binwidth = .01) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns 5 years") +
  theme_update(plot.title = element_text(hjust = 0.5))

asset_returns_long %>%
  ggplot(aes(x = returns, colour=asset)) +
  geom_density(alpha=1) +
  ggtitle("Monthly Returns Density") +
  xlab("monthly returns") +
  ylab("distribution")
  theme_update(plot.title = element_text(hjust = 0.5))
  
asset_returns_long %>%
  ggplot(aes(x = returns)) +
  geom_density(aes(color = asset), alpha=1) +
  geom_histogram(aes(fill = asset), alpha = 0.45, binwidth = .01) +
  guides(fill = FALSE) +
  facet_wrap(~asset)
  ggtitle("Monthly Returns Density") +
  xlab("monthly returns") +
  ylab("distribution")
  theme_update(plot.title = element_text(hjust = 0.5))
  
w <- c(0.35,
       0.25,
       0.20,
       0.15,
       0.05)

tibble(w, symbols)

tibble(w, symbols) %>%
  summarise(total_weight = sum(w))

w_1 <- w[1]
w_2 <- w[2]
w_3 <- w[3]
w_4 <- w[4]
w_5 <- w[5]

asset1 <- asset_returns_xts[,1]
asset2 <- asset_returns_xts[,2]
asset3 <- asset_returns_xts[,3]
asset4 <- asset_returns_xts[,4]
asset5 <- asset_returns_xts[,5]

portfolio_returns_byhand <- 
  (w_1 * asset1) +
  (w_2 * asset2) +
  (w_3 * asset3) +
  (w_4 * asset4) +
  (w_5 * asset5)

names(portfolio_returns_byhand) <- "returns"

help(Return.portfolio)
portfolio_returns_xts_rebalanced_monthly <-
  Return.portfolio(asset_returns_xts,
                   weights = w,
                   rebalance_on = "months") %>%
  `colnames<-`("returns")

portfolio_returns_dplyr_byhand <- 
  asset_returns_long %>% 
  group_by(asset) %>% 
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5]),
         weighted_returns = returns * weights) %>% 
  group_by(date) %>% 
  summarise(returns = sum(weighted_returns))

portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")


portfolio_returns_dplyr_byhand %>% 
  rename(tidyverse = returns) %>% 
  mutate(equation = coredata(portfolio_returns_byhand),
         tq = portfolio_returns_tq_rebalanced_monthly$returns,
         xts = coredata(portfolio_returns_xts_rebalanced_monthly)) %>%
  mutate_if(is.numeric, funs(round(., 3))) 

highchart(type = "stock") %>%
  hc_title(text = "MGAFA Total Price") %>%
  hc_add_series(portfolio_returns_xts_rebalanced_monthly[,1,name = "Total"])

highchart(type = "stock") %>%
  hc_title(text = "MGAFA Total Price") %>%
  hc_add_series(portfolio_returns_xts_rebalanced_monthly$returns,
                name = "Rebalanced Monthly",
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

hc_portfolio <-
  hist(portfolio_returns_xts_rebalanced_monthly$returns,
       breaks = 50,
       plot = FALSE)

hchart(hc_portfolio,
       color = "cornflowerblue",
       name = "Portfolio") %>%
  hc_title(text = "Portfolio Returns Distribution") %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE)

portfolio_returns_tq_rebalanced_monthly %>% 
  ggplot(aes(x = date, y = returns)) +
  geom_point(colour = "cornflowerblue") +
  xlab("date") +
  ylab("monthly return") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Portfolio Returns Scatter") +
  scale_x_date(breaks = pretty_breaks(n=6))

asset_returns_long %>% 
  ggplot(aes(x = returns,
            fill = asset)) +
  geom_histogram(alpha = 0.15,
                 binwidth = .01) +
  geom_histogram(data = portfolio_returns_tq_rebalanced_monthly,
                 fill = "cornflowerblue",
                 binwidth = .01) +
  ggtitle("Portfolio and Asset Monthly Returns") +
  theme_update(plot.title = element_text(hjust = 0.5))

portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = .01,
                 colour = "cornflowerblue",
                 fill = "cornflowerblue") +
  geom_density(alpha = 1, color = "red") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Portfolio Histogram and Density")


covariance_matrix <- cov(asset_returns_xts)
round(covariance_matrix,5)
sd_matrix_algebra <- sqrt(t(w) %*% covariance_matrix %*% w)
sd_matrix_algebra_percent <-
  round(sd_matrix_algebra * 100, 2) %>% 
  `colnames<-`("starndard deviation")

sd_matrix_algebra_percent[1,]

portfolio_sd_xts_builtin <-
  StdDev(asset_returns_xts, weights = w)

portfolio_sd_xts_builtin_percent <-
  round(portfolio_sd_xts_builtin * 100,2)

portfolio_sd_tidy_builtin_percent <-
  portfolio_returns_dplyr_byhand %>% 
  summarise(
    sd = sd(returns),
    sd_byhand =
      sqrt(sum((returns - mean(returns))^2/(nrow(.)-1)))) %>% 
  mutate(dplyr = round(sd,4) * 100,
         dplyr_byhand = round(sd_byhand,4)*100)

portfolio_sd_tidy_builtin_percent %>% 
  select(dplyr,dplyr_byhand)

portfolio_sd_tidyquant_builtin_percent <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  tq_performance(Ra = returns,
                 Rb = NULL,
                 performance_fun = table.Stats) %>% 
  select(Stdev) %>% 
  mutate(tq_sd = round(Stdev,4) * 100)

portfolio_sd_tidy_builtin_percent %>% 
  select(dplyr, dplyr_byhand) %>% 
  mutate(xts_buitin = portfolio_sd_xts_builtin_percent,
         matrix = sd_matrix_algebra_percent,
         tq = portfolio_sd_tidyquant_builtin_percent$tq_sd)

portfolio_returns_dplyr_byhand %>% 
  ggplot(aes(x = date, y = returns)) +
  geom_point(color = "cornflowerblue") +
  scale_x_date(breaks = pretty_breaks(n = 6)) +
  ggtitle("Scatterplot of Returns by Date") +
  theme(plot.title = element_text(hjust = 0.5))

sd_plot <- 
  sd(portfolio_returns_tq_rebalanced_monthly$returns)
mean_plot <-
  mean(portfolio_returns_tq_rebalanced_monthly$returns)

##Fig4.2
portfolio_returns_tq_rebalanced_monthly %>% 
  mutate(hist_col_red =
           if_else(returns < (mean_plot - sd_plot),
                   returns, as.numeric(NA)),
         hist_col_green =
           if_else(returns > (mean_plot + sd_plot),
                   returns, as.numeric(NA)),
         hist_col_blue =
           if_else(returns > (mean_plot - sd_plot) &
                   returns < (mean_plot + sd_plot),
                   returns, as.numeric(NA))
         ) %>% 
  ggplot(aes(x = date)) +
  geom_point(aes(y = hist_col_red),
             color = "red") +
  geom_point(aes(y = hist_col_green),
             color = "green") + 
  geom_point(aes(y = hist_col_blue),
             color = "blue") +
  labs(title = "Colored Scatter", y = "monthly returns") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

###Fig4.3
portfolio_returns_tq_rebalanced_monthly %>% 
  mutate(hist_col_red =
           if_else(returns < (mean_plot - sd_plot),
                   returns, as.numeric(NA)),
         hist_col_green =
           if_else(returns > (mean_plot + sd_plot),
                   returns, as.numeric(NA)),
         hist_col_blue =
           if_else(returns > (mean_plot - sd_plot) &
                     returns < (mean_plot + sd_plot),
                   returns, as.numeric(NA))
  ) %>% 
  ggplot(aes(x = date)) +
  geom_point(aes(y = hist_col_red),
             color = "red") +
  geom_point(aes(y = hist_col_green),
             color = "green") + 
  geom_point(aes(y = hist_col_blue),
             color = "blue") +
  
  geom_hline(yintercept = (mean_plot + sd_plot),
             color = "purple",
             linetype = "dotted") +
  geom_hline(yintercept = (mean_plot - sd_plot),
             color = "purple",
             linetype = "dotted")
  labs(title = "Colored Scatter with Line", y = "monthly returns") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

###Fig4.4
asset_returns_long %>% 
  group_by(asset) %>% 
  summarize(sd = 100 * sd(returns)) %>% 
  add_row(asset = "Portfolio",
          sd = portfolio_sd_tidy_builtin_percent$dplyr) %>% 
  ggplot(aes(x = asset,
             y = sd,
             colour = asset)) +
  geom_point() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(
    aes(x = "Portfolio",
        y =
          portfolio_sd_tidy_builtin_percent$dplyr + .2),
    label = "Portfolio",
    color = "cornflowerblue")+
  labs(y = "standard deviation")

###Fig4.5
asset_returns_long %>% 
  group_by(asset) %>% 
  summarise(expeced_return = mean(returns),
            stand_dev = sd(returns)) %>% 
  add_row(asset = "Portfolio",
          stand_dev =
            sd(portfolio_returns_tq_rebalanced_monthly$returns),
          expeced_return =
            mean(portfolio_returns_tq_rebalanced_monthly$returns)) %>% 

  ggplot(aes(x = stand_dev,
             y = expeced_return,
             color = asset)) +
  geom_point(size = 2) +
  geom_text(
    aes(x =
          sd(portfolio_returns_tq_rebalanced_monthly$returns) * 1.05,
        y =
          mean(portfolio_returns_tq_rebalanced_monthly$returns),
        label = "Portfolio")) +
  ylab("expected return") +
  xlab("standard deviation") +
  ggtitle("Expected Monthly Returns versus Risk") +
  scale_y_continuous(labels = function(x){ paste0(x,"%")}) +
  theme_update(plot.title = element_text(hjust = 0.5))

#### cor

prices_test <-
  getSymbols(c("AAPL","MSFT"),
             src = 'yahoo',
             from = '2019-5-6',
             to = '2019-5-10',
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>% 
  reduce(merge) %>%
  `colnames<-`(c("AAPL","MSFT"))

asset_returns_cov_test <-
  Return.calculate(prices_test,
                   method ="log") %>% 
  na.omit()

asset_returns_cov_test_d <-
  Return.calculate(prices_test,
                   method ="discrete") %>% 
  na.omit()

window <- 24

port_rolling_sd_xt <-
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = sd,
            width = window) %>% 
  na.omit() %>% 
  `colnames<-`("rolling_sd")

portfolio_returns_xts_rebalanced_monthly %>% head(24) %>% sd

port_rolling_sd_tidy_does_not_work <-
  portfolio_returns_dplyr_byhand %>% 
  mutate(rolling_sd = rollapply(returns,
                                FUN = sd,
                                width = window,
                                fill = NA)) %>% 
  select(date, rolling_sd) %>% 
  na.omit()

sd_roll_24 <-
  rollify(sd, window = window)

port_rolling_sd_tidy_tibbletime <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  as_tbl_time(index = date) %>% 
  mutate(sd = sd_roll_24(returns)) %>% 
  select(-returns) %>% 
  na.omit()

port_rolling_sd_tq <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  tq_mutate(mutate_fun = rollapply,
            width = window,
            FUN = sd,
            col_rename = "rolling_sd") %>% 
  select(date, rolling_sd) %>% 
  na.omit()

port_rolling_sd_tidy_tibbletime %>% 
  mutate(sd_tq = port_rolling_sd_tq$rolling_sd,
         sd_xts = round(port_rolling_sd_xt$rolling_sd,4))

port_rolling_sd_xts_hc <-
  round(port_rolling_sd_xt,4) * 100

library(highcharter)
highchart(type = "stock") %>% 
  hc_title(text = "24-Month Rolling Volatility") %>%
  hc_add_series(port_rolling_sd_xts_hc,
                color = "cornflowerblue") %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_yAxis(
    labels = list(format = "{value}%"),
    opposite = FALSE  ) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = TRUE)

library(ggplot2)
port_rolling_sd_tq %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = rolling_sd), color = "cornflowerblue") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = pretty_breaks(n = 8))  +
  labs(title = "Rolling Standard Deviation", y = "") +
  theme(plot.title = element_text(hjust = 0.5))


skew_xts <-
  skewness(portfolio_returns_xts_rebalanced_monthly$returns)

skew_tidy <- 
  portfolio_returns_tq_rebalanced_monthly %>% 
  summarise(skew_builtin = skewness(returns),
            skew_byhand =
              (sum((returns - mean(returns))^3)/length(returns))/
              ((sum((returns-mean(returns))^2)/length(returns)))^(3/2)
            ) %>% 
  select(skew_builtin,skew_byhand)

skew_tidy %>% 
  mutate(xts = coredata(skew_xts)) %>% 
  mutate_all(funs(round(.,3)))

portfolio_returns_tq_rebalanced_monthly %>% 
  ggplot(aes(x = returns)) +
  geom_histogram(alpha = .7,
                 binwidth = .003,
                 fill = "cornflowerblue",
                 color = "cornflowerblue") +
  scale_x_continuous(breaks = 
                       pretty_breaks(n = 10))

portfolio_returns_tq_rebalanced_monthly %>% 
  mutate(hist_col_red =
           if_else(returns < (mean(returns) - 2*sd(returns)),
                   returns, as.numeric(NA)),
         returns =
           if_else(returns > (mean(returns) - 2*sd(returns)),
                   returns, as.numeric(NA))
         ) %>% 
  ggplot() +
  geom_histogram(aes(x = hist_col_red),
                 alpha = .7,
                 binwidth = .003,
                 fill = "red",
                 color = "red") +
  geom_histogram(aes(x = returns),
                 alpha = .7,
                 binwidth = .003,
                 fill = "cornflowerblue",
                 color = "cornflowerblue") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("monthly returns")

portfolio_density_plot <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  ggplot(aes(x = returns)) +
  stat_density(geom = "line",
               alpha =1,
               colour = "cornflowerblue")

shaded_area_data <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>% 
  filter(x <
           mean(portfolio_returns_tq_rebalanced_monthly$returns))

portfolio_density_plot_shaded <-
  portfolio_density_plot +
  geom_area(data = shaded_area_data,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5)

median <-
  median(portfolio_returns_tq_rebalanced_monthly$returns)

mean <-
  mean(portfolio_returns_tq_rebalanced_monthly$returns)

median_line_data <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>% 
  filter( x <= median)

portfolio_density_plot_shaded +
  
  geom_segment(data = shaded_area_data,
               aes(x = mean,
                   y = 0,
                   xend = mean,
                   yend = density),
               color = "red",
               linetype = "dotted") +
  
  annotate(geom = "text",
           x = mean,
           y = 5,
           label = "mean",
           color = "red",
           fontface = "plain",
           angle = 90,
           alpha = .8,
           vjust = -1.75) +
  
  geom_segment(data = median_line_data,
               aes(x = median,
                   y = 0,
                   xend = median,
                   yend = density),
               color = "black",
               linetype = "dotted") +
  
  annotate(geom = "text",
           x = median,
           y = 5,
           label = "median",
           fontface = "plain",
           angle = 90,
           alpha = .8,
           vjust = 1.75) +
  ggtitle("Density Plot Illustrating Skewness")

asset_returns_long %>% 
  summarize(skew_assets = skewness(returns)) %>% 
  add_row(asset = "Portfolio",
          skew_assets = skew_tidy$skew_byhand) %>% 
  ggplot(aes(x = asset,
             y = skew_assets,
             colour = asset)) +
  geom_point() +
  geom_text(
    aes(x = "Portfolio",
        y =skew_tidy$skew_builtin + .04),
    label = "Portfolio",
    color ="cornflowerblue") +
  labs(y = "skewness")

#5.4
window <- 24
rolling_skew_xts <-
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = skewness,
            width = window) %>% 
  na.omit()

skew_roll_24 <-
  rollify(skewness, window = window)

skew_roll_24(portfolio_returns_tq_rebalanced_monthly$returns)

roll_skew_tibbletime <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  as_tbl_time(index = date) %>% 
  mutate(skew = skew_roll_24(returns)) %>% 
  select(-returns) %>% 
  na.omit()

rolling_skew_tq <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  tq_mutate(select = returns,
            mutate_fun = rollapply,
            width = window,
            FUN = skewness,
            col_rename = "tq") %>% 
  na.omit()

rolling_skew_tq %>% 
  select(-returns) %>% 
  mutate(xts = coredata(rolling_skew_xts),
         tbltime = roll_skew_tibbletime$skew) %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>% 
  tail(3)

highchart(type = "stock") %>% 
  hc_title(text = "Rolling 24-Month Skewness") %>% 
  hc_add_series(rolling_skew_xts,
                name = "Rolling skewness",
                color = "cornflowerblue") %>% 
  hc_yAxis(title = list(text = "skewness"),
           opposite = FALSE,
           max = 1,
           min = -1) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE)

highchart(type = "stock") %>% 
  hc_title(text = "Rolling 24-Month Skewness") %>% 
  hc_add_series(rolling_skew_xts,
                name = "Rolling skewness",
                color = "cornflowerblue") %>% 
  hc_yAxis(title = list(text = "skewness"),
           opposite = FALSE) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE)


rolling_skew_tq %>% 
  ggplot(aes(x = date, y =tq)) +
  geom_line(color = "cornflowerblue") +
  ggtitle("Rolling 24-Month Skew") +
  ylab(paste("Rolling ", window, " month skewness",
             sep = " ")) +
  scale_y_continuous(limits = c(-1,1),
                     breaks = pretty_breaks(n = 8)) +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme_update(plot.title = element_text(hjust = 0.5))

### 2019.05.19

##### Kurtosis
kurt_xts <-
  kurtosis(portfolio_returns_xts_rebalanced_monthly$returns)

kurt_tidy <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  summarise(
    kurt_builtin = kurtosis(returns),
    kurt_byhand = 
      (
        ((sum((returns - mean(returns))^4)/length(returns)) /
          ((sum((returns-mean(returns))^2)/length(returns))^2)) -3
      )
  ) %>% 
  select(kurt_builtin, kurt_byhand)

kurt_tidy %>% 
  mutate(xts = kurt_xts)

sd_pos <-
  mean + (2* sd(portfolio_returns_tq_rebalanced_monthly$returns))

sd_neg <-
  mean - (2* sd(portfolio_returns_tq_rebalanced_monthly$returns))

sd_pos_shaded_area <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>% 
  filter(x > sd_pos)

sd_neg_shaded_area <-
  ggplot_build(portfolio_density_plot)$data[[1]] %>% 
  filter(x < sd_neg)

portfolio_density_plot +
  geom_area(data = sd_pos_shaded_area,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5) +
  geom_area(data = sd_neg_shaded_area,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))


portfolio_density_plot +
  geom_area(data = sd_pos_shaded_area,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5) +
  geom_area(data = sd_neg_shaded_area,
            aes(x = x, y = y),
            fill="pink",
            alpha = 0.5) +
  geom_segment(data = shaded_area_data,
               aes(x=mean,
                   y=0,
                   xend = mean,
                   yend = density),
               color = "red",
               linetype = "dotted") +
  
  annotate(geom = "text",
           x = mean,
           y = 5,
           label = "mean",
           color = "red",
           fontface = "plain",
           angle = 90,
           alpha = .8,
           vjust = -1.75) +
  
  geom_segment(data = median_line_data,
               aes(x = median,
                   y = 0,
                   xend = median,
                   yend = density),
               color = "black",
               linetype = "dotted") +
  
  annotate(geom = "text",
           x = median,
           y = 5,
           label = "median",
           fontface = "plain",
           angle = 90,
           alpha = .8,
           vjust = 1.75) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))

asset_returns_long %>% 
  summarize(kurt_assets = kurtosis(returns)) %>% 
  add_row(asset = "Portfolio",
          kurt_assets = kurt_tidy$kurt_byhand) %>% 
  ggplot(aes(x = asset,
             y = kurt_assets,
             colour = asset)) +
  geom_point() +
  geom_text(
    aes(x = "Portfolio",
        y =
          kurt_tidy$kurt_byhand + .06),
    label = "Portfolio",
    color = "cornflowerblue") +
  labs(y = "kurtosis")

window <- 24

rolling_kurt_xts <-
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = kurtosis,
            width = window) %>% 
  na.omit()

kurt_roll_24 <-
  rollify(kurtosis,
          window = window)

roll_kurt_tibbletime <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  as_tbl_time(index = date) %>% 
  mutate(kurt = kurt_roll_24(returns)) %>% 
  select(-returns) %>%
  na.omit()

rolling_kurt_tq <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  tq_mutate(select = returns,
            mutate_fun = rollapply,
            width = window,
            FUN = kurtosis,
            col_name = "tq") %>% 
  select(-returns) %>% 
  na.omit()

rolling_kurt_tq %>% 
  mutate(xts = coredata(rolling_kurt_xts),
         tbltime = roll_kurt_tibbletime$kurt) %>% 
  mutate_if(is.numeric,funs(round(.,3))) %>% 
  tail(3)

highchart(type = "stock") %>% 
  hc_title(text = "Rolling 24-Month kurtosis") %>% 
  hc_add_series(rolling_kurt_xts,
                name = "Rolling 24-Month kurtosis",
                color = "cornflowerblue") %>% 
  hc_yAxis(title = list(text = "kurtosis"),
                        opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = FALSE)

rfr <- .0003
sharpe_xts <-
  SharpeRatio(portfolio_returns_xts_rebalanced_monthly,
              Rf = rfr,
              FUN = "StdDev") %>% 
  `colnames<-`("sharpe_xts")

sharpe_tidyverse_byhand <-
  portfolio_returns_dplyr_byhand %>% 
  summarise(sharpe_dplyr = mean(returns - rfr)/
              sd(returns - rfr))

sharpe_tq <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  tq_performance(Ra = returns,
                 performance_fun = SharpeRatio,
                 Rf = rfr,
                 FUN = "StdDev") %>% 
  `colnames<-`("sharpe_tq")

sharpe_tq %>% 
  mutate(tidy_sharpe = sharpe_tidyverse_byhand$sharpe_dplyr,
         xts_sharpe = sharpe_xts)

market_returns_xts <-
  getSymbols("SPY",
             src = 'yahoo',
             from = "2012-12-31",
             to = "2019-5-23",
             auto.assign = TRUE,
             warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  `colnames<-`("SPY") %>% 
  to.monthly(indexAt = "lastof",
             OHLC = FALSE)

market_sharpe <-
  market_returns_xts %>% 
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>% 
  mutate(returns = 
           (log(SPY) - log(lag(SPY)))) %>% 
  na.omit() %>% 
  summarise(ratio =
              mean(returns -rfr)/sd(returns -rfr))

market_sharpe$ratio

sharpe_byhand_with_return_columns <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  mutate(ratio = 
           mean(returns - rfr)/sd(returns - rfr)) %>% 
  mutate(returns_below_rfr =
           if_else(returns < rfr, returns, as.numeric(NA))) %>% 
  mutate(returns_above_rfr =
           if_else(returns > rfr, returns, as.numeric(NA))) %>% 
  mutate_if(is.numeric, funs(round(.,4)))

sharpe_byhand_with_return_columns %>% 
  ggplot(aes(x = date)) +
  geom_point(aes(y = returns_below_rfr),
             colour = "red") +
  geom_point(aes(y = returns_above_rfr),
             colour = "green") +
  geom_vline(xintercept = 
               as.numeric(as.Date("2016-11-30")),
             color = "blue") +
  geom_hline(yintercept = rfr,
             color = "purple",
             linetype = "dotted") +
  annotate(geom = "text",
           x = as.Date("2016-11-30"),
           y = -.04,
           label = "Election",
           fontface = "plain",
           angle = 90,
           alpha = .5,
           vjust = 1.5) +
  ylab("percent monthly returns") +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_date(breaks = pretty_breaks(n = 8))

sharpe_byhand_with_return_columns %>% 
  ggplot(aes(x = returns)) +
  geom_histogram(alpha = 0.45,
                 binwidth = .01,
                 fill = "cornflowerblue") +
  geom_vline(xintercept = rfr,
             color = "green") +
  annotate(geom = "text",
           x = rfr,
           y = 13,
           label = "rfr",
           fontface = "plain",
           angle = 90,
           alpha = .5,
           vjust = 1)

asset_returns_long %>% 
  summarise(stand_dev = sd(returns),
            sharpe = mean(returns - rfr)/
              sd(returns - rfr)) %>% 
  add_row(asset = "Portfolio",
          stand_dev = 
            portfolio_sd_xts_builtin[1],
          sharpe =
            sharpe_tq$sharpe_tq) %>% 
  ggplot(aes(x = stand_dev,
             y = sharpe,
             color = asset)) +
  geom_point(size = 2) +
  geom_text(
    aes(x = 
          sd(portfolio_returns_tq_rebalanced_monthly$returns),
        y = sharpe_tq$sharpe_tq + .02,
        label = "Portfolio")) +
  ylab("Sharpe Ratio") +
  xlab("standard deviation") +
  ggtitle("Sharpe Ratio versus Standard Deviation") +
  theme_update(plot.title = element_text(hjust = 0.5))

## 20190525
window <- 24

rolling_sharpe_xts <-
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            window,
            function(x)
              SharpeRatio(x,
                          Rf = rfr,
                          FUN = "StdDev")) %>% 
  na.omit() %>% 
  `colnames<-`("xts")

# Creat rolling function
sharpe_roll_24 <-
  rollify(function(returns){
    ratio = mean(returns -rfr) / sd(returns - rfr)
  },
  window = window)

rolling_sharpe_tidy_tibbletime <-
  portfolio_returns_dplyr_byhand %>% 
  as_tbl_time(index = date) %>% 
  mutate(tbltime_sharpe = sharpe_roll_24(returns)) %>% 
  na.omit() %>% 
  select(-returns)

sharpe_tq_roll <- function(df){
  SharpeRatio(df,
              Rf = rfr,
              FUN = "StdDev")
}

rolling_sharpe_tq <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  tq_mutate(
    select = returns,
    mutate_fun = rollapply,
    width = window,
    align = "right",
    FUN = sharpe_tq_roll,
    col_rename = "tq_sharpe") %>% 
  na.omit()

rolling_sharpe_tidy_tibbletime %>% 
  mutate(xts_sharpe = coredata(rolling_sharpe_xts),
         tq_sharpe = rolling_sharpe_tq$tq_sharpe)

highchart(type = "stock") %>% 
  hc_title(text = "Rolling 24-Month Sharpe") %>% 
  hc_add_series(rolling_sharpe_xts,
                name = "Sharpe",
                color = "blue") %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_exporting(enabled = TRUE)

rolling_sharpe_xts %>% 
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>% 
  rename(rolling_sharpe = xts) %>% 
  ggplot(aes(x = date,
             y = rolling_sharpe)) +
  geom_line(color = "cornflowerblue") +
  ggtitle("Rolling 24-Month Sharpe Ratio") +
  labs(y = "rolling sharpe ratio") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

today <- Sys.Date()
today_char <- format(today, "%Y-%m-%d")
start_date <- "2014-12-31"
market_returns_xts <-
  getSymbols("SPY",
             src = 'yahoo',
             from = start_date,
             to = today_char,
             auto.assign = TRUE,
             warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  `colnames<-`("SPY") %>% 
  to.monthly(indexAt = "lastof",
             OHLC = FALSE) %>% 
  Return.calculate(.,
                   method = "log") %>% 
  na.omit()

market_returns_tidy <-
  market_returns_xts %>% 
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>% 
  na.omit() %>% 
  select(date, returns = SPY)

portfolio_returns_tq_rebalanced_monthly %>% 
  mutate(market_returns = market_returns_tidy$returns) %>% 
  head()

cov(portfolio_returns_xts_rebalanced_monthly,
    market_returns_tidy$returns) /
  var(market_returns_tidy$returns)

beta_assets <-
  asset_returns_long %>% 
  nest(-asset)

beta_assets <-
  asset_returns_long %>% 
  nest(-asset) %>% 
  mutate(model =
           map(data, ~
                 lm(returns ~ market_returns_tidy$returns,
                    data = .)))

beta_assets <-
  asset_returns_long %>% 
  nest(-asset) %>% 
  mutate(model =
           map(data, ~
                 lm(returns ~ market_returns_tidy$returns,
                    data = .)))

beta_assets <-
  asset_returns_long %>% 
  nest(-asset) %>% 
  mutate(model =
           map(data, ~
                 lm(returns ~ market_returns_tidy$returns,
                    data = .))) %>% 
  mutate(model = map(model,tidy))

beta_assets <-
  asset_returns_long %>% 
  nest(-asset) %>% 
  mutate(model =
           map(data, ~
                 lm(returns ~ market_returns_tidy$returns,
                    data = .))) %>% 
  mutate(model = map(model,tidy)) %>% 
  unnest(model) %>% 
  mutate_if(is.numeric, funs(round(.,4)))

beta_assets <-
  asset_returns_long %>% 
  nest(-asset) %>% 
  mutate(model =
           map(data, ~
                 lm(returns ~ market_returns_tidy$returns,
                    data = .))) %>% 
  unnest(model %>% map(tidy)) %>% 
  filter(term !="(Intercept)") %>% 
  select(-term)

beta_assets %>% 
  select(asset,estimate) %>% 
  filter(asset == "AAPL")

beta_byhand <-
  w[1] * beta_assets$estimate[1] +
  w[2] * beta_assets$estimate[2] +
  w[3] * beta_assets$estimate[3] +
  w[4] * beta_assets$estimate[4] +
  w[5] * beta_assets$estimate[5]

beta_builtin_xts <- 
  CAPM.beta(portfolio_returns_xts_rebalanced_monthly,
            market_returns_xts)

beta_dplyr_byhand <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  do(model =
       lm(returns ~ market_returns_tidy$returns,
          data = .)) %>% 
  tidy(model) %>% 
  mutate(term = c("alpha","beta")) %>% 
  select(estimate)

beta_dplyr_byhand$estimate[2]

beta_builtin_tq <-
  portfolio_returns_tq_rebalanced_monthly %>% 
  mutate(market_return =
           market_returns_tidy$returns) %>% 
  na.omit() %>% 
  tq_performance(Ra = returns,
                 Rb = market_return,
                 performance_fun = CAPM.beta) %>% 
  `colnames<-`("base_tq")

beta_builtin_tq %>% 
  mutate(dplyr_beta = beta_dplyr_byhand$estimate[2],
         byhand_beta = beta_byhand,
         xts_beta = coredata(beta_builtin_xts)) %>% 
  round(3)