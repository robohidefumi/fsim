########## output
library(tidyr)
pivot_inh <- df_left %>%
  select(pattern,yield,inh_r) %>%
  tidyr::spread(yield,inh_r)
pivot_net <- df_left %>%
  select(pattern,yield,net_r) %>%
  tidyr::spread(yield,net_r)
pivot_last <- df_left %>%
  select(pattern,yield,last_r) %>%
  tidyr::spread(yield,last_r)


library(ggplot2)
gg_inh_2064 <- ggplot(df_left,aes(pattern,inh_ttl)) +
  geom_bar(stat = "identity") +
  facet_wrap(~yield) +
  labs(title = "Total Net Inheritance Asset after Tax in 2064")

gg_inh_2064_diff <- ggplot(df_diff_left,aes(pattern,inh_diff)) +
  geom_bar(stat = "identity") +
  facet_wrap(~yield) +
  labs(title = "Difference Inheritance Asset after Tax in 2064")

gg_net_2064 <- ggplot(df_left,aes(pattern,net)) +
  geom_bar(stat = "identity") +
  facet_wrap(~yield) +
  labs(title = "Total Net Income after Tax in 2064")

gg_net_2064_diff <- ggplot(df_diff_left,aes(pattern,net_diff)) +
  geom_bar(stat = "identity") +
  facet_wrap(~yield) +
  labs(title = "Difference of Income after Tax in 2064")

gg_last_2064 <- ggplot(df_left,aes(pattern,last)) +
  geom_bar(stat = "identity") +
  facet_wrap(~yield) +
  labs(title = "Hidefumi's Net Asset after Tax in 2064")

gg_last_2064_diff <- ggplot(df_diff_left,aes(pattern,net_diff)) +
  geom_bar(stat = "identity") +
  facet_wrap(~yield) +
  labs(title = "Difference of Hidefumi's Net Asset after Tax in 2064")


gg_2064 <- ggplot(df_left,aes(inh_ttl,net)) +
  geom_point(aes(colour = pattern)) +
  facet_wrap(~yield,ncol=1) +
  labs(title = "Inheritance and Income in 2064")

gg_2064_diff <- ggplot(df_diff_left,aes(inh_diff,net_diff)) +
  geom_point(aes(colour = pattern)) +
  facet_wrap(~yield,ncol=1) +
  labs(title = "Difference of Inheritance and Income in 2064")

gg_2064_b <- ggplot(df_left,aes(inh_ttl,last)) +
  geom_point(aes(colour = pattern)) +
  facet_wrap(~yield,ncol=1) +
  labs(title = "Inheritance and Asset in 2064")

gg_2064_b_diff <- ggplot(df_diff_left,aes(inh_diff,last_diff)) +
  geom_point(aes(colour = pattern)) +
  facet_wrap(~yield,ncol=1) +
  labs(title = "Difference of Inheritance and Asset in 2064")


gg_inh <- ggplot(df_ttl,aes(m_age,inh_ttl)) +
  #geom_line() +
  geom_bar(stat = "identity") +
  facet_grid(pattern~yield) +
  labs(title = "Total Net Inheritance Asset after Tax")

gg_inh_diff <- ggplot(df_diff,aes(m_age,inh_diff)) +
  #geom_line() +
  geom_bar(stat = "identity") +
  facet_grid(pattern~yield) +
  labs(title = "Difference of Inheritance Asset after Tax")

gg_net <- ggplot(df_ttl,aes(m_age,net)) +
  geom_bar(stat = "identity") +
  facet_grid(pattern~yield) +
  labs(title = "Total Net Income after Tax")

gg_net_diff <- ggplot(df_diff,aes(m_age,net_diff)) +
  geom_bar(stat = "identity") +
  facet_grid(pattern~yield) +
  labs(title = "Difference of Income after Tax")

gg_last <- ggplot(df_ttl,aes(m_age,last)) +
  geom_bar(stat = "identity") +
  facet_grid(pattern~yield) +
  labs(title = "Total Net Income after Tax")

gg_last_diff <- ggplot(df_diff,aes(m_age,last_diff)) +
  geom_bar(stat = "identity") +
  facet_grid(pattern~yield) +
  labs(title = "Difference of Income after Tax")
