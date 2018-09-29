########## output
library(tidyr)
library(dplyr)

ana_mst <- list(
  item = c('inh','net','last'),
  common = c('pattern','yield'),
  mst = list(
    item = data.frame(
      param = c("inh","net","last"),
      name = c("Inheritance","Net Income","Net Asset")
    ),
    ext = data.frame(
      param = c("r","diff"),
      name = c("Amount","Difference")
    )
  )
)

### 2064だけじゃないバージョンを作る必要がある
make_pivot <- function(item,ext_value,df){
  item_ext <- paste(item,ext_value,sep='_')
  select_v <- c(ana_mst$common,item_ext)
  df %>%
    filter(year == 2064) %>%
    select(.,one_of(select_v)) %>%
    tidyr::spread_(paste(ana_mst$common[2]),paste(item_ext))
}

mkslide_title <- function(item, ext){
  
  cat1 <- ana_mst$mst$ext %>%
    filter(param == ext) %>%
    select(name)
  
  cat2 <- ana_mst$mst$item %>%
    filter(param == item) %>%
    select(name)
  
  title <- paste("##",cat1$name,"of", cat2$name,"by",ana_mst$common[1],"&",ana_mst$common[2], sep=" ")
  
  return(title)
}

mkslide_pivot <- function(item,ext_value){
  k <- make_pivot(item,ext_value,df_ttl)
  title <- mkslide_title(item,ext_value)
  return(list(title = title, table = k))
}

loop_grid <- expand.grid(x = ana_mst$item, y = ana_mst$mst$ext$param)
x <- as.vector(loop_grid$x)
y <- as.vector(loop_grid$y)
slide_list <- map2(x,y,function(x,y) mkslide_pivot(paste0(x),paste0(y)))

library(ggplot2)
gg_inh_2064 <- ggplot(df_left,aes(pattern,inh_ttl)) +
  geom_bar(stat = "identity") +
  facet_wrap(~yield) +
  labs(title = "Total Net Inheritance Asset after Tax in 2064")

gg_inh_2064_4 <- ggplot(df_left %>% filter(yield == 0.04),aes(pattern,inh_ttl)) +
  geom_bar(stat = "identity") +
  #  facet_wrap(~yield) +
  labs(title = "Total Net Inheritance Asset after Tax in 2064(0.04)")

gg_inh_2064_6 <- ggplot(df_left %>% filter(yield == 0.06),aes(pattern,inh_ttl)) +
  geom_bar(stat = "identity") +
#  facet_wrap(~yield) +
  labs(title = "Total Net Inheritance Asset after Tax in 2064(0.06)")

gg_inh_2064_8 <- ggplot(df_left %>% filter(yield == 0.08),aes(pattern,inh_ttl)) +
  geom_bar(stat = "identity") +
  #  facet_wrap(~yield) +
  labs(title = "Total Net Inheritance Asset after Tax in 2064(0.08)")

gg_inh_2064_diff <- ggplot(df_diff_left,aes(pattern,inh_diff)) +
  geom_bar(stat = "identity") +
  facet_wrap(~yield) +
  labs(title = "Difference Inheritance Asset after Tax in 2064")

gg_inh_2064_diff_4 <- ggplot(df_diff_left %>% filter(yield == 0.04),aes(pattern,inh_diff)) +
  geom_bar(stat = "identity") +
  #  facet_wrap(~yield) +
  labs(title = "Diff Total Net Inheritance Asset after Tax in 2064(0.04)")

gg_inh_2064_diff_6 <- ggplot(df_diff_left %>% filter(yield == 0.06),aes(pattern,inh_diff)) +
  geom_bar(stat = "identity") +
  #  facet_wrap(~yield) +
  labs(title = "Diff Total Net Inheritance Asset after Tax in 2064(0.06)")

gg_inh_2064_diff_8 <- ggplot(df_diff_left %>% filter(yield == 0.08),aes(pattern,inh_diff)) +
  geom_bar(stat = "identity") +
  #  facet_wrap(~yield) +
  labs(title = "Diff Total Net Inheritance Asset after Tax in 2064(0.08)")

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
