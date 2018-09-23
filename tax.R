library("rlist")
library("dplyr")

#対象年度設定
y <- c(0:45 + 2019)
#年齢設定
## 生年
f <- 1974
yu <- 1977
m <- 2009
## 年齢計算
fumi_a <- y - f
yuko_a <- y - yu
michi_a <- y - m

#投資設定
coupon_share <- 0.7 #利回りのうち利息収入の割合

df_inheritance <- data.frame(
  year = y,
  f_age = fumi_a,
  y_age = yuko_a,
  m_age = michi_a
)

#海外居住年度入力
###秀文
##ずっと日本
#rep("j",46)
##秀道21歳までSG
#c(rep("s",11),rep("j",35))
##ずっとSG
fumi_l <- rep("s",46)
###秀道
##ずっと日本
#rep("j",46)
##秀道21歳までSG
michi_l <- c(rep("s",12),rep("j",34))
##ずっとSG
#rep("s",46)
df_inheritance <- cbind(df_inheritance,f_living=fumi_l)
df_inheritance <- cbind(df_inheritance,m_living=michi_l)

df_inheritance <- df_inheritance %>%
  mutate(f_inc_tax = case_when(
    f_living == "j" ~ 0.2,
    f_living == "s" ~ 0
  )
  ) %>%
  mutate(f_live_flg = case_when(
    f_living == "j" ~ FALSE,
    f_living == "s" ~ TRUE
  )
  ) %>% 
  mutate(m_inc_tax = case_when(
    m_living == "j" ~ 0.2,
    m_living == "s" ~ 0
  )
  ) %>%
  mutate(m_live_flg = case_when(
    m_living == "j" ~ FALSE,
    m_living == "s" ~ TRUE
  )
  )

yield <- 0.06
ini <- 2.5

func_yield <- function(df,yield){
  for(i in 1:length(y)){
    ###総合課税算出（ネットの利息収入）
    year <- df[i,"year"]
    if(year == 2019){
      df[i,"begin"] <- ini
    }else{
      df[i,"begin"] <- df[i-1,"last"]
    }
    df[i,"gross"] <- df[i,"begin"] * yield
    df[i,"net"] <- 
      df[i,"gross"] * 
      (1 - df[i,"gross"] * df[i,"f_inc_tax"])
    df[i,"last"] <- 
      df[i,"net"] +  df[i,"begin"]
    ### 相続・贈与税チェック
    if(i > 10){
      j <- i-10
      k <- i-1
      f_validity <- sum(df[j:k,"f_live_flg"])
      m_validity <- sum(df[j:k,"m_live_flg"])
    }else{
      f_validity <- FALSE
      m_validity <- FALSE
    }
    if((f_validity >= 10 & m_validity >= 10)
       & (df[i,"f_living"] == "s" & df[i,"m_living"] == "s")){
      df[i,"inh_tax"] <- 0
    }else{
      df[i,"inh_tax"] <- 0.55
    }
    df[i,"inv_inh"] <- df[i,"last"] * (1 - df[i,"inh_tax"])
  }
  df
}

df_inheritance <- func_yield(df_inheritance,yield)


duration <- 45
yield_s <- c(0.04,0.06,0.08)
cf_r <- 0.7
ins <- 5.1
spend <- 5 + 1

jp <- lapply(yield_s,function(x){
  y <- x*cf_r*0.8 + x*(1 - cf_r)
  t <- ttl * (1+y)^duration
  income <- t * 0.45 + ins * 0.45 - spend
})

jp_m <- list.cbind(jp)

h_allocation <- 1.5
sgjp <- lapply(yield_s,function(x){
  t <- ttl * (1+x)^10
  t <- t - h_allocation
  y <- x*cf_r*0.8 + x*(1 - cf_r)
  t <- t*(1+y)^(duration - 10 )
  t_h <- h_allocation*(1+y)^(duration - 10 )
  t * 0.45 + t_h + ins * 0.725  - spend
})
sgjp_m <-  list.cbind(sgjp)

sg <- lapply(yield_s,function(x){
  t <- ttl * (1+x)^duration
  t + ins  - spend
})
sg_m <- list.cbind(sg)

all_m <- rbind(jp_m,sgjp_m,sg_m)
all_m <- apply(all_m,c(1,2),function(x){round(x,digits=1)} )
all_m <- rbind(all_m, all_m[2,]  - all_m[1,])
all_m <- rbind(all_m, all_m[3,]  - all_m[2,])
all_m <- rbind(all_m, all_m[3,]  - all_m[1,])

all_df <- data.frame(all_m)
colnames(all_df) <- c("(i) 4%","(ii) 6%","(iii)8%")
rownames(all_df) <- c("(a)Without 10 years overseas","(b)with 10 years","(c)double 10 years","(b)-(a)","(c)-(b)","(c)-(a)")
ggplot(data=all_df, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())

all_df