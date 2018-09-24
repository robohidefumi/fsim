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
#生命保険受け取り
ins_day1 <- 0.95
ins_yield <- 0.0438
ins <- 5.1

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
##保険所有権設定
df_inheritance[,"ins_owner"] <- c(rep("f",11),rep("m",35))

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
    ### 保険贈与フラグ設定
    df[i,"ins_donate_flg"] <- FALSE
    if(i > 1){
      if(df[i-1,"ins_owner"] == "f" & df[i,"ins_owner"] == "m"){
        df[i,"ins_donate_flg"] <- TRUE
      }
    }
    ### 相続・贈与税チェック
    if(i > 10){
      j <- i-10
      k <- i-1
      f_validity <- sum(df[j:k,"f_live_flg"])
      m_validity <- sum(df[j:k,"m_live_flg"])
    }else{
      f_validity <- 0
      m_validity <- 0
    }
    if(f_validity >= 10 & m_validity >= 10){
      df[i,"validity"] <- TRUE
    }else{
      df[i,"validity"] <- FALSE
    }
    if((df[i,"validity"] == TRUE)
       &(df[i,"f_living"] == "s" & df[i,"m_living"] == "s")){
      df[i,"inv_inh_tax"] <- 0
      df[i,"ins_inh_tax"] <- 0
    }else{
      df[i,"inv_inh_tax"] <- 0.55
      if(df[i,"ins_owner"] == "m"){
        df[i,"ins_inh_tax"] <- 0.55/2
      }else{
        df[i,"ins_inh_tax"] <- 0.55
      }
    }
    #### 受贈者手取算定
    df[i,"inv_inh"] <- df[i,"last"] * (1 - df[i,"inv_inh_tax"])
    df[i,"ins_inh"] <- ins * (1 - df[i,"ins_inh_tax"])
    #### 保険算定
    if(i==1){df[i,"ins_csv"] <- ins_day1}
    else{df[i,"ins_csv"] <- df[i-1,"ins_csv"] * (1 + ins_yield)}
    if(df[i,"ins_donate_flg"] == TRUE){
      df[i,"ins_don_tax"] <- df[i,"ins_csv"] * df[i,"ins_inh_tax"]
    }else{
      df[i,"ins_don_tax"] <- 0
    }
    #### 保険贈与税減算処理
    df[i,"inh_ttl"] <- 
      sum(df[i,"inv_inh"],df[i,"ins_inh"]) - df[i,"ins_don_tax"]
  }
  df
}

df_inheritance <- func_yield(df_inheritance,yield)
df_inheritance