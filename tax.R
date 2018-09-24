library("rlist")
library("dplyr")

##############関数開始##################
#### 関数定義
m_donate <- function(df_master,df_args){
  l <- length(df_args$year)
  df_master[,"inv_donate"] <- rep(0,l)
  if(l > 0){
    for(i in 1:length(df_args$year)){
      position <- df_args[i,"year"] - 2019 + 1
      df_master[position,"inv_donate"] <- df_args[i,"donate"]
    }
  }
  df_master
}

func_yield <- function(df,yield){
  for(i in 1:length(y)){
    ###総合課税算出（ネットの利息収入）
    year <- df[i,"year"]
    if(year == 2019){
      df[i,"begin"] <- ini
      df[i,"m_begin"] <- 0
    }else{
      df[i,"begin"] <- df[i-1,"last"]
      df[i,"m_begin"] <- df[i-1,"m_last"] + df[i-1,"inv_donate"]
    }
    df[i,"gross"] <- df[i,"begin"] * yield
    df[i,"m_gross"] <- df[i,"m_begin"] * yield
    df[i,"net"] <- 
      df[i,"gross"] * 
      (1 - coupon_share * df[i,"f_inc_tax"])
    df[i,"m_net"] <- 
      df[i,"m_gross"] * 
      (1 - coupon_share * df[i,"m_inc_tax"])
    df[i,"last"] <- 
      df[i,"net"] +  df[i,"begin"] - df[i,"expense"] - df[i,"inv_donate"]
    df[i,"m_last"] <-
      df[i,"m_net"] +  df[i,"m_begin"]
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
    #### 投資贈与税算定
    df[i,"inv_don_tax"] = df[i,"inv_donate"] * df[i,"inv_inh_tax"]
    #### 保険贈与税算定
    if(i==1){df[i,"ins_csv"] <- ins_day1}
    else{df[i,"ins_csv"] <- df[i-1,"ins_csv"] * (1 + ins_yield)}
    if(df[i,"ins_donate_flg"] == TRUE){
      df[i,"ins_don_tax"] <- df[i,"ins_csv"] * df[i,"ins_inh_tax"]
    }else{
      df[i,"ins_don_tax"] <- 0
    }
    #### 保険贈与税減算処理
    df[i,"inh_ttl"] <- 
      sum(df[i,"inv_inh"],df[i,"ins_inh"],df[i,"m_last"])
    - df[i,"ins_don_tax"] - df[i,"inv_don_tax"]
  }
  df
}

#### 海外居住による税率設定
set_tax <- function(df){
  df <- df %>%
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
  df
}
##############関数終了##################

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

df_common <- data.frame(
  year = y,
  f_age = fumi_a,
  y_age = yuko_a,
  m_age = michi_a
)

### 生活費試算
expense <- c(rep(0,15),rep(0.15,10),rep(0.25,10),rep(0.45,11))
df_common[,"expense"] <- expense

### Pattern A: 親子双方ずっと日本居住
fumi_l <- rep("j",46)
michi_l <- rep("j",46)
df_a <- cbind(df_common,f_living=fumi_l)
df_a <- cbind(df_a,m_living=michi_l)
##保険所有権設定
df_a[,"ins_owner"] <- rep("f",46)
##投資贈与設定
df_a[,"inv_donate"] <- rep(0,46)

### Pattern B: 親はずっとSG居住、子はずっと日本居住
fumi_l <- rep("s",46)
michi_l <- rep("j",46)
df_b <- cbind(df_common,f_living=fumi_l)
df_b <- cbind(df_b,m_living=michi_l)
##保険所有権設定
df_b[,"ins_owner"] <- rep("f",46)
##投資贈与設定
df_b[,"inv_donate"] <- rep(0,46)

#パターンC: 親子ともに最初の12年のみSG,あとはずっとJP
fumi_l <- c(rep("s",12),rep("j",34))
michi_l <- c(rep("s",12),rep("j",34))
df_c <- cbind(df_common,f_living=fumi_l)
df_c <- cbind(df_c,m_living=michi_l)
##保険所有権設定
df_c[,"ins_owner"] <- c(rep("f",11),rep("m",35))
##投資分贈与設定
#df_master: マスターのデータフレーム
#df_args: 設定用データフレーム
df_args <- data.frame(
  year = c(2030),
  donate = c(0.5)
)
df_c <- m_donate(df_c,df_args)

#パターンD: 親はずっとSG、子は最初の12年のみSG,あとはずっとJP
fumi_l <- rep("s",46)
michi_l <- c(rep("s",12),rep("j",34))
df_d <- cbind(df_common,f_living=fumi_l)
df_d <- cbind(df_d,m_living=michi_l)
##保険所有権設定
df_d[,"ins_owner"] <- c(rep("f",11),rep("m",35))
##投資分贈与設定
#df_master: マスターのデータフレーム
#df_args: 設定用データフレーム
df_args <- data.frame(
  year = c(2030),
  donate = c(0.5)
)
df_d <- m_donate(df_d,df_args)

#パターンE: 親はずっとSG、子は9~21歳までと35~45歳SG,その他はJP
fumi_l <- rep("s",46)
michi_l <- c(rep("s",12),rep("j",14),rep("s",11),rep("j",9))
df_e <- cbind(df_common,f_living=fumi_l)
df_e <- cbind(df_e,m_living=michi_l)
##保険所有権設定
df_e[,"ins_owner"] <- c(rep("f",11),rep("m",35))
##投資分贈与設定
#df_master: マスターのデータフレーム
#df_args: 設定用データフレーム
df_args <- data.frame(
  year = c(2030,2054),
  donate = c(0.5,2)
)
df_e <- m_donate(df_d,df_args)

#### メインルーチン
df_a <- set_tax(df_a)
df_a_4 <- func_yield(df_a,0.04)
df_a_6 <- func_yield(df_a,0.04)
df_a_8 <- func_yield(df_a,0.04)

df_b <- set_tax(df_b)
df_b_4 <- func_yield(df_b,0.04)
df_b_6 <- func_yield(df_b,0.06)
df_b_8 <- func_yield(df_b,0.08)

df_c <- set_tax(df_c)
df_c_4 <- func_yield(df_c,0.04)
df_c_6 <- func_yield(df_c,0.06)
df_c_8 <- func_yield(df_c,0.08)

df_d <- set_tax(df_d)
df_d_4 <- func_yield(df_d,0.04)
df_d_6 <- func_yield(df_d,0.06)
df_d_8 <- func_yield(df_d,0.08)

df_e <- set_tax(df_e)
df_e_4 <- func_yield(df_e,0.04)
df_e_6 <- func_yield(df_e,0.06)
df_e_8 <- func_yield(df_e,0.08)

