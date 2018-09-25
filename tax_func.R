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
