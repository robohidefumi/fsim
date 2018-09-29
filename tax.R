#### メインルーチン
p_v <- letters[1:6] #パターンリスト
y_v <- c(0.04,0.06,0.08)

df_ttl <- data_frame()
for(i in 1:length(p_v)){
  df <- eval(parse(text=paste("df_",p_v[i],sep='')))
  df <- set_tax(df)
  df_pattern <- data_frame()
  for(j in 1:length(y_v)){
    df_for <- func_yield(df,y_v[j])
    df_for[,"yield"] <- y_v[j]
    df_pattern <- rbind(df_pattern,df_for)
  }
  df_pattern[,"pattern"] <- p_v[i]
  df_ttl <- rbind(df_ttl,df_pattern)
}

df_left <- df_ttl %>%
  filter(year==2064) %>%
  mutate(inh_r = round(inh_ttl,digits=1)) %>%
  mutate(net_r = round(net,digits=1)) %>%
  mutate(last_r = round(last,digits=1))

df_diff_left <- df_left %>%
  select(pattern,yield,net,inh_ttl,last)

k <- length(df_diff_left$pattern)
for(i in 1:k){
  if(i %% 3 == 1){j <- 1}
  if(i %% 3 == 2){j <- 2}
  if(i %% 3 == 0){j <- 3}
  df_diff_left[i,"inh_diff"] <- df_diff_left[i,"inh_ttl"] - df_diff_left[j,"inh_ttl"]
  df_diff_left[i,"net_diff"] <- df_diff_left[i,"net"] - df_diff_left[j,"net"]
  df_diff_left[i,"last_diff"] <- df_diff_left[i,"last"] - df_diff_left[j,"last"]
}

df_diff <- df_ttl %>%
  select(m_age,pattern,yield,net,inh_ttl,last)

k <- length(df_diff$m_age)
for(i in 1:k){
  d <- 46*3
  if(i %% d == 0){j <- d}else{j <- i %% d }
  df_diff[i,"inh_diff"] <- df_diff[i,"inh_ttl"] - df_diff[j,"inh_ttl"]
  df_diff[i,"net_diff"] <- df_diff[i,"net"] - df_diff[j,"net"]
  df_diff[i,"last_diff"] <- df_diff[i,"last"] - df_diff[j,"last"]
}

k <- length(df_ttl$m_age)
for(i in 1:k){
  d <- 46*3
  if(i %% d == 0){j <- d}else{j <- i %% d }
  df_ttl[i,"inh_diff"] <- df_ttl[i,"inh_ttl"] - df_ttl[j,"inh_ttl"]
  df_ttl[i,"net_diff"] <- df_ttl[i,"net"] - df_ttl[j,"net"]
  df_ttl[i,"last_diff"] <- df_ttl[i,"last"] - df_ttl[j,"last"]
}
df_ttl <- df_ttl %>%
  mutate(inh_r = round(inh_ttl,digits=1)) %>%
  mutate(net_r = round(net,digits=1)) %>%
  mutate(last_r = round(last,digits=1))
