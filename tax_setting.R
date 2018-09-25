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
ini <- 2.5
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
expense <- c(rep(0,35),rep(0.30,11))
#expense <- rep(0,46)
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

#パターンB: 親子ともに最初の12年のみSG,あとはずっとJP
fumi_l <- c(rep("s",12),rep("j",34))
michi_l <- c(rep("s",12),rep("j",34))
df_b <- cbind(df_common,f_living=fumi_l)
df_b <- cbind(df_b,m_living=michi_l)
##保険所有権設定
df_b[,"ins_owner"] <- c(rep("f",11),rep("m",35))
##投資分贈与設定
#df_master: マスターのデータフレーム
#df_args: 設定用データフレーム
df_args <- data.frame(
  year = c(2030),
  donate = c(0.5)
)
df_b <- m_donate(df_b,df_args)

### Pattern C: 親はずっとSG居住、子はずっと日本居住
fumi_l <- rep("s",46)
michi_l <- rep("j",46)
df_c <- cbind(df_common,f_living=fumi_l)
df_c <- cbind(df_c,m_living=michi_l)
##保険所有権設定
df_c[,"ins_owner"] <- rep("f",46)
##投資贈与設定
df_c[,"inv_donate"] <- rep(0,46)

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
df_e <- m_donate(df_e,df_args)

#パターンF: 親はずっとSG、子もずっとSG
fumi_l <- rep("s",46)
michi_l <- rep("s",46)
df_f <- cbind(df_common,f_living=fumi_l)
df_f <- cbind(df_f,m_living=michi_l)
##保険所有権設定
df_f[,"ins_owner"] <- rep("f",46)
##投資分贈与設定
#df_master: マスターのデータフレーム
#df_args: 設定用データフレーム
df_f[,"inv_donate"] <- rep(0,46)
