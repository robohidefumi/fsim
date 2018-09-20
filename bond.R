### config(varied)
current_fx <- 110
ins_default <- 0.956
ins_y_rate <- 0.0438
ins_y_rate <- ins_y_rate * ins_default

### config(fixed)
inv_sum <- 1230000
ins_l_sum <- 11000000
ins_sum <- 1000000
ins_l_rate <- 0.012
c_rate <- 0.0025

### ins settings
ins_yield <- ins_y_rate * ins_sum
ins_loan <- (ins_l_rate * ins_l_sum) / current_fx
ins_net <- ins_yield - ins_loan

### define function
func_r_yield <- function(c,d,p){
  duration = length(seq(Sys.Date(), as.Date(d), "month"))-1
  (((100 - p)/duration)*12) + c/p *100
}

func_yield <- function(c,p){
  c/p * 100
}

### read.csv
setwd("~/work/WM/")
bond_df <- read.csv("bond.csv")

### apply and merge
library("dplyr")

r_yield <- apply(bond_df[,2:4],1,function(x){
  func_r_yield(as.numeric(x[2]),x[1],as.numeric(x[3]))
})

yield <- apply(bond_df[,3:4],1,function(x){
  func_yield(as.numeric(x[1]),as.numeric(x[2]))
})

bond_df2 <- bond_df %>%
  mutate(
    r_yield = r_yield,
    yield = yield
  )

pl <- apply(bond_df2[,5:7], 1, function(x){
 x[1] * (x[2]/100) 
}
)

cf <- apply(bond_df2[,5:7], 1, function(x){
  x[1] * (x[3]/100) 
}
)

c_vec <- apply(bond_df2[,4:5],1,function(x){
  x[1] * x[2] / 100
})

c_sum <- sum(c_vec)

bond_df3 <- bond_df2 %>%
  mutate(
    pl = pl,
    cf = cf
  )

score <- apply(bond_df3[,c(5,8,9)],2,sum)
pl_ratio <- score[2]/score[1] * 100
cf_ratio <- score[3]/score[1] * 100

ttl_cf <- score[c("cf")] - ins_loan - (c_sum * c_rate)
ttl_yieldings <- (score[c("pl")] + ins_net - (c_sum * c_rate))/inv_sum
