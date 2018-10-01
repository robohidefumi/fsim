########## output
library(tidyr)
library(dplyr)

ana_mst <- list(
  item = c('inh','net','last'),
  common = c('pattern','yield'),
  year_list = c(0:45 + 2019), #変えたからね
  year_by_5 = seq(2019,2064,by=5),
  mst = list(
    item = data.frame(
      param = c("inh","net","last"),
      name = c("Inheritance","Net Income","Net Asset")
    ),
    ext = data.frame(
      param = c("r","diff"),
      name = c("Amount","Difference")
    )
  ),
  mst_plot = list(
    item = data.frame(
      param = c("inh_ttl","net","last","inh_diff","net_diff","last_diff"),
      name = c("Amount of Inheritance","Amount of Net Income","Amount of Net Asset",
               "Difference of Inheritance","Difference of Net Income","Difference of Net Asset")
    )
  ),
  mst_plots = list(
    item = data.frame(
      param = c("inh_ttl","net","last","inh_diff","net_diff","last_diff"),
      name = c("Inheritance","Net Income","Net Asset",
               "Difference of Inheritance","Difference of Net Income","Difference of Net Asset")
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

mkpivot2 <- function(item,ext_value,year_value,df){
  item_ext <- paste(item,ext_value,sep='_')
  select_v <- c(ana_mst$common,item_ext)
  df %>%
      filter(year == year_value) %>%
      select(.,one_of(select_v)) %>%
      tidyr::spread_(paste(ana_mst$common[2]),paste(item_ext))
}
#mkpivot2('inh','r',2020,df_ttl)

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

mktitle2 <- function(item, ext, year){
  
  cat1 <- ana_mst$mst$ext %>%
    filter(param == ext) %>%
    select(name)
  
  cat2 <- ana_mst$mst$item %>%
    filter(param == item) %>%
    select(name)
  m_age <- year - 2009
  m_age <- paste0("(",m_age,")")
  title <- paste("##",cat1$name,"of", cat2$name,"by",ana_mst$common[1],"&",ana_mst$common[2],m_age, sep=" ")
  
  return(title)
}


mkslide_pivot <- function(item,ext_value){
  k <- make_pivot(item,ext_value,df_ttl)
  title <- mkslide_title(item,ext_value)
  return(list(title = title, table = k))
}

mkslide_pivot2 <- function(item,ext_value){
  
  x <- list()
  for(i in ana_mst$year_by_5){
    k <- mkpivot2(item,ext_value,i,df_ttl)
    title <- mktitle2(item,ext_value,i)
    x <- list.append(x,title = title,table = k )
  }
  return(x)
}

loop_grid <- expand.grid(x = ana_mst$mst$item$param, y = ana_mst$mst$ext$param)
x <- as.vector(loop_grid$x)
y <- as.vector(loop_grid$y)
#slide_list <- map2(x,y,function(x,y) mkslide_pivot(paste0(x),paste0(y)))
slide2_list <- map2(x,y,function(x,y) mkslide_pivot2(paste0(x),paste0(y)))

library(ggplot2)

mkplot_syb <- function(year_value,yield_value,item,df){
  g <- ggplot(
    df %>% filter(year == year_value & yield == yield_value),
    aes_string(x=paste0(ana_mst$common[1]),y=paste0(item))) +
    geom_bar(stat = "identity")
  return(g)
}
#mkplot_syb(2050, 0.04, 'inh_ttl',df_ttl)

mkplot_sys <- function(year_value,yield_value,item_x,item_y, df){
  g <- ggplot(
    df %>% filter(year == year_value & yield == yield_value),
    aes_string(x=paste0(item_x),y=paste0(item_y))) +
    geom_point(aes(colour = pattern))
  return(g)
}
#mkplot_sys(2050,0.06,'inh_ttl','net',df_ttl)

gg_inh <- ggplot(df_ttl,aes(m_age,inh_ttl)) +
  #geom_line() +
  geom_bar(stat = "identity") +
  facet_grid(pattern~yield) +
  labs(title = "Total Net Inheritance Asset after Tax")

mkplot_tsb <- function(pattern_value,yield_value,item_x,item_y, df){
  g <- ggplot(
    df %>% filter(pattern == pattern_value & yield == yield_value),
    aes_string(x=paste0(item_x),y=paste0(item_y))) +
    geom_line()
  return(g)
}
#mkplot_tsb('b',0.06,'m_age', 'inh_ttl',df_ttl)

mkplot_tsb2 <- function(yield_value,item_x,item_y, df){
  g <- ggplot(
    df %>% filter(yield == yield_value),
    aes_string(x=paste0(item_x),y=paste0(item_y))) +
    geom_line(aes(colour = pattern))
  return(g)
}
#mkplot_tsb2(0.06,'m_age', 'inh_ttl',df_ttl)

mktitle_plot <- function(year_value,yield_value,item){
  
  subject <- ana_mst$mst_plot$item %>%
    filter(param == item) %>%
    select(name)
  
  yield_string <- paste0(ana_mst$common[2], "=" ,yield_value)
  m_age <- year_value - 2009
  m_age <- paste0("(",m_age,")")
  title <- paste("##",subject$name,"by",ana_mst$common[1],"&",yield_string,m_age, sep=" ")
  
  return(title)
}

mktitle2_plot <- function(year_value,yield_value,item_x,item_y){
  
  subject_x <- ana_mst$mst_plots$item %>%
    filter(param == item_x) %>%
    select(name)
  
  subject_y <- ana_mst$mst_plots$item %>%
    filter(param == item_y) %>%
    select(name)
  
  subject <- paste(subject_x$name,"x",subject_y$name,sep=" ")
  
  yield_string <- paste0(ana_mst$common[2], "=" ,yield_value)
  m_age <- year_value - 2009
  m_age <- paste0("(",m_age,")")
  title <- paste("##",subject,"by",ana_mst$common[1],"&",yield_string,m_age, sep=" ")
  
  return(title)
}
#mktitle2_plot(2050, 0.06,"inh_ttl","net")

mktitle3_plot <- function(yield_value,item){
  
  subject <- ana_mst$mst_plot$item %>%
    filter(param == item) %>%
    select(name)
  
  title <- paste("##",subject$name,"by",ana_mst$common[1],"&","yeld","=",yield_value, sep=" ")
  
  return(title)
}
#mktitle3_plot(0.06,'inh_ttl')


mkslide_plot <- function(item){

  grid_list <- list()
  grid_list <- unique(df_ttl$yield)  %>% map(function(x){
    list(expand.grid( year = ana_mst$year_by_5, yield=x ))
  })
  
  o <- list()
  for(i in 1:length(grid_list)){
    m <- grid_list[[i]][[1]]$year
    n <- grid_list[[i]][[1]]$yield
    x <- map2(m,n, function(m,n){
      k <- mkplot_syb(m, n, item, df_ttl)
      title <- mktitle_plot(m,n,item)
      list(title = title,plot = k )
    })
    o <- list.append(o,x)
  }
  return(o)
}

slide_plot1 <- as.vector(ana_mst$mst_plot$item$param) %>% map(function(x) mkslide_plot(paste0(x)))

mkslide2_plots <- function(item_x,item_y){
  
  grid_list <- list()
  grid_list <- unique(df_ttl$yield)  %>% map(function(x){
    list(expand.grid( year = ana_mst$year_by_5, yield=x ))
  })
  
  o <- list()
  for(i in 1:length(grid_list)){
    m <- grid_list[[i]][[1]]$year
    n <- grid_list[[i]][[1]]$yield
    x <- map2(m,n, function(m,n){
      k <- mkplot_sys(m, n, item_x, item_y, df_ttl)
      title <- mktitle2_plot(m,n,item_x,item_y)
      list(title = title,plot = k )
    })
    o <- list.append(o,x)
  }
  return(o)
}

x <- c('inh_ttl','inh_diff')
y <- c('net','net_diff')
slide_plot2 <- map2(x,y, function(x,y) mkslide2_plots(paste0(x),paste0(y)))


grid <- expand.grid( item = as.vector(ana_mst$mst_plot$item$param), yield=unique(df_ttl$yield) )
x <- as.vector(grid$item)
y <- as.vector(grid$yield)
slide_plot3 <- map2(x,y, function(x,y){
  k <- mkplot_tsb2(y,'m_age',x ,df_ttl)
  title <- mktitle3_plot(y,x)
  list(title = title,plot = k )
})
