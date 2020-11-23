balance_wf_plot <- function(yyyy_v){
  
  tax_balance <- apple %>%
    filter(category == "Tax_Balance" & yyyy == yyyy_v) %>%
    select(yyyy,item,value)
  
  df <- data.frame(Category = tax_balance$item, Value2 = tax_balance$value)
  
  df <- df %>%
    mutate(Value = ifelse(Category == "Ending balances", Value2 * -1 , Value2))
  
  levels <- df$Category
  
  data1 <- df  %>%
    mutate(Category = factor(Category, levels = levels),
           ymin = round(cumsum(Value), 3),
           ymax = lag(cumsum(Value), default = 0),
           xmin = c(head(Category, -1), NA),
           xmax = c(tail(Category, -1), NA),
           Impact = ifelse(Category %in% c(as.character(df$Category[1]), as.character(df$Category[nrow(df)])),"Balance",
                           ifelse(Value > 0, "Increase", "Decrease")
           ))
  
  g <- ggplot(data1) +
    theme_bw()+
    theme(legend.position = "right", panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(y = "USD Millions", x = "Tax Category", title = paste("Tax WF",yyyy_v, collapse = "_"))
  
  w <- 0.4  #use to set width of bars
  
  g <- g +
    geom_rect(aes(xmin = as.integer(Category) - w/2,
                  xmax = as.integer(Category) + w/2, ymin = ymin, ymax = ymax,
                  fill = Impact), colour = "black") +
    scale_x_discrete(limits = levels) +
    scale_fill_manual(values = (c("Decrease" = "blue", "Increase" = "orange", "Balance" = "black")))
  
  g <- g +
    geom_segment(data = data1[1:(nrow(data1) -1),],aes(x = xmin,
                                                       xend = xmax,
                                                       y = ymin,
                                                       yend = ymin))
  
  return(g)
}

balance_plot <- function(category_v, yyyy_v){
  
  df_org <- apple %>%
    filter(category == category_v & yyyy == yyyy_v) %>%
    select(yyyy,item,value)

  df <- data.frame(Category = df_org$item, Value2 = df_org$value)
  
  df <- df %>%
    mutate(Value = ifelse(Category == "TTL", Value2 * -1 , Value2))
  
  levels <- df$Category
  
  data1 <- df  %>%
    mutate(Category = factor(Category, levels = levels),
           ymin = round(cumsum(Value), 3),
           ymax = lag(cumsum(Value), default = 0),
           xmin = c(head(Category, -1), NA),
           xmax = c(tail(Category, -1), NA),
           Impact = ifelse(Category %in% c("TTL"),"Balance",
                           ifelse(Value > 0, "Increase", "Decrease")
           ))
  
  g <- ggplot(data1) +
    theme_bw()+
    theme(legend.position = "right", panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(y = "USD Millions", x = "Category", title = paste(category_v,yyyy_v, collapse = "_"))
  
  w <- 0.4  #use to set width of bars
  
  g <- g +
    geom_rect(aes(xmin = as.integer(Category) - w/2,
                  xmax = as.integer(Category) + w/2, ymin = ymin, ymax = ymax,
                  fill = Impact), colour = "black") +
    scale_x_discrete(limits = levels) +
    scale_fill_manual(values = (c("Decrease" = "blue", "Increase" = "orange", "Balance" = "black")))
  
  g <- g +
    geom_segment(data = data1[1:(nrow(data1) -1),],aes(x = xmin,
                                                       xend = xmax,
                                                       y = ymin,
                                                       yend = ymin))
  
  return(g)
}

facet_plot <- function(category_v){
  
  df <- apple %>%
    filter(category == category_v) %>%
    select(yyyy,item,value)
  
  g <- ggplot(df, aes(yyyy,value)) + geom_bar(stat="identity") + 
    facet_wrap(~item) +
    theme_bw() +
    theme(legend.position = "right", panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(y = "USD Million", x = "Year", title = category_v)
  
  return(g)
}

balance_termdebt_plot <- function(category_v, yyyy_v){
  
  df_org <- apple %>%
    filter(category == category_v & yyyy == yyyy_v) %>%
    select(yyyy,item,value)
  
  df <- data.frame(Category = df_org$item, Value2 = df_org$value)
  
  df <- df %>%
    mutate(Value = ifelse(Category == "NET", Value2 * -1 , Value2))
  
  levels <- df$Category
  
  data1 <- df  %>%
    mutate(Category = factor(Category, levels = levels),
           ymin = round(cumsum(Value), 3),
           ymax = lag(cumsum(Value), default = 0),
           xmin = c(head(Category, -1), NA),
           xmax = c(tail(Category, -1), NA),
           Impact = ifelse(Category %in% c("TTL","NET"),"Balance",
                           ifelse(Value > 0, "Increase", "Decrease")
           ))
  
  g <- ggplot(data1) +
    theme_bw()+
    theme(legend.position = "right", panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(y = "USD Millions", x = "Category", title = paste(category_v,yyyy_v, collapse = "_"))
  
  w <- 0.4  #use to set width of bars
  
  g <- g +
    geom_rect(aes(xmin = as.integer(Category) - w/2,
                  xmax = as.integer(Category) + w/2, ymin = ymin, ymax = ymax,
                  fill = Impact), colour = "black") +
    scale_x_discrete(limits = levels) +
    scale_fill_manual(values = (c("Decrease" = "blue", "Increase" = "orange", "Balance" = "black")))
  
  g <- g +
    geom_segment(data = data1[1:(nrow(data1) -1),],aes(x = xmin,
                                                       xend = xmax,
                                                       y = ymin,
                                                       yend = ymin))
  
  return(g)
}

fill_bar_plot <- function(category_v){
  
  df <- apple %>%
    filter(category == category_v) %>%
    select(yyyy,item,value)
  
  g <- ggplot(df, aes(yyyy,value,fill=item)) + 
    geom_bar(stat="identity") +
    theme_bw() +
    theme(legend.position = "right", panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(y = "USD Million", x = "Year", title = category_v)
  
  return(g)
}

facet_trans_plot <- function(category_v){
  
  df <- apple %>%
    filter(category == category_v) %>%
    select(yyyy,item,value)
  
  g <- ggplot(df, aes(item,value)) + geom_bar(stat="identity") + 
    facet_wrap(~yyyy) +
    theme_bw() +
    theme(legend.position = "right", panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(y = "USD Million", x = "Year", title = category_v)
  
  return(g)
}

balance_wf_plot2 <- function(category_v, yyyy_v, y_title){
  
  df <- apple %>%
    filter(category == category_v & yyyy == yyyy_v) %>%
    select(yyyy,item,value)
  
  df <- data.frame(Category = df$item, Value2 = df$value)
  
  df <- df %>%
    mutate(Value = ifelse(Category == "Ending", Value2 * -1 , Value2))
  
  levels <- df$Category
  
  data1 <- df  %>%
    mutate(Category = factor(Category, levels = levels),
           ymin = round(cumsum(Value), 3),
           ymax = lag(cumsum(Value), default = 0),
           xmin = c(head(Category, -1), NA),
           xmax = c(tail(Category, -1), NA),
           Impact = ifelse(Category %in% c(as.character(df$Category[1]), as.character(df$Category[nrow(df)])),"Balance",
                           ifelse(Value > 0, "Increase", "Decrease")
           ))
  
  g <- ggplot(data1) +
    theme_bw()+
    theme(legend.position = "right", panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(y = y_title, x = category_v, title = paste(category_v, yyyy_v, collapse = "_"))
  
  w <- 0.4  #use to set width of bars
  
  g <- g +
    geom_rect(aes(xmin = as.integer(Category) - w/2,
                  xmax = as.integer(Category) + w/2, ymin = ymin, ymax = ymax,
                  fill = Impact), colour = "black") +
    scale_x_discrete(limits = levels) +
    scale_fill_manual(values = (c("Decrease" = "blue", "Increase" = "orange", "Balance" = "black")))
  
  g <- g +
    geom_segment(data = data1[1:(nrow(data1) -1),],aes(x = xmin,
                                                       xend = xmax,
                                                       y = ymin,
                                                       yend = ymin))
  
  return(g)
}

facet_yyyy_plot <- function(category_v,exclude_v,y_title){
  
  df <- apple%>% 
    filter(category == "Common_Stock" & !item %in% exclude_v) %>%
    select(yyyy,item,value)
  
  g <- ggplot(df, aes(yyyy,value)) + geom_bar(stat="identity") + 
    facet_wrap(~item) +
    theme_bw() +
    theme(legend.position = "right", panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(y = y_title, x = "Year", title = category_v)
  
  return(g)
}
