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