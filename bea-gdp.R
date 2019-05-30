install.packages('bea.R')
library(bea.R)
library(tidyr)

beaKey <- "8D061272-ADA9-41F8-8A01-51A5AE6222D3"
beaSearch('personal consumption', beaKey)
beaSearch('gross domestic', beaKey, asHtml = TRUE)

beaSpecs <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T20305',
  'Frequency' = 'Q',
  'Year' = '2019',
  'ResultFormat' = 'json'
);
beaPayload <- beaGet(beaSpecs);
beaLong <- beaGet(beaSpecs, asWide = FALSE)
beaStatTab <- beaGet(beaSpecs, iTableStyle = FALSE)
beaViz(beaPayload)

beaSearch("gross domestic product")

# percent  
beaSpecs <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10101',
  'LineNumber' = '1',
  'Frequency' = 'Q',
  'Year' = paste(1990:2019,collapse = ","),
  'ResultFormat' = 'json'
);
beaPayload <- beaGet(beaSpecs, asWide = FALSE);
df_plot <- beaPayload %>% filter(LineNumber == '1') %>% select(TimePeriod, DataValue)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)
gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=DataValue)) + geom_line()　+ ggtitle("GDP Quarterly")
plot(gg)

# GDP
# real number
beaSpecs <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10103',
  'Frequency' = 'Q',
  'Year' = paste(1970:2019,collapse = ","),
  'ResultFormat' = 'json'
);
beaPayload <- beaGet(beaSpecs, asWide = FALSE);
df_plot <- beaPayload %>% filter(LineNumber == '1') %>% select(TimePeriod, LineDescription,DataValue)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)
gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=DataValue)) + geom_line()　+ ggtitle("GDP Quarterly")
plot(gg)

# PCE
beaSpecs <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T20303',
  'Frequency' = 'Q',
  'Year' = paste(1970:2019,collapse = ","),
  'ResultFormat' = 'json'
);
beaPayload <- beaGet(beaSpecs, asWide = FALSE);
df_plot <- beaPayload %>% filter(LineNumber == '1') %>% select(TimePeriod, LineDescription,DataValue)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)
gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=DataValue)) + geom_line()　+ ggtitle("PCE Quarterly")
plot(gg)

# GDI
beaSpecs <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T50100',
  'Frequency' = 'Q',
  'Year' = paste(1970:2019,collapse = ","),
  'ResultFormat' = 'json'
);
beaPayload <- beaGet(beaSpecs, asWide = FALSE);
df_plot <- beaPayload %>% filter(LineNumber == '1') %>% select(TimePeriod, LineDescription,DataValue)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)
gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=DataValue)) + geom_line()　+ ggtitle("GDI Quarterly")
plot(gg)

# GCE
beaSpecs <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T30905',
  'Frequency' = 'Q',
  'Year' = paste(1970:2019,collapse = ","),
  'ResultFormat' = 'json'
);
beaPayload <- beaGet(beaSpecs, asWide = FALSE);
df_plot <- beaPayload %>% filter(LineNumber == '1') %>% select(TimePeriod, LineDescription,DataValue)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)
gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=DataValue)) + geom_line()　+ ggtitle("GCE Quarterly")
plot(gg)

# NEGS
beaSpecs <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T40100',
  'Frequency' = 'Q',
  'Year' = paste(1970:2019,collapse = ","),
  'ResultFormat' = 'json'
);
beaPayload <- beaGet(beaSpecs, asWide = FALSE);
df_plot <- beaPayload %>% filter(LineNumber == '2' | LineNumber == '18' ) %>% select(TimePeriod, SeriesCode, DataValue)
df_plot <- df_plot %>% spread(SeriesCode, DataValue)
df_plot <- df_plot %>% mutate(net = B020RC - B021RC)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)

gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=net)) + geom_line()　+ ggtitle("NEGS Quarterly")
plot(gg)

#################### 重ねグラフ
today <- Sys.Date()
yyyy <- format(today, "%Y")
end_yyyy <- as.numeric(yyyy)
start_yyyy <- end_yyyy - 50

# GDP
# real number
beaSpecs <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10105',
  'Frequency' = 'Q',
  'Year' = paste(start_yyyy:end_yyyy,collapse = ","),
  'ResultFormat' = 'json'
);
beaPayload <- beaGet(beaSpecs, asWide = FALSE);
df_plot <- beaPayload %>% filter(LineNumber == '1'| LineNumber == '2'| LineNumber == '7'| LineNumber == '15'| LineNumber == '22') %>% select(TableName,TimePeriod, LineDescription,DataValue)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)
gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=DataValue,
                 colour=LineDescription)) + geom_line() + ggtitle("GDP Quarterly") + theme(legend.position = "bottom") + theme(legend.direction = "vertical")
plot(gg)

beaSpecs <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10105',
  'Frequency' = 'Q',
  'Year' = paste(start_yyyy:end_yyyy,collapse = ","),
  'ResultFormat' = 'json'
);
beaPayload <- beaGet(beaSpecs, asWide = FALSE);
df_plot <- beaPayload %>% filter( LineNumber == '3'| LineNumber == '4'| LineNumber == '5') %>% select(TableName,TimePeriod, LineDescription,DataValue)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)
gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=DataValue,
                 colour=LineDescription)) + geom_line() + ggtitle("GDP Quarterly") + theme(legend.position = "bottom") + theme(legend.direction = "vertical")
plot(gg)
######
df_plot <- beaPayload %>% filter( LineNumber == '7'| LineNumber == '8'| LineNumber == '14') %>% select(TableName,TimePeriod, LineDescription,DataValue)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)
gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=DataValue,
                 colour=LineDescription)) + geom_line() + ggtitle("GDI Quarterly") + theme(legend.position = "bottom") + theme(legend.direction = "vertical")
plot(gg)
####
df_plot <- beaPayload %>% filter( LineNumber == '8'| LineNumber == '9'| LineNumber == '13') %>% select(TableName,TimePeriod, LineDescription,DataValue)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)
gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=DataValue,
                 colour=LineDescription)) + geom_line() + ggtitle("Fixed Investment Quarterly") + theme(legend.position = "bottom") + theme(legend.direction = "vertical")
plot(gg)
####
df_plot <- beaPayload %>% filter( LineNumber == '9'| LineNumber == '10'| LineNumber == '11'| LineNumber == '12') %>% select(TableName,TimePeriod, LineDescription,DataValue)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)
gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=DataValue,
                 colour=LineDescription)) + geom_line() + ggtitle("Fixed Investment Quarterly") + theme(legend.position = "bottom") + theme(legend.direction = "vertical")
plot(gg)

####
df_plot <- beaPayload %>% filter( LineNumber == '16'| LineNumber == '19') %>% select(TableName,TimePeriod, LineDescription,DataValue)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)
gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=DataValue,
                 colour=LineDescription)) + geom_line() + ggtitle("Fixed Investment Quarterly") + theme(legend.position = "bottom") + theme(legend.direction = "vertical")
plot(gg)
#####
df_plot <- beaPayload %>% filter( LineNumber == '16'| LineNumber == '17'| LineNumber == '18') %>% select(TableName,TimePeriod, LineDescription,DataValue)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)
gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=DataValue,
                 colour=LineDescription)) + geom_line() + ggtitle("Fixed Investment Quarterly") + theme(legend.position = "bottom") + theme(legend.direction = "vertical")
plot(gg)

df_plot <- beaPayload %>% filter( LineNumber == '19'| LineNumber == '20'| LineNumber == '21') %>% select(TableName,TimePeriod, LineDescription,DataValue)
df_plot$qdate <- as.yearqtr(df_plot$TimePeriod)
gg <- ggplot(df_plot, 
             aes(x=qdate, 
                 y=DataValue,
                 colour=LineDescription)) + geom_line() + ggtitle("Fixed Investment Quarterly") + theme(legend.position = "bottom") + theme(legend.direction = "vertical")
plot(gg)
