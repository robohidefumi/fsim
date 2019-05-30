library(blscrapeR)
set_bls_key("113ed2250a5c4080b4f9d4853a584679")
readRenviron("~/.Renviron")
Sys.getenv("BLS_KEY")
setwd("~/pj/fsim")
df_catalog <- read.delim("catalog.txt")
df_fd <- bls_api(c("WPSFD4","WPUFD49116","WPUFD41","WPUFD42"),startyear = 2009, endyear = 2019,
                 catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_ppi <- left_join(df_fd, df_catalog, by = "seriesID")
ggplot(df_ppi, aes(x=date, y=value,colour=title)) + geom_line()

df_service <- bls_api(c("WPUFD42","WPUFD421","WPUFD422","WPUFD423"),startyear = 2009, endyear = 2019,
                 catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_service_title <- left_join(df_service, df_catalog, by = "seriesID")
ggplot(df_service_title, aes(x=date, y=value,colour=title)) + geom_line()

df_im <- bls_api(c("WPUID61","WPUID62","WPUID63"),startyear = 2009, endyear = 2019,
                      catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_im_title <- left_join(df_im, df_catalog, by = "seriesID")
ggplot(df_im_title, aes(x=date, y=value,colour=title)) + geom_line()

df_stage <- bls_api(c("WPUID54","WPUID53","WPUID52","WPUID51"),startyear = 2009, endyear = 2019,
                 catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_stage_title <- left_join(df_stage, df_catalog, by = "seriesID")
ggplot(df_stage_title, aes(x=date, y=value,colour=title)) + geom_line()

df_sp <- bls_api(c("WPSFD49207","WPSFD4131","WPU00000000","WPU03THRU15"),startyear = 2009, endyear = 2019,
                    catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_sp_title <- left_join(df_sp, df_catalog, by = "seriesID")
ggplot(df_sp_title, aes(x=date, y=value,colour=title)) + geom_line()

df_sp <- bls_api(c("WPSFD49207","WPSFD4131","WPU00000000","WPU03THRU15"),startyear = 2009, endyear = 2019,
                 catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_sp_title <- left_join(df_sp, df_catalog, by = "seriesID")
ggplot(df_sp_title, aes(x=date, y=value,colour=title)) + geom_line()

df_energy <- bls_api(c("WPU0571","WPU057303","WPU0561"),startyear = 2009, endyear = 2019,
                 catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_energy_title <- left_join(df_energy, df_catalog, by = "seriesID")
ggplot(df_energy_title, aes(x=date, y=value,colour=title)) + geom_line()

df_food <- bls_api(c("WPU0221","WPU029","WPU012"),startyear = 2009, endyear = 2019,
                     catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_food_title <- left_join(df_food, df_catalog, by = "seriesID")
ggplot(df_food_title, aes(x=date, y=value,colour=title)) + geom_line()

df_material <- bls_api(c("WPU0638","WPU061","WPU081"),startyear = 2009, endyear = 2019,
                   catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_material_title <- left_join(df_material, df_catalog, by = "seriesID")
ggplot(df_material_title, aes(x=date, y=value,colour=title)) + geom_line()

df_healthcare <- bls_api(c("WPU5111","WPU5121"),startyear = 2009, endyear = 2019,
                       catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_healthcare_title <- left_join(df_healthcare, df_catalog, by = "seriesID")
ggplot(df_healthcare_title, aes(x=date, y=value,colour=title)) + geom_line()

df_retail <- bls_api(c("WPU5811","WPU5831"),startyear = 2009, endyear = 2019,
                         catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_retail_title <- left_join(df_retail, df_catalog, by = "seriesID")
ggplot(df_retail_title, aes(x=date, y=value,colour=title)) + geom_line()

df_machine <- bls_api(c("WPU571101","WPU1017","WPU101211"),startyear = 2009, endyear = 2019,
                     catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_machine_title <- left_join(df_machine, df_catalog, by = "seriesID")
ggplot(df_machine_title, aes(x=date, y=value,colour=title)) + geom_line()

df_trans <- bls_api(c("WPU3022","WPU3012","WPU1411"),startyear = 2009, endyear = 2019,
                      catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_trans_title <- left_join(df_trans, df_catalog, by = "seriesID")
ggplot(df_trans_title, aes(x=date, y=value,colour=title)) + geom_line()

df_biz <- bls_api(c("WPU3911","WPU4011","WPU4511"),startyear = 2009, endyear = 2019,
                    catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_biz_title <- left_join(df_biz, df_catalog, by = "seriesID")
ggplot(df_biz_title, aes(x=date, y=value,colour=title)) + geom_line()

library(ggplot2)
df09_19 <- bls_api(c("WPSFD4"),startyear = 2009, endyear = 2019,
                   Sys.getenv("BLS_KEY")) %>% dateCast()


ggplot(df09_19, aes(x=date, y=value,colour=seriesID)) + geom_line()

df_plot <- df09_19 %>% arrange(desc(date)) %>% head(3)
ggplot(df_plot, aes(x=date, y=value)) + geom_line()

df_plot <- df09_19 %>% arrange(desc(date)) %>% head(6)
ggplot(df_plot, aes(x=date, y=value)) + geom_line()

df_plot <- df09_19 %>% arrange(desc(date)) %>% head(12)
ggplot(df_plot, aes(x=date, y=value)) + geom_line()

df_plot <- df09_19 %>% arrange(desc(date)) %>% head(36)
ggplot(df_plot, aes(x=date, y=value)) + geom_line()

df_plot <- df09_19 %>% arrange(desc(date)) %>% head(60)
ggplot(df_plot, aes(x=date, y=value)) + geom_line()

df_plot <- df09_19 %>% arrange(desc(date)) %>% head(120)
ggplot(df_plot, aes(x=date, y=value)) + geom_line()


gg <- ggplot(df_plot, aes(x=date, y=value)) + geom_line() + ggtitle("Last 3 monts")

df_fd <- bls_api(c("WPSFD4","WPUFD49116","WPUFD41","WPUFD42"),startyear = 2009, endyear = 2019,
                catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

ggplot(df_fd, aes(x=date, y=value,colour=seriesID)) + geom_line()

df_fd


df_fd


WPUFD49116
WPUFD41
WPUFD42

df_test <- bls_api(c("LAUCN040010000000005", "LAUCN040010000000006"),
              startyear = "2010", endyear = "2012",
              registrationKey = "BLS_KEY",
              calculations = TRUE, annualaverage = TRUE, catalog = TRUE)


library(blscrapeR)
library(tidyverse)
setwd("~/pj/fsim")
df_catalog_lfs <- read_tsv("catalog_lfs.txt",col_names=FALSE)
names(df_catalog_lfs) <- c("name", "seriesID")

df_unr_40 <- bls_api(c("LNS13327709", "LNS14000000"),startyear = 1940, endyear = 1959,
                         catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_unr_60 <- bls_api(c("LNS13327709", "LNS14000000"),startyear = 1960, endyear = 1979,
                         catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_unr_80 <- bls_api(c("LNS13327709", "LNS14000000"),startyear = 1980, endyear = 1999,
                     catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_unr_00 <- bls_api(c("LNS13327709", "LNS14000000"),startyear = 2000, endyear = 2019,
                     catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_unr_00_new <- df_unr_00[, colnames(df_unr_00) != "latest"]
df_unr <- rbind(df_unr_40,df_unr_60,df_unr_80,df_unr_00_new)
df_unr <- left_join(df_unr, df_catalog_lfs, by = "seriesID")

highchart(type = "stock") %>% 
  hc_title(text = "Rolling 24-Month kurtosis") %>% 
  hc_add_series(rolling_kurt_xts,
                name = "Rolling 24-Month kurtosis",
                color = "cornflowerblue") %>% 
  hc_yAxis(title = list(text = "kurtosis"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = FALSE)


df_unr %>%
  ggvis(~date, ~value,stroke = ~name) %>%
    layer_lines() %>%
      add_relative_scales() %>%
      add_legend('stroke', title = "Unemployment",
                 properties=legend_props(
                  legend=list(x=scaled_value("x_rel", 0), 
                              y=scaled_value("y_rel", 1)
      )))

library(lubridate)
today <- Sys.Date()
floor_date(today,"month")
end_date <- floor_date(today,"month") - months(1)
start_date <- end_date - months(12)

df_unr %>%
  filter(date >= start_date) %>%
  ggvis(~date, ~value,stroke = ~name) %>% layer_lines() %>% add_legend('stroke',orient="bottom")

ggplot(df_unr, aes(x=date, y=value,colour=name)) + geom_line() + ggtitle("60yrs")

df_plot <- df_unr %>% arrange(desc(date)) %>% head(24)
ggplot(df_plot, aes(x=date, y=value,colour=name)) + geom_line() + ggtitle("12 months")

df_plot <- df_unr %>% arrange(desc(date)) %>% head(72)
ggplot(df_plot, aes(x=date, y=value,colour=name)) + geom_line() + ggtitle("3 yrs")

df_nonfarm_30 <- bls_api(c("CES0000000001"),startyear = 1930, endyear = 1949,
                         catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_nonfarm_50 <- bls_api(c("CES0000000001"),startyear = 1950, endyear = 1969,
                         catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_nonfarm_70 <- bls_api(c("CES0000000001"),startyear = 1970, endyear = 1989,
                      catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_nonfarm_90 <- bls_api(c("CES0000000001"),startyear = 1990, endyear = 2009,
                         catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_nonfarm_10 <- bls_api(c("CES0000000001"),startyear = 2010, endyear = 2019,
                         catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_nonfarm <- rbind(df_nonfarm_30,df_nonfarm_50,df_nonfarm_70,df_nonfarm_90)

df_nonfarm_10_new <- df_nonfarm_10[, colnames(df_nonfarm_10) != "latest"]
df_catalog_ces <- read_tsv("catalog_ces.txt",col_names=FALSE)
names(df_catalog_ces) <- c("name", "seriesID")

df_nonfarm <- rbind(df_nonfarm,df_nonfarm_10_new)
df_nonfarm <- left_join(df_nonfarm, df_catalog_ces, by = "seriesID")

df_nonfarm %>%
  ggvis(~date, ~value,stroke = ~name) %>% layer_lines()

df_nonfarm %>%
  arrange(date) %>%
  mutate(incremental = value - lag(value,n=1,default = NA)) %>%
  mutate(lag = lag(value,n=1,default = NA)) %>%
  select(date,value,lag,incremental)

df_nonfarm %>%
  filter(value == 71241) %>%
  select(date,value)

df_nonfarm %>%
  filter(format(date,"%Y") == "1969") %>%

df_nonfarm %>%
  mutate(incremental = value - lead(value,n=1,default = NA)) %>%
  ggvis(~date, ~incremental,stroke = ~name) %>% layer_lines()

ggplot(df_nonfarm, aes(x=date, y=value,colour=name)) + geom_line() + ggtitle("90yrs")

### 6month
df_plot <- df_nonfarm %>% arrange(desc(date)) %>% head(6)
ggplot(df_plot, aes(x=date, y=value,colour=name)) + geom_line() + ggtitle("6 month")

#####
df_tpa_00 <- bls_api(c("CES0500000003"),startyear = 2000, endyear = 2019,
                     catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_tpa <- left_join(df_tpa_00, df_catalog_ces, by = "seriesID")
ggplot(df_tpa, aes(x=date, y=value,colour=name)) + geom_line() + ggtitle("20yrs")

install.packages("ggvis")
pressure %>% ggvis(~temperature, ~pressure) %>% layer_lines()

library(ggvis)
df_tpa %>%
  ggvis(~date, ~value,stroke = ~name) %>% layer_lines()

df_cpi <- bls_api(c("CUUR0000SA0"),startyear = 2010, endyear = 2019,
                  catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_cpi %>%
  ggvis(~date, ~value,stroke = ~seriesID) %>% layer_lines()

df_cpi %>%
  arrange(date) %>% 
  mutate(incremental = value - lag(value,n=1,default = NA)) %>% 
  ggvis(~date, ~incremental,stroke = ~seriesID) %>% layer_lines()


 df_cpi_adj <- bls_api(c("CUSR0000SA0"),startyear = 2010, endyear = 2019,
                   catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
 
 library(ggvis)
 df_cpi %>%
   arrange(date) %>% 
   mutate(incremental = value - lag(value,n=1,default = NA)) %>% 
   ggvis(~date, ~incremental,stroke = ~seriesID) %>% layer_lines()
 
 df_cpi_adj %>%
   arrange(desc(date)) %>% 
   head(12) %>% 
   arrange(date) %>%  
   mutate(incremental = (value / lag(value,n=1,default = NA) - 1) *100) %>% 
   ggvis(~date, ~incremental,stroke = ~seriesID) %>% layer_lines()
 
 df_cpi_adj %>%
   arrange(desc(date)) %>% 
   head(12) %>% 
   arrange(date) %>%  
   mutate(incremental = (value / lag(value,n=1,default = NA) - 1) *100) %>% 
   ggplot(aes(date,incremental)) + geom_bar(stat = "identity")
 
 
 df_cpi_non <- bls_api(c("CUUR0000SA0"),startyear = 2010, endyear = 2019,
                   catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
 
 df_cpi_non %>%
   arrange(desc(date)) %>% 
   head(24) %>% 
   arrange(date) %>%
   mutate(incremental = round((value / lag(value,n=12,default = NA) - 1) * 100, 1)) %>% 
   tail(12) %>% 
   ggvis(~date, ~incremental,stroke = ~seriesID) %>% layer_lines()
 
# Hourly Earnings All Employees(CES0500000003)
df_hea <- bls_api(c("CES0500000003"),startyear = 2010, endyear = 2019,
                       catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_hea %>%
  arrange(desc(date)) %>% 
  ggvis(~date, ~value,stroke = ~seriesID) %>% layer_lines()

hea_12 <- df_hea %>%
  arrange(desc(date)) %>% 
  head(13) %>% 
  arrange(date) %>%  
  mutate(incremental = (value / lag(value,n=1,default = NA) - 1) *100) %>% 
  tail(12) 

cpi_adj_12 <- df_cpi_adj %>%
  arrange(desc(date)) %>% 
  head(13) %>%  
  arrange(date) %>%  
  mutate(incremental = (value / lag(value,n=1,default = NA) - 1) *100)%>% 
  tail(12) 

hea_cpi_12 <- left_join(hea_12, cpi_adj_12, by = "date")
hea_cpi_12 %>% 
  mutate(mom = incremental.x - incremental.y) %>%
  ggvis(~date, ~mom,stroke = ~seriesID.x) %>% layer_lines()

#Hourly Earnings production and nonsupervisory(CES0500000008)
df_hep <- bls_api(c("CES0500000008"),startyear = 2010, endyear = 2019,
                  catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_hep %>%
  arrange(desc(date)) %>% 
  ggvis(~date, ~value,stroke = ~seriesID) %>% layer_lines()

hep_12 <- df_hep %>%
  arrange(desc(date)) %>% 
  head(13) %>% 
  arrange(date) %>%  
  mutate(incremental = (value / lag(value,n=1,default = NA) - 1) *100) %>% 
  tail(12) 

cpiw_adj_12 <- df_cpiw_adj %>%
  arrange(desc(date)) %>% 
  head(13) %>%  
  arrange(date) %>%  
  mutate(incremental = (value / lag(value,n=1,default = NA) - 1) *100)%>% 
  tail(12) 

hep_cpiw_12 <- left_join(hep_12, cpiw_adj_12, by = "date")
hep_cpiw_12 %>% 
  mutate(mom = incremental.x - incremental.y) %>%
  ggvis(~date, ~mom,stroke = ~seriesID.x) %>% layer_lines()

####
df_cpiw_adj <- bls_api(c("CWSR0000SEEE"),startyear = 2010, endyear = 2019,
                      catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

cpiw_adj_12 <- df_cpiw_adj %>%
  arrange(desc(date)) %>% 
  head(13) %>%  
  arrange(date) %>%  
  mutate(incremental = (value / lag(value,n=1,default = NA) - 1) *100)%>% 
  tail(12) 
  
library(ggvis)
df_cpiw_adj %>%
  arrange(date) %>% 
  mutate(incremental = value - lag(value,n=1,default = NA)) %>% 
  ggvis(~date, ~incremental,stroke = ~seriesID) %>% layer_lines()


df_cpiw_adj %>%
  arrange(desc(date)) %>% 
  head(12) %>% 
  arrange(date) %>%  
  mutate(incremental = (value / lag(value,n=1,default = NA) - 1) *100) %>% 
  ggplot(aes(date,incremental)) + geom_bar(stat = "identity")

##### Job Opening/Hire/Separation 
### Opening
df_open_2009 <- bls_api(c("JTS00000000JOR"),startyear = 2000, endyear = 2009,
                   catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_open <- bls_api(c("JTS00000000JOR"),startyear = 2010, endyear = 2019,
                  catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_open_new <- df_open[, colnames(df_open) != "latest"]

df_open_all <- rbind(df_open_2009,df_open_new)

df_open_all %>% 
  ggplot(aes(x=date, y=value,colour=seriesID)) + 
  geom_line(color = "cornflowerblue") +
  ggtitle("Job Opening Rate")

df_open_all %>% 
  arrange(date) %>% 
  tail(24) %>% 
  ggplot(aes(x=date, y=value,colour=seriesID)) + 
  geom_line(color = "cornflowerblue") +
  ggtitle("Job Opening Rate")

##### Hire & Separation
df_hs_2009 <- bls_api(c("JTS00000000HIR","JTS00000000TSR"),startyear = 2000, endyear = 2009,
                      catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_hs <- bls_api(c("JTS00000000HIR","JTS00000000TSR"),startyear = 2010, endyear = 2019,
                   catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_hs_new <- df_hs[, colnames(df_hs) != "latest"]

df_hs_all <- rbind(df_hs_2009,df_hs_new)


df_hs_all %>% 
  ggplot(aes(x=date, y=value,colour=seriesID)) + 
  geom_line() +
  ggtitle("Hire & Separation Rate")

df_hs %>% 
  arrange(date) %>% 
  tail(24) %>% 
  ggplot(aes(x=date, y=value,colour=seriesID)) + 
  geom_line() +
  ggtitle("Hire & Separation Rate")

##### Labor Productivity
df_laborproductivity <- bls_api(c("PRS85006092","PRS85006112","PRS85006091"),
                                startyear = 2010, endyear = 2019,
                 catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_laborproductivity %>% 
  ggplot(aes(x=date, y=value,colour=seriesID)) + 
  geom_line() +
  ggtitle("Labor & Productivity")

###### Import/Export Price EIUIR/EIUIQ
df_tradeprice_90 <- bls_api(c("EIUIR","EIUIQ"),startyear = 1990, endyear = 1999,
                            catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_tradeprice_00 <- bls_api(c("EIUIR","EIUIQ"),startyear = 2000, endyear = 2009,
                         catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_tradeprice_10 <- bls_api(c("EIUIR","EIUIQ"),startyear = 2010, endyear = 2019,
                                catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()


df_tradeprice_new <- df_tradeprice_10[, colnames(df_tradeprice_10) != "latest"]

df_tradeprice_all <- rbind(df_tradeprice_new,df_tradeprice_00,df_tradeprice_90)


df_tradeprice_all %>% 
  ggplot(aes(x=date, y=value,colour=seriesID)) + 
  geom_line() +
  ggtitle("Trade Price")

df_tradeprice_all %>%
  #filter(seriesID == "EIUIQ") %>%  
  arrange(desc(date)) %>% 
  head(24) %>% 
  arrange(date) %>%  
  mutate(inc_m = (value / lag(value,n=2,default = NA) - 1) *100) %>% 
  ggplot(aes(x=date, y=inc_m,colour=seriesID)) + 
  geom_line() +
  ggtitle("Trade Price: Month")

df_tradeprice_all %>%
  #filter(seriesID == "EIUIQ") %>%  
  arrange(desc(date)) %>% 
  head(48) %>% 
  arrange(date) %>%  
  mutate(inc_a = (value / lag(value,n=24,default = NA) - 1) *100) %>% 
  tail(24) %>% 
  ggplot(aes(x=date, y=inc_a,colour=seriesID)) + 
  geom_line() +
  ggtitle("Trade Price: year")

#### 2019.05.24

tra_wide <- df_tradeprice_all %>% 
  spread(seriesID,value) %>% 
  select(date,EIUIQ,EIUIR)

tra_xts <- xts(tra_wide[,-1],order.by = as.Date(tra_wide$date))
tra_xts <- tra_xts %>% na.omit()

highchart(type = "stock") %>% 
  hc_title(text = "Import/Export") %>% 
  hc_add_series(tra_xts$EIUIQ,name = "Export") %>% 
  hc_add_series(tra_xts$EIUIR,name = "Import") %>% 
  hc_yAxis(title = list(text = "price"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = FALSE) %>% 
  hc_legend(enabled = TRUE)

####### 2019.05.25 ##############

df_lp <- bls_api(c("PRS85006092","PRS85006112","PRS85006091"),
                                startyear = 2010, endyear = 2019,
                                catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

lp_wide <- df_lp %>% 
  spread(seriesID,value) %>% 
  select(date,PRS85006092,PRS85006112,PRS85006091)

lp_xts <- xts(lp_wide[,-1],order.by = as.Date(lp_wide$date))
lp_xts <- lp_xts %>% na.omit()

highchart(type = "stock") %>% 
  hc_title(text = "Major Sector Productivity and Costs") %>% 
  hc_add_series(lp_xts$PRS85006092,name = "Labor productivity (output per hour)") %>% 
#  hc_add_series(lp_xts$PRS85006112,name = "Import") %>% 
#  hc_add_series(lp_xts$PRS85006091,name = "Import") %>% 
  hc_yAxis(title = list(text = "Percent change from previous quarter at annual rate"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = FALSE) %>% 
  hc_legend(enabled = TRUE)

highchart(type = "stock") %>% 
  hc_title(text = "Major Sector Productivity and Costs") %>% 
  #hc_add_series(lp_xts$PRS85006092,name = "Labor productivity (output per hour)") %>% 
  #hc_add_series(lp_xts$PRS85006112,name = "Import") %>% 
  hc_add_series(lp_xts$PRS85006091,name = "Labor productivity (output per hour)") %>% 
  hc_yAxis(title = list(text = "Percent change from same quarter a year ago"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = FALSE) %>% 
  hc_legend(enabled = TRUE)

highchart(type = "stock") %>% 
  hc_title(text = "Major Sector Productivity and Costs") %>% 
  #hc_add_series(lp_xts$PRS85006092,name = "Labor productivity (output per hour)") %>% 
  hc_add_series(lp_xts$PRS85006112,name = "Unit labor costs") %>% 
  #  hc_add_series(lp_xts$PRS85006091,name = "Import") %>% 
  hc_yAxis(title = list(text = "Percent change from previous quarter at annual rate"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = FALSE) %>% 
  hc_legend(enabled = TRUE)

highchart(type = "stock") %>% 
  hc_title(text = "Major Sector Productivity and Costs") %>% 
  #hc_add_series(lp_xts$PRS85006092,name = "Labor productivity (output per hour)") %>% 
  #hc_add_series(lp_xts$PRS85006112,name = "Unit labor costs") %>% 
    hc_add_series(lp_xts$PRS85006091,name = "Import") %>% 
  hc_yAxis(title = list(text = "Percent change from previous quarter at annual rate"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = FALSE) %>% 
  hc_legend(enabled = TRUE)

#### 2019.05.26
##### CPI

df_cpi_910 <- bls_api(c("CUSR0000SA0","CUUR0000SA0"),startyear = 1910, endyear = 1919,
                     catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_cpi_920 <- bls_api(c("CUSR0000SA0","CUUR0000SA0"),startyear = 1920, endyear = 1929,
                     catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_cpi_930 <- bls_api(c("CUSR0000SA0","CUUR0000SA0"),startyear = 1930, endyear = 1939,
                     catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_cpi_940 <- bls_api(c("CUSR0000SA0","CUUR0000SA0"),startyear = 1940, endyear = 1949,
                     catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_cpi_950 <- bls_api(c("CUSR0000SA0","CUUR0000SA0"),startyear = 1950, endyear = 1959,
                     catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_cpi_960 <- bls_api(c("CUSR0000SA0","CUUR0000SA0"),startyear = 1960, endyear = 1969,
                     catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_cpi_970 <- bls_api(c("CUSR0000SA0","CUUR0000SA0"),startyear = 1970, endyear = 1979,
                     catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_cpi_980 <- bls_api(c("CUSR0000SA0","CUUR0000SA0"),startyear = 1980, endyear = 1989,
                     catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_cpi_990 <- bls_api(c("CUSR0000SA0","CUUR0000SA0"),startyear = 1990, endyear = 1999,
                            catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_cpi_000 <- bls_api(c("CUSR0000SA0","CUUR0000SA0"),startyear = 2000, endyear = 2009,
                            catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_cpi_010 <- bls_api(c("CUSR0000SA0","CUUR0000SA0"),startyear = 2010, endyear = 2019,
                      catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()

df_cpi_010 <- df_cpi_010[, colnames(df_cpi_010) != "latest"]

df_cpi_all <- rbind(df_cpi_010,df_cpi_000,df_cpi_990,df_cpi_980,df_cpi_970,df_cpi_960,
                    df_cpi_950,df_cpi_940,df_cpi_930,df_cpi_920,df_cpi_910)

cpi_wide <- df_cpi_all %>% 
  spread(seriesID,value) %>% 
  select(date,CUSR0000SA0,CUUR0000SA0)

cpi_xts <- xts(cpi_wide[,-1],order.by = as.Date(cpi_wide$date))
cpi_xts <- cpi_xts %>% na.omit()

highchart(type = "stock") %>% 
  hc_title(text = "CPI-All Urban Consumers") %>% 
  hc_add_series(cpi_xts$CUSR0000SA0,name = "Seasonal Adjusted") %>% 
  hc_add_series(cpi_xts$CUUR0000SA0,name = "Non-Adjusted") %>% 
  hc_yAxis(title = list(text = "Index(1982-84=100)"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = FALSE) %>% 
  hc_legend(enabled = TRUE)

cpi_monthly <- to.monthly(cpi_xts,
                             indexAt = "firstof",
                             OHLC = FALSE)
cpi_xts_inc <-  
  Return.calculate(cpi_monthly,
                   method = "discrete") %>%
  na.omit()

highchart(type = "stock") %>% 
  hc_title(text = "CPI-All Urban Consumers") %>% 
  hc_add_series(cpi_xts_inc$CUSR0000SA0 * 100,name = "Seasonal Adjusted") %>% 
  hc_add_series(cpi_xts_inc$CUUR0000SA0 * 100,name = "Non-Adjusted") %>% 
  hc_yAxis(title = list(text = "Index(1982-84=100)"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = FALSE) %>% 
  hc_legend(enabled = TRUE)

seq(1940,2019,by=10)

f <- function(x){
  bls_api(c("CUUR0400SA0L1E","CUUR0000SA0"),startyear = x, endyear = x+9,
          catalog = TRUE, Sys.getenv("BLS_KEY")) %>%
  dateCast() %>%
  select(date,seriesID,value)
}
x <- seq(1940,2019,by=10) 
unlist(map(x ,f))

df_cpilfe_000 <- bls_api(c("CUUR0400SA0L1E","CUUR0000SA0"),startyear = 2000, endyear = 2009,
                         catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()
df_cpilfe_010 <- bls_api(c("CUUR0400SA0L1E","CUUR0000SA0"),startyear = 2010, endyear = 2019,
                      catalog = TRUE, Sys.getenv("BLS_KEY")) %>% dateCast()


cpilfe_wide <- df_cpilfe_010 %>% 
  spread(seriesID,value) %>% 
  select(date,CUUR0400SA0L1E,CUUR0000SA0)

cpilfe_xts <- xts(cpilfe_wide[,-1],order.by = as.Date(cpilfe_wide$date))
cpilfe_xts <- cpilfe_xts %>% na.omit()

highchart(type = "stock") %>% 
  hc_title(text = "CPI-All Urban Consumers") %>% 
  hc_add_series(cpilfe_xts$CUUR0400SA0L1E,name = "Less foods & energy") %>% 
  hc_add_series(cpilfe_xts$CUUR0000SA0,name = "All") %>% 
  hc_yAxis(title = list(text = "Index(1982-84=100)"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = FALSE) %>% 
  hc_legend(enabled = TRUE)


df_cpi_010 <- df_cpi_010[, colnames(df_cpi_010) != "latest"]

