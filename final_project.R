library(RMySQL)
library(ggplot2)
library(xts)
library(DBI)
library(RMySQL)
library(dplyr)
library(plotly)
library(purrr)
library(quantmod)
library(treemapify)
library(RColorBrewer)
#期货主力合约数据
ZZ_pnl <- dbGetQuery(mydb,"select trade_date,trade_code,price_change_pct from cn_future.daily_quote where trade_code in (\"SF912\",\"AP912\",\"CJ912\",\"ZC912\") order by trade_date desc;")
DL_pnl <- dbGetQuery(mydb,"select trade_date,trade_code,price_change_pct from cn_future.daily_quote where trade_code in (\"I1912\",\"M1912\",\"JD1912\",\"Y1912\") order by trade_date desc;")
SH_pnl <- dbGetQuery(mydb,"select trade_date,trade_code,price_change_pct from cn_future.daily_quote where trade_code in (\'RB1912\',\'AG1912\',\'FU1912\',\'PB1912\') order by trade_date desc;")
ZZ_pnl <- reshape(ZZ_pnl, idvar = "trade_date", timevar = "trade_code", direction = "wide")
DL_pnl <- reshape(DL_pnl, idvar = "trade_date", timevar = "trade_code", direction = "wide")
SH_pnl <- reshape(SH_pnl, idvar = "trade_date", timevar = "trade_code", direction = "wide")
ZZ_pnl <- xts(ZZ_pnl[,-1],as.Date(ZZ_pnl$trade_date))
DL_pnl <- xts(DL_pnl[,-1],as.Date(DL_pnl$trade_date))
SH_pnl <- xts(SH_pnl[,-1],as.Date(SH_pnl$trade_date))
names(ZZ_pnl) <-  unlist(strsplit(names(ZZ_pnl),'.',fixed = TRUE))[c(2,4,6,8)]
names(DL_pnl) <-  unlist(strsplit(names(DL_pnl),'.',fixed = TRUE))[c(2,4,6,8)]
names(SH_pnl) <-  unlist(strsplit(names(SH_pnl),'.',fixed = TRUE))[c(2,4,6,8)]




## Get Tushare Data
#SSE Composite Index
today = format(Sys.Date(),"%Y%m%d")
tushare <- Tushare::pro_api(token = "a060f5bc02599c4f873ae86e6f9197d83e27469834e3a07be4716df5")
SSE_Index <- tushare(api_name = "index_daily", ts_code = "000001.SH", start_date = '20180101', end_date = today,
                     fields='trade_date,open,high,low,close')
head(SSE_Index)
SSE_Index$trade_date<-as.Date(SSE_Index$trade_date,"%Y%m%d")
#Get trade dates
trade_date<-tushare(api_name = "trade_cal", start_date = '20191001', end_date = today)
trade_date <- trade_date[trade_date$is_open == 1,]
return_cal_dates <- trade_date$cal_date[(nrow(trade_date)-1):nrow(trade_date)]
#300 Industry indices
ind_returns <- data.frame()
ind_indices_code <-paste0(c('000908','000909','000910','000912','000913','000914','000915','000916','000917'),".SH")
for (code in ind_indices_code){
  ind_index <- tushare(api_name = "index_daily", ts_code = code, start_date = '20180830', end_date = '20180831')
  
  ind_returns[code,"returns"] <- ind_index$close[2]/ind_index$close[1] - 1
}
row.names(ind_returns)<- c("CSI 300 Energy","CSI 300 Materials","CSI 300 Industrials","CSI 300 Cons Staples",
     "CSI 300 Health Care","CSI 300 Financials","CSI 300 Info Tech","CSI 300 Telecom Svc","CSI 300 Utilities")
#SSE 50 Constituents
SSE50_constituents <- read.csv("C:/Users/Administrator/Desktop/final_project/000016closeweight.csv")
SSE50_codes <- paste0(SSE50_constituents$Constituent.Code,".SH")
SSE50_names <- SSE50_constituents$Constituent.Name
SSE50 <- data.frame()
for (code in SSE50_codes){
  stk <- tushare(api_name = "daily", ts_code = code, trade_date = return_cal_dates[2])
  stk_mv <- tushare(api_name = "daily_basic", ts_code = code,  trade_date = return_cal_dates[2])
  SSE50[code,"returns"] <- stk$close/stk$pre_close -1 
  SSE50[code,"mv"] <- stk_mv$total_mv
}
SSE50$names <- paste0(SSE50_names,'\n',round(SSE50$returns*100,2),'%')

###Draw figures
## plot candlestick
SSE_Index <- tail(SSE_Index,50)
SSE_candlestick <- SSE_Index %>%
  plot_ly(x = ~trade_date, type="candlestick",
          open = ~open, close = ~close,
          high = ~high, low = ~low) %>%
  layout(title = "SSE Composite Index")
SSE_candlestick
## plot industry indices barchart
to_color <- function (x){
  if (x > 0){
    return(sprintf("rgb(%d,0,0)",floor(255*(1-exp(-x*100)))));
  }
  else if (x<=0){
    return(sprintf("rgb(0,%d,0)",ceiling(255*(1-exp(x*100)))));
  }
}
ind_returns["color"] = map_chr(ind_returns$returns, to_color)
ind_indices_barchart <- plot_ly(y = reorder(row.names(ind_returns), ind_returns$returns), 
                                x = abs(ind_returns$returns),
                                marker =list(color = ind_returns$color),
                                type = 'bar',orientation = 'h') %>%
  add_annotations(xref = 'x1', yref = 'y',
                  x = abs(ind_returns$returns)+0.003 ,  y = row.names(ind_returns),
                  text = paste(round(ind_returns$returns*100, 2), '%'),
                  font = list(family = 'Arial', size = 12, color = 'rgb(0, 0, 0)'),
                  showarrow = FALSE)

ind_indices_barchart

## plot SSE50 constituents maps
rgb2hex <- function(rgb){
  rgb <- strsplit(substr(rgb,5,nchar(rgb)-1),',')[[1]]
  rgb <- as.integer(rgb)
  rgb <- as.character(as.hexmode(rgb))
  hex <- "#"
  for (i in rgb){
    if (nchar(i) == 1){i <- paste0('0',i)} 
    hex <- paste0(hex, i)
  }
  return(hex)
}

SSE50["color"] = map_chr(SSE50$returns, to_color) %>% map_chr(rgb2hex)
ggplot(SSE50, aes(area = mv, label = names)) +
  geom_treemap(fill = SSE50$color) +
  geom_treemap_text(fontface = "bold", colour = "white", place = "centre",
                    grow = TRUE)

