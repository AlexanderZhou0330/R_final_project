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
library(ggpubr)


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
SSE50_basics <- data.frame()
SSE50 <- data.frame()
for (code in SSE50_codes){
  stk <- tushare(api_name = "daily", ts_code = code, trade_date = return_cal_dates[1])
  stk_mv <- tushare(api_name = "daily_basic", ts_code = code,  trade_date = return_cal_dates[1])
  SSE50[code,"returns"] <- stk$close/stk$pre_close -1 
  SSE50[code,"vol"] <- stk$vol
  SSE50[code,"mv"] <- stk_mv$total_mv
  SSE50_basics <- rbind(SSE50_basics,stk_mv)
}
SSE50$names <- paste0(SSE50_names,'\n',round(SSE50$returns*100,2),'%')
SSE50_basics_new <- data.frame(row.names = 1:50)
SSE50_basics_new$Code <- SSE50_codes
SSE50_basics_new$Company <- SSE50_names
SSE50_basics_new$Exchange <- SSE50_constituents$Exchange
SSE50_basics_new["Market Cap"] <- SSE50_basics$total_mv
SSE50_basics_new["P/E"] <- SSE50_basics$pe
SSE50_basics_new$Price <- SSE50_basics$close
SSE50_basics_new$Change <- paste0(round(SSE50$returns*100,2),"%")
SSE50_basics_new$Volume <- SSE50$vol
###Draw figures
## Plot candlestick
SSE_Index <- tail(SSE_Index,50)
SSE_candlestick <- SSE_Index %>%
  plot_ly(x = ~trade_date, type="candlestick",
          open = ~open, close = ~close,
          high = ~high, low = ~low) %>%
  layout(title = "SSE Composite Index")
SSE_candlestick
## Plot industry indices barchart
to_color <- function (x){
  if (x > 0){
    return(sprintf("rgb(%d,0,0)",floor(255*(1-exp(-x*30)))));
  }
  else if (x<=0){
    return(sprintf("rgb(0,%d,0)",ceiling(255*(1-exp(x*30)))));
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

## Plot SSE50 constituents maps
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
                    grow = TRUE) + 
  ggtitle("上证50成分股涨跌情况")

## Generate the screener overview
SSE50_basics_new <- head(SSE50_basics_new,5)
tbody.style = tbody_style(color = "black",
                          fill = c("#bdbdbd","#ededed"), hjust=1, x=0.9)
ggtexttable(SSE50_basics_new,
            theme = ttheme(
              colnames.style = colnames_style(color = "white", fill = "#7d7d7d"),
              tbody.style = tbody.style
            ))
## Generate the line graph of individual stock
PingAn <- tushare(api_name = "daily", ts_code = '000001.SZ', start_date = '20190405', end_date = return_cal_dates[1])
PingAn_return = PingAn$close[nrow(PingAn)]/PingAn$pre_close[nrow(PingAn)] - 1 

PingAn_name <- list(
          xref = 'paper',
          yref = 'paper',
          x = 0.23,
          y = 0.99,
          xanchor = 'right',
          yanchor = 'middle',
          text = '平安银行\n',
          font = list(family = '楷体',
                      size = 20,
                      color = '#efefef'),
          showarrow = FALSE)
PingAn_price <- list(
  xref = 'paper',
  yref = 'paper',
  x = 0.20,
  y = 0.85,
  xanchor = 'right',
  yanchor = 'middle',
  text = ~as.character(PingAn$close[nrow(PingAn)]),
  font = list(family = 'Arial',
              size = 26,
              color = '#efefef'),
  showarrow = FALSE)

PingAn_change <- list(  
        xref = 'paper',
        yref = 'paper',
        x = 0.84,
        y = 0.98,
        xanchor = 'right',
        yanchor = 'middle',
        text = ~paste0(round(PingAn_return*100,2),'%'),
        font = list(family = 'Arial',
                    size = 20,
                    color = '#efefef'),
        showarrow = FALSE)

PingAn_HL <- list(  
  xref = 'paper',
  yref = 'paper',
  x = 0.84,
  y = 0.80,
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste0("H ",PingAn$high[nrow(PingAn)],"\nL ",PingAn$low[nrow(PingAn)]),
  font = list(family = 'Arial',
              size = 16,
              color = '#cfcfcf'),
  showarrow = FALSE)

PingAn_plot <- plot_ly(PingAn, y=~close,x=~paste0(substr(trade_date,5,6),'-',substr(trade_date,7,8)),
                       type = 'scatter', mode = 'lines',
                       line = list(color = '#adadad')) %>%
              layout(paper_bgcolor=to_color(PingAn_return), plot_bgcolor=to_color(PingAn_return),
                     xaxis = list(title = ""),
                     yaxis = list(title = "", range=c(min(PingAn$close),max(PingAn$close)*1.5 - 0.5*min(PingAn$close))),
                     margin =list(autoexpand = TRUE, r=10,l=10))%>%
              layout(annotations = PingAn_name)%>%
              layout(annotations = PingAn_price)%>%
              layout(annotations = PingAn_change)%>%
              layout(annotations = PingAn_HL)
PingAn_plot






