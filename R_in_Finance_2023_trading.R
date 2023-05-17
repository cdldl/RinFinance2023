# DOWNLOAD / LOAD LIBRARIES
chooseCRANmirror(ind=1)
list.of.packages <- c("data.table","fasttime","anytime","RQuantLib","PerformanceAnalytics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, function(x) require(x, character.only = TRUE))

# CONSTANTS
delta_t = 6
initial_capital = 1e4
multiplier = 100
riskfree = (1+0.0165)^(1/12) -1
option_path_output = "/home/cyril/TRADING/ORATS/RinFinance6/"
benchmark_path='/home/cyril/TRADING/EOD/data/US/XLK.US.csv'
source(file = "/home/cyril/RinFinance/R_in_Finance_2023_functions.R")

# LOAD OPTIONS 
files = list.files(option_path_output, full.names = T)
tmp_files = lapply(files,fread)
to_remove = sapply(tmp_files,function(x) nrow(x) == 0)
whi = which(to_remove == TRUE)
if(length(whi) != 0) {
  tmp_files = tmp_files[-whi]
} 
options = rbindlist(tmp_files)
options[,midpoint:=fifelse(type=='call',(cAskPx + cBidPx)/2,(pAskPx + pBidPx)/2)]

# LOAD STOCK DATA
data = fread('/home/cyril/code/usa_filtered.csv')
data[,asset:=symbol]
data[,date:=paste0(substr(as.character(date),1,4),'-',
                   substr(as.character(date),5,6),'-',
                   substr(as.character(date),7,8))]
data[,date:=fastPOSIXct(date, 'UTC')]
data = data[order(date)]
data[,year_month:=format(date,'%Y-%m')]
options[,year_month:=format(date,'%Y-%m')]
options2 = merge(options,data[,c('year_month','asset','me'),with=F],by=c('year_month','asset'))
options = copy(options2)

# METRICS TRADING
options[,expected_returns:=fifelse(!is.finite(expected_returns),0,expected_returns)]
options[,real_returns:=fifelse(!is.finite(real_returns),0,real_returns)]

val = options[year(date) < 2018]
cut_exp_ret = cut(val$expected_returns,unique(quantile(val$expected_returns, seq(0,1,0.1))))
cut_exp_delta = cut(val$delta,seq(0,1,0.2))
cut_exp_mat = cut(val$maturity,unique(quantile(val$maturity, seq(0,1,0.1))))
cut_exp_margin = cut(val$margin,unique(quantile(val$margin,seq(0,1,0.2))))
cut_exp_me = cut(val$me.y,unique(quantile(val$me.y,seq(0,1,0.2))))
cut_exp_midpoint = cut(val$midpoint,unique(quantile(val$midpoint,seq(0,1,0.2))))
cut_exp_spread = cut(val$spread,unique(quantile(val$spread,seq(0,1,0.2))))
print(tapply(val$real_returns, list(cut_exp_ret,cut_exp_delta,val$type),sum,na.rm=T)) 

# METRICS ABOVE MADE ME SELECT THESE THRESHOLDS
test = options[year(date) >= 2018  & expected_returns>0.35 & delta<0.6 & 
                 maturity < 0.16 & spread > 0.05 & midpoint > 0.5]

cut_exp_ret = cut(test$expected_returns,unique(quantile(test$expected_returns, seq(0,1,0.1))))
cut_exp_delta = cut(test$delta,seq(0,1,0.2))
print(tapply(test$real_returns, list(cut_exp_ret,cut_exp_delta,test$type),sum,na.rm=T)) 

# INVEST DAILY A MAXIMUM OF YOUR INITIAL CAPITAL (FIXED CAPITAL OVER TIME)
trading = test[order(-expected_returns),.SD,by=date]
trading[,cum_margin:=cumsum(margin),by=date]
trading[,initial_capital:=initial_capital]
whi = which(trading$initial_capital <= trading$initial_capital)
trading2 =trading[whi]

# BASIC PERFORMANCE WITHOUT AGGREGATION AND DAILY AGGREGATION
trading2[,ret:=real_returns/multiplier]
trading2 = trading2[order(date)]
trading2[,pnl:=fifelse(type=='call', cBidPx - lag_call_ask,pBidPx - lag_put_ask)]
# trading2 = benchmark[date>min(trading$date)& date < max(trading$date)]
# trading2[,ret:=benchmark]
data.table(
  Starting.Period = first(trading2$date),
  Ending.Period = last(trading2$date),
  pnl= sum(trading2$ret),
  n.trades=nrow(trading2),
  pip.trade = mean(trading2$ret,na.rm=T)* 10000,
  win.ratio.strat= (length(which(trading2$ret>0))/length(trading2$ret))*100,
  sharpe= (mean(trading2$ret,na.rm=T)* sqrt(252/delta_t)) / sd(trading2$ret,na.rm=T)
)

trading3 = trading2[,list(ret=mean(real_returns,na.rm=T)/multiplier,
                          pnl=sum(fifelse(type=='call', cBidPx - lag_call_ask,
                                          pBidPx - lag_put_ask))),by=date]
trading3 = copy(trading2)
data.table(
  pnl= sum(trading3$ret),
  pip.trade = mean(trading3$ret,na.rm=T)* 10000,
  win.ratio.strat= (length(which(trading3$ret>0))/length(trading3$ret))*100,
  sharpe= (mean(trading3$ret,na.rm=T)* sqrt(252/delta_t)) / sd(trading3$ret,na.rm=T),
  MDD=maxDrawdown(xts(trading3$ret,order.by=as.Date(trading3$date)))
)

# PERFORMANCE AGAINST BENCHMARK
benchmark =fread(benchmark_path)
benchmark[,benchmark:=(Adjusted_close - shift(Adjusted_close,1,type='lag'))/
            shift(Adjusted_close,1,type='lag')]
benchmark$benchmark[1] = 0
benchmark[,date:=Date]
combined = merge(trading3,benchmark[,c('date','benchmark'),with=F],by='date')
xt = xts(cbind(ret=combined[,2],benchmark=combined[,3]),order.by=as.Date(combined$date))
ratios = calculate_ratios(xt$ret,xt$benchmark,riskfree)
print(ratios)