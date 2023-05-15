# FIXME:
# PIPELINE TO MAKE IT FASTER (LOAD OPTION FILES BY DELTA_T AND DO IT IN PARALLEL)
# SET PATHS WITH CONSTANTS

# DOWNLOAD / LOAD LIBRARIES
chooseCRANmirror(ind=1)
list.of.packages <- c("data.table","fasttime","anytime","RQuantLib","PerformanceAnalytics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, function(x) require(x, character.only = TRUE))

# CONSTANTS
delta_t = 1
initial_capital = 1e6
multiplier = 100
n_days_in_a_month=21

# LOAD STOCK AND VOL DATA
vol = fread('~/code/tft/preds_vol_surface_real_otm_test.csv')
stock = fread('/home/cyril/RinFinance/stock_wpreds.csv')

# FORMAT DATA 
vol[,preds_vol:=shift(get('t+0'),1,type='lag',fill=NA),by=identifier]
vol[,expected_volatility:=exp(preds_vol)]
stock[,year_month:=format(date,'%Y-%m')]
stock[,asset:=symbol]

# LOAD OPTION DATA
option_files = list.files('/home/cyril/TRADING/ORATS/data/')
test= substr(do.call(rbind,strsplit(option_files,'_|.csv'))[,4],1,4)
option_files = option_files[test >= 2017 & test < 2020]
options = NULL
#FIXME: MUST BE DONE ON A ROLLING BASIS (CAN REACH 750GB OF RAM)
for(file in option_files[1:5]) {
  tmp_data = fread(paste0('/home/cyril/TRADING/ORATS/data/',file))
  options = rbind(options, tmp_data)
}

# CONCATENATE OPTIONS WITH STOCK PREDICTIONS
options[,date:=as.IDate(anytime(trade_date[1])),by=trade_date]
options = merge(options, stock[,c('me','year_month','asset','preds'),with=F],
                by=c('year_month','asset'))


# CONCATENATE OPTIONS WITH VOL PREDICTIONS
vol[,date:=as.IDate(forecast_time + 8*60*60)]
vol[,c('asset','maturity','delta'):=tstrsplit(identifier, '__',type.convert=T)]
vol[,type:=fifelse(delta > 0,'call','put')]
vol[,delta:=abs(delta/100)]
vol[,maturity:=maturity/365]
vol_calls = vol[type == 'call']
vol_puts = vol[type == 'put']

deltas = sort(unique(vol_calls$delta))
maturities = sort(unique(vol_calls$maturity))

options[,asset:=ticker]
options[,year_month:=format(date[1],'%Y-%m'),by=date]
options[,delta_original:=copy(delta)]
options[,delta:=deltas[which.min(abs(delta - deltas))],by=seq.int(1,nrow(options))]
options[,maturity:=maturities[which.min(abs(yte - maturities))],by=seq.int(1,nrow(options))]
options = merge(options, vol_calls[,c('date','asset','maturity','delta','expected_volatility'),with=F],
                by=c('date','asset','delta','maturity'))

options[,delta:=abs(1-delta_original)]
options[,delta:=deltas[which.min(abs(delta - deltas))],by=seq.int(1,nrow(options))]
options[,maturity:=maturities[which.min(abs(yte - maturities))],by=seq.int(1,nrow(options))]
options = merge(options, vol_puts[,c('date','asset','maturity','delta','expected_volatility'),with=F],
                by=c('date','asset','delta','maturity'))

# COMPOUND OPTION INPUTS
options[,expected_maturity:=round((yte*365) - delta_t)/365]
options[,expected_stock_price:=(stkPx * (1+preds/n_days_in_a_month))]

# COMPOUND EXPECTED PRICE OPTIONS
options[,paste0('option_price_pred_put',delta_t):= {
  if (inherits(try(ans<-AmericanOption('put' 
                                       , expected_stock_price 
                                       , strike, divRate, iRate
                                       , expected_maturity
                                       , expected_volatility.y)$value
                   ,silent=TRUE),"try-error"))
    0
  else
    ans},
  by = seq.int(1,nrow(options))]

options[,paste0('option_price_pred_call',delta_t):= {
  if (inherits(try(ans<-AmericanOption('call' 
                                       , expected_stock_price 
                                       , strike, divRate, iRate
                                       , expected_maturity
                                       , expected_volatility.x)$value
                   ,silent=TRUE),"try-error"))
    0
  else
    ans},
  by = seq.int(1,nrow(options))]

# GET LAG PRICES
options = options[order(date)]
options[,lag_call_ask:=shift(cAskPx,1,type='lag',fill=NA),by=c('asset','strike','expirDate')]
options[,lag_put_ask:=shift(pAskPx,1,type='lag',fill=NA),by=c('asset','strike','expirDate')]

# COMPOUND RETURNS
options[,expected_returns_call:=(option_price_pred_call1 - lag_call_ask)/lag_call_ask]
options[,expected_returns_put:=(option_price_pred_put1 - lag_put_ask)/lag_put_ask]
options[,real_returns_call:=(cBidPx - lag_call_ask)/lag_call_ask]
options[,real_returns_put:=(pBidPx - lag_put_ask)/lag_put_ask]

# SET CALLS AND PUTS WITH THRESHOLD GIVEN
calls = options[expected_returns_call >0.1 & delta_original > 0.3 &
                  delta_original < 0.55 & maturity < 0.5] 
calls[,real_returns_call:=fifelse(!is.finite(real_returns_call),0,real_returns_call)]
calls[,delta:=delta_original]

puts = options[expected_returns_put > 0.1 & abs(1-delta_original) > 0.3 & 
                 abs(1-delta_original) < 0.55  & maturity < 0.5] 
puts[,real_returns_put:=fifelse(!is.finite(real_returns_put),0,real_returns_put)]
puts[,delta:=abs(1-delta_original)]

# METRICS CALLS
cut_exp_ret = cut(calls$expected_returns_call,
                  unique(quantile(calls$expected_returns_call, seq(0,1,0.1))))
cut_exp_delta = cut(calls$delta,unique(quantile(calls$delta, seq(0,1,0.1))))
cut_exp_mat = cut(calls$maturity,unique(quantile(calls$maturity, seq(0,1,0.1))))
print(tapply(calls$real_returns_call, list(cut_exp_ret,cut_exp_delta,cut_exp_mat),mean,na.rm=T)) 

# METRICS PUTS
cut_exp_ret = cut(puts$expected_returns_put,
                  unique(quantile(puts$expected_returns_put, seq(0,1,0.1))))
cut_exp_delta = cut(puts$delta,unique(quantile(puts$delta, seq(0,1,0.2))))
cut_exp_mat = cut(puts$maturity,unique(quantile(puts$maturity, seq(0,1,0.1))))
print(tapply(puts$real_returns_put, list(cut_exp_ret,cut_exp_delta,cut_exp_mat),mean,na.rm=T))

# TRADING
calls[,margin:=cAskPx*multiplier]
calls[,expected_returns:=expected_returns_call]
calls[,real_returns:=real_returns_call]
puts[,margin:=pAskPx*multiplier]
puts[,expected_returns:=expected_returns_put]
puts[,real_returns:=real_returns_put]

trading = rbind(calls,puts)
trading = trading[order(expected_returns)]
trading[,cum_margin:=cumsum(margin),by=date]
fwrite(trading,'/home/cyril/RinFinance/trading.csv')

# INVEST DAILY A MAXIMUM OF YOUR INITIAL CAPITAL (FIXED INITIAL CAPITAL OVER TIME)
trading2 = trading[,.SD[order(expected_returns)][cum_margin <= initial_capital],by=date]

# PERFORMANCE AGAINST BENCHMARK
trading3 = trading2[,list(ret=mean(real_returns)/100),by=date]
benchmark =fread('/home/cyril/TRADING/EOD/data/US/XLK.US.csv')
benchmark[,benchmark:=(Adjusted_close - shift(Adjusted_close,1,type='lag'))/
            shift(Adjusted_close,1,type='lag')]
benchmark$benchmark[1] = 0
benchmark[,date:=Date]
combined = merge(trading3,benchmark[,c('date','benchmark'),with=F],by='date')
fwrite(combined,'/home/cyril/RinFinance/perf_vs_benchmark.csv')

# xt = xts(combined,order.by=as.Date(combined$date))
# ratios = calculate_ratios(xt[,'ret'],xt[,'benchmark'],rf)
# print(ratios)