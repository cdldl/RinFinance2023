# DOWNLOAD / LOAD LIBRARIES
chooseCRANmirror(ind=1)
list.of.packages <- c("data.table","fasttime","anytime","RQuantLib","PerformanceAnalytics","doMC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, function(x) require(x, character.only = TRUE))

# CONSTANTS
cores = detectCores()-2
delta_t = 1
multiplier = 100
n_days_in_a_month=21
option_files_path ='/home/cyril/TRADING/ORATS/data/'
option_path_output = "/home/cyril/TRADING/ORATS/RinFinance/"
dir.create(option_path_output)
source(file = "/home/cyril/RinFinance/R_in_Finance_2023_functions.R")

# LOADING BACKEND 
if(Sys.info()['sysname'] == "Windows" ) {
  registerdoParallel(cores=cores)
} else {
  registerDoMC(cores=cores)
}

# LOAD STOCK AND VOL DATA
vol = fread('~/code/tft/preds_vol_surface_real_otm_test.csv')
stock = fread('/home/cyril/RinFinance/stock_wpreds.csv')

# FORMAT EXPECTED VOLS 
vol[,date:=as.IDate(forecast_time + 8*60*60)]
vol = vol[order(date)]
vol[,name:=identifier]
vol[,preds_vol:=shift(get(paste0('t+0')),1,type='lead'),by=name]
vol[,expected_volatility:=exp(preds_vol)]
vol[,c('asset','maturity','delta'):=tstrsplit(identifier, '__',type.convert=T)]
vol[,type:=fifelse(delta > 0,'call','put')]
vol[,delta:=abs(delta/100)]
vol[,maturity:=maturity/365]
vol_calls = vol[type == 'call']
vol_puts = vol[type == 'put']

# LOAD OPTION DATA
option_files = list.files(option_files_path)
test= substr(do.call(rbind,strsplit(option_files,'_|.csv'))[,4],1,4)
option_files = option_files[test >= 2017 & test < 2020]

option_files_lag = option_files[1:(length(option_files)-1)]
option_files = option_files[2:length(option_files)]

grid.param = cbind(option_files_lag,option_files) # [1:5,1:2] 

fe <- foreach(param = iter(grid.param, by = "row"),
              .verbose = TRUE, .errorhandling = "pass",
              .multicombine = TRUE, .maxcombine = max(2, nrow(grid.param)))
fe$args <- fe$args[1]
fe$argnames <- fe$argnames[1]
results <- fe %dopar% {
  # GET PARAMETERS
  option_files_lag = as.character(param[,1])
  option_file = as.character(param[,2])
  
  # option_files_lag = grid.param[1,1]
  # option_file = grid.param[1,2]

  # LOAD OPTIONS
  old_options = fread(paste0(option_files_path,option_files_lag))
  options = fread(paste0(option_files_path,option_file))
  tmp_date = options$trade_date[1]
  options = rbind(old_options, options)
  
  # GET LAG PRICES
  options[,date:=as.IDate(anytime(trade_date[1])),by=trade_date]
  options = options[order(date)]
  options[,asset:=ticker]
  options[,lag_call_ask:=shift(cAskPx,1,type='lag',fill=NA),by=c('asset','strike','expirDate')]
  options[,lag_put_ask:=shift(pAskPx,1,type='lag',fill=NA),by=c('asset','strike','expirDate')]
  
  # GET OLD DELTA
  options[,delta_original_call:=shift(delta,1,type='lag'),by=c('asset','strike','expirDate')]
  options[,delta_original_put:=shift(1-delta,1,type='lag'),by=c('asset','strike','expirDate')]
  
  # SUBSET TO TODAY'S OPTIONS
  options = options[tmp_date == trade_date]
  
  # CONCATENATE OPTIONS WITH STOCK PREDICTIONS
  options[,year_month:=format(date[1],'%Y-%m'),by=date]
  stock[,year_month:=format(date,'%Y-%m')]
  stock[,asset:=symbol]
  options = merge(options, stock[,c('me','year_month','asset','preds'),with=F],
                  by=c('year_month','asset'))
  
  # CONCATENATE OPTIONS WITH VOL PREDICTIONS
  deltas = sort(unique(vol_calls$delta))
  maturities = sort(unique(vol_calls$maturity))
  
  options[,delta:=deltas[which.min(abs(delta_original_call - deltas))],by=seq.int(1,nrow(options))]
  options[,maturity:=maturities[which.min(abs(yte - maturities))],by=seq.int(1,nrow(options))]
  options = merge(options, vol_calls[,c('date','asset','maturity','delta','expected_volatility'),with=F],
                  by=c('date','asset','delta','maturity'))
  
  options[,delta:=deltas[which.min(abs(delta_original_put - deltas))],by=seq.int(1,nrow(options))]
  options[,maturity:=maturities[which.min(abs(yte - maturities))],by=seq.int(1,nrow(options))]
  options = merge(options, vol_puts[,c('date','asset','maturity','delta','expected_volatility'),with=F],
                  by=c('date','asset','delta','maturity'))
  
  # COMPOUND OPTION INPUTS
  options[,expected_maturity:=round((yte*365) - delta_t)/365]
  options[,expected_stock_price:=(shift(stkPx,1,type='lag') * (1+preds/n_days_in_a_month))]
    
  # COMPOUND EXPECTED PRICE OPTIONS
  options = option_pricing(options, delta_t)
  
  # COMPOUND RETURNS
  options[,expected_returns_call:=(option_price_pred_call1 - lag_call_ask)/lag_call_ask]
  options[,expected_returns_put:=(option_price_pred_put1 - lag_put_ask)/lag_put_ask]
  options[,real_returns_call:=(cBidPx - lag_call_ask)/lag_call_ask]
  options[,real_returns_put:=(pBidPx - lag_put_ask)/lag_put_ask]
  
  # SET CALLS AND PUTS WITH THRESHOLD GIVEN
  calls = options[expected_returns_call >0.]
  calls[,delta:=delta_original_call]
  calls[,real_returns:=fifelse(!is.finite(real_returns_call),0,real_returns_call)]
  
  puts = options[expected_returns_put > 0.]
  puts[,delta:=delta_original_put]
  puts[,real_returns:=fifelse(!is.finite(real_returns_put),0,real_returns_put)]

  # SET VARS FOR ANALYSIS
  calls[,margin:=cAskPx*multiplier]
  calls[,expected_returns:=expected_returns_call]
  puts[,margin:=pAskPx*multiplier]
  puts[,expected_returns:=expected_returns_put]
  trading = rbind(calls,puts)
  
  fwrite(trading,paste0(option_path_output,trading$date[1],'.csv'))  
  return(NULL)
}







