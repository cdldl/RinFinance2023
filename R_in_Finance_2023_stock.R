# DOWNLOAD / LOAD LIBRARIES
chooseCRANmirror(ind=1)
list.of.packages <- c("rmarkdown","data.table","imputeFin","fasttime","roll","doMC",
                      "FactorAnalytics", "knitr","lightgbm","PerformanceAnalytics",
                      'Metrics',"RQuantLib","h2o","glmnet","ranger", "PerformanceAnalytics") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, function(x) require(x, character.only = TRUE))

# LOAD FUNCTIONS
source(file = '~/RinFinance/R_in_Finance_2023_functions.R')
riskfree_file = '/home/cyril/TRADING/EOD/data/INDX/US1M.INDX.csv'
# CONSTANTS
target = 'ret_exc_lead1m'
train_cutoff_date = "2015-01-01"
val_cutoff_date = "2017-01-01"
cores=detectCores()-2


# LOAD DATA AND FORMAT IT
# DATA FROM: https://www.dropbox.com/sh/61j1v0sieq9z210/AACdJ68fs5_eT_eJMunwMBWia?dl=0
data = fread('/home/cyril/code/usa_filtered.csv')
data[,asset:=id]
data[,date:=paste0(substr(as.character(date),1,4),'-',
                   substr(as.character(date),5,6),'-',
                   substr(as.character(date),7,8))]
data[,date:=fastPOSIXct(date, 'UTC')]
data = data[order(date)]

# SHIFT EXPOSURES AND RESET TARGET
data[,paste0(target):=ret_exc]
cols = names(data)[which(! names(data)%in% c(target,'date'))]
data = data[,(cols):=shift(.SD,1,type='lag'),by=asset,.SDcols=cols]
data = data[complete.cases(ret_exc)]

# # GET EXPOSURES
exposures = names(which(sapply(data,is.double)==TRUE))
missings = data[,lapply(.SD, function(x) length(which(!is.na(x)))),by=asset,.SDcols=exposures]
exposures = names(which(sapply(missings,function(x) any(x < 5))==FALSE))
exposures =  names(data[, exposures,with=F])[which(sapply(data[, exposures,with=F],is.double) == TRUE)]
exposures = exposures[which(!exposures %in%  c("date","Date",target))]

# IMPUTE DATA 
data = data[order(date)]
data = imputing_values(copy(data), train_cutoff_date)
for(exposure in c(exposures,target)) set(data,which(is.na(data[[exposure]])),exposure
                            ,tryCatch(mean(data[date < train_cutoff_date, exposure,with=F],na.rm=T)
                            ,error=function(e) 0, warning=function(w) 0))

# STANDARDIZE EXPOSURES AND TARGET
data = data[order(date)]
data = standardize(copy(data), exposures, train_cutoff_date)
data = data[order(date)]
data = standardizeReturns(copy(data), train_cutoff_date)

# TRAINING MODELS
test = rolling_ml(data, val_cutoff_date)
fwrite(data,'/home/cyril/RinFinance/data2.csv')
fwrite(test,'/home/cyril/RinFinance/test_wpreds.csv')


# CALCULATE PERFORMANCE RATIOS

# GET RAW PRED METRICS
test = fread('/home/cyril/RinFinance/stock_wpreds.csv')
metrics = test[,get_metrics(.SD),by=date]
print(sapply(metrics[,2:4,with=F],quantile))

# GET PORTFOLIO RETURNS
test[,weights:=sqrt(abs(me))/sum(sqrt(abs(me))),by=date]
test[,weights2:=fifelse(abs(preds) > quantile(abs(preds),0.95),
                       sqrt(abs(me)),0),by=date]
test[,weights2:=weights2 /sum(weights2),by=date]
returns = test[,list(porf_returns=sum(sign(preds) * ret_exc_lead1m * weights2),
           benchmark=sum(ret_exc_lead1m * weights)),by=date]

# GET RISK-FREE RATE
rf = fread(riskfree_file)
rf[,date:=Date]
rf[,rf:=Adjusted_close/100]
rf[,year_month:=format(date,'%Y-%m')]
rf = rf[,.SD[.N],by=year_month]
returns[,year_month:=format(date,'%Y-%m')]
returns = merge(returns,rf[,c('year_month','rf')],by='year_month')

# COMPOUND RATIOS
xt = xts(returns[,3:ncol(returns),with=F],order.by=returns$date)
ratios <- calculate_ratios(xt$porf_returns, xt$benchmark, (1+mean(xt$rf))^(1/12) -1)
print(ratios)