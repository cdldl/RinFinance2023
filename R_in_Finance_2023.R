options(warn=-1)

# DOWNLOAD / LOAD LIBRARIES
chooseCRANmirror(ind=1)
list.of.packages <- c("rmarkdown","data.table","imputeFin","fasttime","roll","doMC",
                      "FactorAnalytics", "knitr","lightgbm","PerformanceAnalytics",
                      'Metrics', "RQuantLib") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(suppressMessages(lapply(list.of.packages, function(x) require(x, character.only = TRUE,quietly=TRUE))))

# LOAD FUNCTIONS
source(file = 'R_in_Finance_2023_functions.R')

# CONSTANTS
train_cutoff_date = "2015-01-01"
val_cutoff_date = "2017-01-01"
cores=detectCores()-2
target = 'ret_exc_lead1m'

# LOAD DATA AND FORMAT IT
# DATA FROM: https://www.dropbox.com/sh/61j1v0sieq9z210/AACdJ68fs5_eT_eJMunwMBWia?dl=0
data = fread('/home/cyril/code/usa_filtered.csv')
data[,asset:=id]
data[,date:=paste0(substr(as.character(date),1,4),'-',
                   substr(as.character(date),5,6),'-',
                   substr(as.character(date),7,8))]
data[,date:=fastPOSIXct(date, 'UTC')]
data = data[order(date)]
data[,asset:=id]

# GET EXPOSURES
exposures = names(which(sapply(data,is.double)==TRUE))
missings = data[,lapply(.SD, function(x) length(which(!is.na(x)))),by=asset,.SDcols=exposures]
exposures = names(which(sapply(missings,function(x) any(x < 5))==FALSE))
exposures =  names(data[, exposures,with=F])[which(sapply(data[, exposures,with=F],is.double) == TRUE)]
exposures = exposures[which(!exposures %in%  c("date","Date",target))]

# IMPUTE DATA 
data = imputing_values(copy(data), train_cutoff_date)
for(exposure in c(exposures,target)) set(data,which(is.na(data[[exposure]])),exposure,
                                 mean(data[[exposure]],na.rm=T))

# STANDARDIZE EXPOSURES AND TARGET
data = standardize(copy(data), exposures, train_cutoff_date)
data = standardizeReturns(copy(data), train_cutoff_date)

# SPLIT INTO TRAINING / VAL / TESTING SET
train = data[(date < train_cutoff_date)]  
val = data[(date >=train_cutoff_date) & (date < val_cutoff_date)]
test = data[date >= val_cutoff_date]

# TRAINING MODELS
models = list()
models[[1]] = lightgbm_model(train, val, exposures)
models[[2]] = rf_model(train, exposures)

# MAKING PREDICTIONS AND ENSEMBLE
val_and_test = making_preds(models, rbind(val,test), exposures)
preds_name = c('preds1_norm',"preds2_norm")
val_and_test = making_ensemble(val_and_test, preds_name)
val = val_and_test[date < val_cutoff_date]
test = val_and_test[date >= val_cutoff_date]

# UPLOAD RAW OPTION 
# CREATE VOL SURFACE
# MAKE PREDICTIONS
# FIT THE SURFACE BACK TO OPTIONS (+FUNCTION)
# INFER DIVIDEND YIELD
# OPTION PRICING
# BACKTEST (+FUNCTION)

option_data[,divRate:=optim(c(0.01), 
                            get_dividend_yield,data_option=data_option,
                            end_date=end_dates[i],
                            lower=c(0))$par
            ,by=asset]
