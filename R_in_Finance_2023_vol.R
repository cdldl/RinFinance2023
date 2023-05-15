# DOWNLOAD / LOAD LIBRARIES
chooseCRANmirror(ind=1)
list.of.packages <- c("imputeFin","data.table","anytime","doMC","ranger")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# CONSTANTS
source(file = "/home/cyril/RinFinance/R_in_Finance_2023_functions.R")
cores = detectCores()-2
files_strikes_path  = '/home/cyril/TRADING/ORATS/data/'
path_vol_surface_by_date = '/home/cyril/TRADING/VOL_SURFACE/data/'
path_vol_surf_by_ticker = '/home/cyril/TRADING/VOL_SURFACE/vol_surf_data/'
symbol_universe = fread('/home/cyril/RinFinance/all_linking.csv')$symbol
train_cutoff_date = "2015-01-01"
val_cutoff_date = "2017-01-01"

exposures = c('vol')

# LOADING BACKEND 
if(Sys.info()['sysname'] == "Windows" ) {
    registerdoParallel(cores=cores)
  } else {
    registerDoMC(cores=cores)
}

# CREATE VOL SURFACE BY DATE
files_strikes = list.files(files_strikes_path)
create_vol_surface(files_strikes, path_vol_surface_by_date)


# CONCATENATE VOL SURFACE
vol_surfaces = list.dirs(path_vol_surface_by_date)
concatenate_vol_surfaces(vol_surfaces, path_vol_surf_by_ticker)

# LOAD DATA
files = list.files(path_vol_surf_by_ticker)
files = files[files %in% paste0(symbol_universe,'.csv')]
data = NULL
for(file in files) {
  tmp_data = fread(paste0(path_vol_surf_by_ticker,file), col.names=c('maturity','delta','type','vol','date'))
  tmp_data[,asset:=gsub('.csv','',file)]
  data = rbind(data,tmp_data)
}

# IMPUTING MISSING VALUES
set(data,which(data$vol==0. |!is.finite(data$vol)),'vol',NA)
data = data[order(date)]
data = imputing_values_vol(copy(data), 'vol',train_cutoff_date)
data = data[order(date)]

# REPLACE LEADING NAs
data[,vol:=replace_leading_na(vol),by=c('maturity','delta','type','asset')]

# CREATE VOL FEATURES
data = vol_features(data)
data = data[complete.cases(data)]

# MAKE PREDICTIONS
target = 'log_vol'
exposures = names(which(sapply(data,is.numeric)==TRUE))
exposures = exposures[which(!exposures %in% c(target,'vol'))]
data = data[order(date)]
train = data[date < val_cutoff_date]
test = data[date >= val_cutoff_date]
model = rf_model(train, exposures, target)
test[,preds_vol:=exp(predict(model,test[,exposures,with=F])$predictions)]
fwrite(test,'/home/cyril/RinFinance/vol_surface_wpreds.csv')

