
# IMPUTE MISSING VALUES WITH TRAINING AND TESTING SET
imputing_values = function(tmp_data, train_cutoff_date) {
  tmp_data[,(exposures):=lapply(.SD,function(x) tryCatch(jitter(x),
                                                         error=function(e) x, warning=function(w) x))
           ,by=asset,.SDcols=exposures]
  func = function(x, train_cutoff_date) {
    train = x[tmp_data$date < train_cutoff_date]
    val_and_test = x[tmp_data$date >= train_cutoff_date]
    impute_insample = impute_AR1_t(train, remove_outliers = TRUE, return_estimates=TRUE)
    impute_outsample = val_and_test
    idx_missing = which(!is.finite(val_and_test))
    if(!length(idx_missing)) return(c(impute_insample$y_imputed,impute_outsample))
    for(i in idx_missing) {
      if(idx_missing==1) impute_outsample[i] = impute$phi0 + impute$phi1 *         
          impute_insample$y_imputed[length(impute_insample)]
      impute_outsample[i] = impute$phi0 + impute$phi1 * impute_outsample[i-1]
    }
    c(impute_insample$y_imputed,impute_outsample)
  }
  tmp_data[,(exposures):=mclapply(.SD, function(x) 
    tryCatch(func(x, train_cutoff_date),
             error=function(e) x, warning=function(w) x)
    , mc.cores=cores
    , mc.silent = TRUE)
    ,by=asset,.SDcols=exposures]
  tmp_data
}

# STANDARDIZE EXPOSURES WITH TRAINING AND TESTING SET
standardize = function(tmp_data, exposures, train_cutoff_date) {
  zScore <- function(exposure, train_cutoff_date, lambda=0.9) {
    mu = mean(exposure[tmp_data$date < train_cutoff_date], na.rm=T)
    sigma = sd(exposure[tmp_data$date < train_cutoff_date], na.rm=T)
    ts <- (exposure - mu)^2
    var_past_2 <- sigma^2
    sigmaEWMA = vector(mode='numeric',length=length(exposure))
    for(i in 1:length(exposure)) {
      var_past_2 = (1- lambda) * ts[i] + lambda * var_past_2
      sigmaEWMA[i] = var_past_2
    }
    sigmaEWMA[which(sigmaEWMA==0)]<- 1
    as.vector((exposure -  mu) / sqrt(sigmaEWMA))
  }
  tmp_data[,(exposures):=mclapply(.SD,function(x) tryCatch(zScore(x, train_cutoff_date),
                                                           error=function(e) x, warning=function(w) x)
                                  , mc.cores=cores
                                  , mc.silent = TRUE),by=asset,.SDcols=exposures]
  tmp_data[order(date)]
}

# STANDARDIZE RETURNS WITH GARCH
standardizeReturns = function(tmp_data,train_cutoff_date) {
  computeGarch <- function(returns, train_cutoff_date, omega = 0.09, alpha = 0.1, beta = 0.81) {
    sdReturns = sd(returns[tmp_data$date < train_cutoff_date], na.rm = TRUE)
    ts = returns^2
    
    sigmaGarch = vector(mode='numeric',length=length(returns))
    sigmaGarch[1] = (1 - alpha - beta) * sdReturns^2 +  alpha * ts[1]
    for (i in 2:length(returns))
      sigmaGarch[i] = (1 - alpha - beta) * sdReturns^2 + 
      alpha * ts[i] +
      beta * sigmaGarch[i-1]
    
    sigmaGarch = sqrt(sigmaGarch)
    return(sigmaGarch)
  }
  tmp_data[,sigmaGarch:=computeGarch(ret_exc, train_cutoff_date), by=asset]
  
  tmp_data[,ret_exc_lead1m_norm:=ret_exc_lead1m/sigmaGarch]
}

# STANDARDIZE RETURNS WITH BEST NORMALIZE PACKAGE
standardizeReturns = function(tmp_data, train_cutoff_date, val_cutoff_date) {
  train = tmp_data[date < train_cutoff_date]
  val = tmp_data[(date >=train_cutoff_date) & (date < val_cutoff_date)]
  test = tmp_data[date > val_cutoff_date]
  
  BN_obj = train[,list(BN_obj=list(tryCatch(
    bestNormalize(ret_exc, allow_lambert_s = TRUE)
    ,error=function(e) NA, warning=function(w) NA))),by=asset]
  
  train = merge(train, BN_obj,by='asset')
  train[,ret_exc_lead1m_norm:=tryCatch(predict(BN_obj[[1]]
                                               ,newdata=ret_exc_lead1m)
                                       ,error=function(e) NA, warning=function(w) NA)
        ,by=asset]
  train[,ret_exc_lead1m_norm:=fifelse(!is.finite(ret_exc_lead1m_norm),0
                                      ,ret_exc_lead1m_norm)]
  val = merge(val, BN_obj,by='asset')
  val[,ret_exc_lead1m_norm:=tryCatch(predict(BN_obj[[1]]
                                             ,newdata=ret_exc_lead1m)
                                     ,error=function(e) NA, warning=function(w) NA)
      ,by=asset]
  val[,ret_exc_lead1m_norm:=fifelse(!is.finite(ret_exc_lead1m_norm),0
                                    ,ret_exc_lead1m_norm)]
  
  test = merge(test, BN_obj,by='asset')
  test[,ret_exc_lead1m_norm:=tryCatch(predict(BN_obj[[1]]
                                             ,newdata=ret_exc_lead1m)
                                     ,error=function(e) NA, warning=function(w) NA)
      ,by=asset]
  test[,ret_exc_lead1m_norm:=fifelse(!is.finite(ret_exc_lead1m_norm),0
                                    ,ret_exc_lead1m_norm)]
  # winsowrize_cutoff = 2.5
  # train[,ret_exc_lead1m_norm:=fifelse(
  #   abs(ret_exc_lead1m_norm) > winsowrize_cutoff,
  #   sign(ret_exc_lead1m_norm) *winsowrize_cutoff,
  #   ret_exc_lead1m_norm)]
  return(list(BN_obj, train, val, test))
}

# BINARIZE FEATURES TO AVOID OVERFITTING AND IMPROVE ML CONVERGENCE 
binarize_features = function(x) {
  asset_count_per_date = train[,list(count=length(asset)),by=date][order(date)]
  starting_date = asset_count_per_date[count >= 5]$date[1]
  train = train[date>=starting_date]
  
  func = function(x,probs_bucket=1/50) {
    buckets = data.table(x, f=tryCatch(
      cut(x,unique(quantile(x, seq(0,1,probs_bucket),na.rm=TRUE,include.lowest=TRUE)))
      ,error=function(e) x, warning=function(w) x))
    buckets[,mean_bucket:=mean(x),by=f]$mean_bucket
  }
  train[,(exposures):=mclapply(.SD, func, mc.cores=cores, mc.silent=TRUE), by=date, .SDcols=exposures]
  train
}

# TRAINING LIGHTGBM MODEL
lightgbm_model = function(train, val, exposures) {  
  dtrain <- lgb.Dataset(as.matrix(train[,exposures,with=F])
                        , label = train$ret_exc_lead1m_norm)
  dval <- lgb.Dataset.create.valid(dtrain,
                                   data=as.matrix(val[,exposures,with=F]),
                                   label = val$ret_exc_lead1m_norm) #FIXME
  valids <- list(train = dtrain, test = dval)
  param <- list(
    objective = "regression"
    , metric = "mse"
    , learning_rate = 0.1
    , num_iterations=10000
    , early_stopping_round = 30L
    , num_rounds = 5L
    , num_threads=cores
    , verbosity =0)
  model <- lgb.train(params=param, data=dtrain, valids=valids)
  model
}

rf_model = function(train, exposures) {
  model = ranger(train$ret_exc_lead1m_norm ~ .,data= train[,exposures,with=F],importance="impurity")
  model
}


making_preds = function(models, test, exposures) {
  tmp_data = copy(test)
  tmp_data[,preds1_norm:= predict(models[[1]],
                                    as.matrix(tmp_data[,exposures,with=F]))]
  tmp_data[,preds2_norm:= predict(model2,
                               data=tmp_data[,exposures,with=F])$predictions]
  tmp_data
}

making_ensemble =function(data, preds_name) {
  func_ensemble = function(target, predictions) {
    linear_coeffs = roll_lm(as.matrix(predictions),target,width=12,min_obs=1)$coefficients[1:(length(target)-1),]
    for(i in 1:ncol(linear_coeffs)) linear_coeffs[which(!is.finite(linear_coeffs[,i])),i] =0
    linear_preds = linear_coeffs[,1] + diag(linear_coeffs[,2:ncol(linear_coeffs)] %*% t(predictions[2:nrow(predictions),]))
    c(0,linear_preds)
  }
  data[,preds_ens:=func_ensemble(ret_exc_lead1m, .SD),by=asset, .SDcols=preds_name]
}


volSurface = function(data,maturity,delta,call_or_put) {
  h_1 = 0.05
  h_2 = 0.005
  h_3 = 0.001
  x = log(data$maturity / maturity)
  y = data$delta - delta
  z = ifelse(data$type == call_or_put,0,1)
  kernel = exp(-((x^2)/(2*h_1)+(y^2)/(2* h_2)+(z^2)/(2* h_3))) /
    sqrt(2*pi)
  if(call_or_put == 'call') bid = 'callBidIV' else bid= 'putBidIV'
  vol = sum(data$vega * data[,bid,with=F] * kernel) /
    sum(data$vega * kernel)
  return(vol)
}

get_dividend_yield = function(para, data, end_date) {
  data[,c('valueBS','deltaBS','gammaBS','vegaBS','thetaBS','rho','yo'):=
         EuropeanOption(type=cp_flag, underlying=close,strike=strike_price
                        , dividendYield=para[1], riskFreeRate=interestRate, maturity=mat_yearly
                        , volatility=impl_volatility)
       , by=seq.int(nrow(data))]
  
  op_error = sum( (data$valueBS - data$midpoint)^2,na.rm=T)
  delta_error = sum((data$deltaBS - data$delta)^2,na.rm=T)
  gamma_error = sum((data$gammaBS - data$gamma)^2,na.rm=T)
  vega_error = sum((data$vegaBS - data$vega)^2,na.rm=T)
  theta_error = sum((data$thetaBS - data$theta)^2,na.rm=T)
  
  return(sqrt(sum(op_error + delta_error + gamma_error+ vega_error + theta_error)))
}

option_pricing = function(options) {
  options[,paste0('option_price_pred',delta_t):= {
    if (inherits(try(ans<-AmericanOption('put' 
                                         , expected_stock_price
                                         , strike, divRate, iRate
                                         , expected_maturity
                                         , expected_volatlity
                                         , engine= 'CrankNicolson')$value
                     ,silent=TRUE),"try-error"))
      0
    else
      ans},
    by = seq.int(1,nrow(options))]
}