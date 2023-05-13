
# IMPUTE MISSING VALUES WITH TRAINING AND TESTING SET
imputing_values = function(tmp_data, train_cutoff_date) {
  tmp_data[,(exposures):=lapply(.SD,function(x) tryCatch(jitter(x),
                                                         error=function(e) x, warning=function(w) x))
           ,by=asset,.SDcols=exposures]
  func = function(x, train_cutoff_date) {
    train = x[tmp_data$date < train_cutoff_date]
    val_and_test = x[tmp_data$date >= train_cutoff_date]
    impute_insample = impute_AR1_t(train, remove_outliers = TRUE, return_estimates=TRUE)
    impute_outsample = copy(val_and_test)
    idx_missing = which(!is.finite(val_and_test))
    if(!length(idx_missing)) return(c(impute_insample$y_imputed,impute_outsample))
    for(i in idx_missing) {
      if(idx_missing==1) { 
        impute_outsample[i] = impute$phi0 + impute$phi1 * impute_insample$y_imputed[length(impute_insample)]
      } else {
        impute_outsample[i] = impute$phi0 + impute$phi1 * impute_outsample[i-1]  
      }
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
standardizeReturnsBestNormalize = function(tmp_data, train_cutoff_date, val_cutoff_date) {
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

rf_model = function(train, exposures, target, weights) {
  model = ranger(train[,target,with=F] ~ .,data= train[,exposures,with=F],
                 importance="impurity",case.weights=weights)
  model
}

# GET MARKET CAP WEIGHTS
get_market_cap_weights = function(train) {
  me_weights = train[date == last(date),c('me','asset'),with=F]
  me_weights[,weights:=sqrt(abs(me))/sum(sqrt(abs(me)))]
  train = merge(train,me_weights[,c('asset','weights'),with=F],by='asset')
  train[,weights:=weights/length(ret_exc),by=asset]
  train
}

# ROLLING ML
rolling_ml = function(data, val_cutoff_date) {
  dates = sort(unique(data[date >= val_cutoff_date]$date))
  for(i in 1:length(dates)) {
    # i=1
    tmp_date = dates[i]
    tmp_train = data[date < tmp_date]  
    tmp_train = get_market_cap_weights(tmp_train)
    exposures = feature_selection(tmp_train)
    # ADD AUTOML AND LIGHTGBM
    model = rf_model(tmp_train, exposures, target, tmp_train$weights)
    set(data,which(data$date == tmp_date),'preds',
        predict(model,data[date == tmp_date,exposures,with=F])$predictions)
  }
  data[date >= val_cutoff_date]
}


making_preds = function(models, test, exposures) {
  tmp_data = copy(test)
  tmp_data[,preds1_norm:= predict(models[[1]],
                                    as.matrix(tmp_data[,exposures,with=F]))]
  tmp_data[,preds2_norm:= predict(model2,
                               data=tmp_data[,exposures,with=F])$predictions]
  tmp_data
}

making_ensemble_by_ticker =function(data, preds_name) {
  func_ensemble = function(target, predictions) {
    linear_coeffs = roll_lm(as.matrix(predictions),target,width=12,min_obs=1)$coefficients[1:(length(target)-1),]
    for(i in 1:ncol(linear_coeffs)) linear_coeffs[which(!is.finite(linear_coeffs[,i])),i] =0
    linear_preds = linear_coeffs[,1] + diag(linear_coeffs[,2:ncol(linear_coeffs)] %*% t(predictions[2:nrow(predictions),]))
    c(0,linear_preds)
  }
  data= data[order(date)]
  data[,preds_ens:=func_ensemble(ret_exc_lead1m, .SD),by=asset, .SDcols=preds_name]
}

making_ensemble_by_date =function(data, preds_name) {
  dates = sort(unique(data$date))
  data = data[order(date)]
  for(i in 3:length(dates)) {
    tmp_date = dates[i]
    tmp_data = data[date < dates[i-1]]
    model = lm(ret_exc_lead1m_norm~ .,data=tmp_data[,c(preds_name,'ret_exc_lead1m_norm'),with=F],
               weights=tmp_data$mve)
    print(summary(model)$r.squared)
    set(data,which(data$date == tmp_date),"preds_ens",predict(model, 
                                                              data[which(data$date == tmp_date)
                                                                       ,preds_name,with=F]))
  }
  data#[,list(models=lm(ret_exc_lead1m ~., data=.SD),by=date, .SDcols=preds_name]
}

autoML = function(train,val,test, exposures, target) {
  h2o.init()
  train.hex  <- as.h2o(train)
  val.hex = as.h2o(val)
  test.hex = as.h2o(test)
  
  automl_h2o_models <- h2o.automl(
    x = exposures, 
    y = target,
    training_frame    = train.hex,
    leaderboard_frame = val.hex
  )
  pred_conversion <- h2o.predict(object = automl_h2o_models, newdata = val.hex)
  val[,preds:=as.vector(pred_conversion)]
  pred_conversion <- h2o.predict(object = automl_h2o_models, newdata = test.hex)
  test[,preds:=as.vector(pred_conversion)]
  return(list(val, test))
  #val_and_test = autoML(train,val,test, exposures, target)
  # val = val_and_test[[1]]
  # test  = val_and_test[[2]]
  # fwrite(val,'/home/cyril/RinFinance/val_with_preds.csv')
  # fwrite(test,'/home/cyril/RinFinance/test_with_preds.csv')
}

feature_selection = function(data) {
  # OPTIMIZE LAMBDA
  lambdas <- 10^seq(2, -4, by = -.1)
  lasso_reg <- cv.glmnet(as.matrix(data[,exposures,with=F]), 
                         data$ret_exc_lead1m, alpha = 1, lambda = lambdas, nfolds = 5,
                         parallel = TRUE,weights=data$weights)
  lambda_best <- lasso_reg$lambda.min 
  
  # FEATURE SELECTION
  lasso_model <- glmnet(as.matrix(data[,exposures,with=F]),
                        data$ret_exc_lead1m, alpha = 1, lambda = lambda_best
                        ,weights=data$weights)
  features = row.names(lasso_model$beta)[which(as.numeric(lasso_model$beta)!=0)]
  features
}

get_metrics = function(data) {
  weights=sqrt(abs(data$me))/sum(sqrt(abs(data$me)))
  
  # WEIGHTED CORRELATION
  x_wt_mean <- weighted.mean(data$preds, weights)
  y_wt_mean <- weighted.mean(data$ret_exc_lead1m, weights)
  cov_wt <- cov.wt(data[,c('preds','ret_exc_lead1m')], wt = weights)$cov[1,2]
  wt_cor <- cov_wt / (sqrt(sum(weights * (data$preds - x_wt_mean)^2) / sum(weights)) * 
                        sqrt(sum(weights * (data$ret_exc_lead1m - y_wt_mean)^2) / sum(weights)))
  
  # WEIGHTED ACCURACY
  acc = weighted.mean(sign(data$preds)==sign(data$ret_exc_lead1m),weights)
  
  # TAIL ACCURACY
  quants=fifelse(abs(data$preds) >= quantile(abs(data$preds),0.95),TRUE,FALSE)
  metrics = data[quants == T]
  acc2 = weighted.mean(sign(metrics$preds)==sign(metrics$ret_exc_lead1m),weights[quants==T])
  
  list(weighted_cor=wt_cor, weighted_accuracy=acc, tail_weighted_accuracy=acc2)
}

# Source https://tsimagine.com/2020/09/thinking-about-building-a-volatility-surface-think-again/
filterOptions = function(data) {
  # CALLS
  calls = copy(data)
  # Check for zeros
  calls = calls[cBidPx > 0.05 & cBidIv > 0 & yte > 0]
  # Remove in-the-money options
  calls[,diff_cal_put:=abs(cValue - pValue)]
  calls[,forward:=strike[which.min(diff_cal_put)] + exp(iRate * yte)* (cValue[which.min(diff_cal_put)]  - pValue[which.min(diff_cal_put)]),by=yte]
  calls = calls[strike >= strike[which.min(diff_cal_put)]]
  # Eliminate options where prices are not increasing or decreasing monotonically. 
  calls = calls[order(strike)]
  to_keep = calls[,cValue <= shift(cValue,1,type='lag',fill=Inf),by=yte]
  calls = calls[to_keep$V1 ==T]
  # Eliminate options where the bid/ask spread is too high.
  calls[,spread:=cAskPx - cBidPx]
  calls[,mu:=mean(spread),by=yte]
  calls[,sigma:=sd(spread),by=yte]
  calls = calls[spread < (mu + 4*sigma) | is.na(sigma)]
  # Remove If the number of remaining strikes is below 20% of the median number of strikes on the curve
  calls[,len:=nrow(.SD),by=yte]
  calls[,cutoff:=0.2*median(len)]
  calls = calls[len> cutoff]
  # Liquidity checks
  calls = calls[cVolu != 0 | cOi != 0]
  
  # PUTS
  puts = copy(data)
  puts[,delta:= 1 - delta]
  # Check for zeroes
  puts = puts[pBidPx > 0.05 & pBidIv > 0 & yte > 0]
  # Remove in-the-money options
  puts[,diff_cal_put:=abs(cValue - pValue)]
  puts[,forward:=strike[which.min(diff_cal_put)] + exp(iRate * yte)* (cValue[which.min(diff_cal_put)]  - pValue[which.min(diff_cal_put)]),by=yte]
  puts = puts[strike <= strike[which.min(diff_cal_put)]]
  # Eliminate options where prices are not increasing or decreasing monotonically. 
  puts = puts[order(strike)]
  to_keep = puts[,pValue >= shift(pValue,1,type='lag',fill=0),by=yte]
  puts = puts[to_keep$V1 ==T]
  # Eliminate options where the bid/ask spread is too high.
  puts[,spread:=pAskPx - pBidPx]
  puts[,mu:=mean(spread),by=yte]
  puts[,sigma:=sd(spread),by=yte]
  puts = puts[spread < (mu + 4*sigma) | is.na(sigma)]
  # Remove If the number of remaining strikes is below 20% of the median number of strikes on the curve
  puts[,len:=nrow(.SD),by=yte]
  puts[,cutoff:=0.2*median(len)]
  puts = puts[len > cutoff]
  # Liquidity checks
  puts = puts[pVolu != 0 | pOi != 0]
  
  # Stack
  calls[,type:='call']
  puts[,type:='put']
  all =rbind(calls,puts)
  return(all)
}

# CREATE VOLATILITY SURFACE FROM RAW OPTION PRICES
volSurface = function(data,maturity,delta,call_or_put) {
  h_1 = 0.05
  h_2 = 0.005
  h_3 = 0.001
  x = log(as.numeric(data$yte) / maturity)
  y = data$delta - delta
  z = ifelse(data$type == call_or_put,0,1)
  kernel = exp(-((x^2)/(2*h_1)+(y^2)/(2* h_2)+(z^2)/(2* h_3))) /
    sqrt(2*pi)
  if(call_or_put == 'call') bid = 'cBidIv' else bid= 'pBidIv'
  vol = sum(as.numeric(data$vega) * data[[bid]] * kernel) /
    sum(as.numeric(data$vega) * kernel)
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