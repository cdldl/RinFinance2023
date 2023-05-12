# RinFinance2023: Ensemble Option Forecasting

## 1. Forecasting Stock returns

*S*<sub>*t*</sub>:

1.  Features as per Bryan Kelly’s work (ExpectedReturns library soon)
2.  Missing values (ImputeFin library)
3.  Standardization (FactorAnalytics library)
4.  Making predictions (lightgbm library)

## *S*<sub>*t*</sub>: Missing values

    library(data.table)
    library(doMC)
    library(imputeFin)

    imputing_values = function(tmp_data, train_cutoff_date) {
      tmp_data[,(exposures):=mclapply(.SD,function(x) tryCatch(jitter(x),
             error=function(e) x, warning=function(w) x), 
             mc.cores=cores, mc.silent=T)
           ,by=asset,.SDcols=exposures]
      tmp_data[,(exposures):=mclapply(.SD, function(x) 
           tryCatch(impute_AR1_t,
             error=function(e) x, warning=function(w) x)
                                  , remove_outliers = TRUE
                                  , mc.cores=cores
                                  , mc.silent = TRUE)
           ,by=asset,.SDcols=exposures]
      tmp_data
    }

## *S*<sub>*t*</sub>: Features Standardization

$$ts_i = ts_i - \mu_{ts}$$

$$s_1 = ts_i$$

$$s_i = (1 - \lambda) ts_i + \lambda * s_{i-1}$$

      library(FactorAnalytics)
      
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

## *S*<sub>*t*</sub>: Target Standardization

    library(FactorAnalytics)
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

## *S*<sub>*t*</sub>: Making predictions

    library(lightgbm)
    lightgbm_model = function(train, val, exposures) {  
      dtrain <- lgb.Dataset(as.matrix(train[,exposures,with=F])
                            , label = train$ret_exc_lead1m_norm)
      dval <- lgb.Dataset.create.valid(dtrain,
        data=as.matrix(val[,exposures,with=F]),
        label = val$ret_exc_lead1m_norm) 
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

## 2. Forecasting variance

*σ*<sup>2</sup>:

1.  Filtration (bid available, exclude ITM and so on)
2.  Volatility surface according to OptionMetrics
3.  Making predictions (lightgbm library)
4.  Curve fitting to match with respective option maturities and delta

## *σ*<sup>2</sup>: Volatility Surface

Kernel function of Smoothed volatility:

$$ \Phi(x,y,z)= \frac{1}{\sqrt{2\pi}}e^{\[(x^2/2h\_1)+(y^2/2h\_2)+(z^2/2h\_3)\]} $$

$$ \frac{\sum V\_i \sigma\_i \Phi(x\_{ij},y\_{ij},z\_{ij})}{\sum V\_i \Phi(x\_{ij},y\_{ij},z\_{ij}}$$

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


## 3. Forecasting Options

1.  Infer implied dividend yield
2.  Option Pricing (RQuantLib library)
3.  Option Ranking
4.  Backtest Results

## Forecasting Options: Pricing Engine

    #library(RQuantLib)

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

