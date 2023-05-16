# DOWNLOAD / LOAD LIBRARIES
chooseCRANmirror(ind=1)
list.of.packages <- c("data.table","fasttime","anytime","RQuantLib","PerformanceAnalytics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, function(x) require(x, character.only = TRUE))

# CONSTANTS
initial_capital = 1e4
multiplier = 100
riskfree = (1+0.0165)^(1/12) -1
option_path_output = "/home/cyril/TRADING/ORATS/RinFinance/"
benchmark_path='/home/cyril/TRADING/EOD/data/US/XLK.US.csv'
source(file = "/home/cyril/RinFinance/R_in_Finance_2023_functions.R")

# LOAD OPTIONS 
files = list.files(option_path_output, full.names = T)
options = NULL
for(file in files) {
  tmp_file = fread(file)
  options = rbind(options,tmp_file)
}
options[,type:=fifelse(expected_returns_call>0,'call','put')]

# METRICS TRADING
val = options[year(date) < 2018]
cut_exp_ret = cut(val$expected_returns,
                  unique(quantile(val$expected_returns, seq(0,1,0.05))))
cut_exp_delta = cut(val$delta,seq(0,1,0.2))
cut_exp_mat = cut(val$maturity,unique(quantile(val$maturity, seq(0,1,0.2))))
cut_exp_margin = cut(val$margin,unique(quantile(val$margin,seq(0,1,0.2))))
print(tapply(val$real_returns, list(cut_exp_ret,cut_exp_delta,cut_exp_margin),mean,na.rm=T)) 

# METRICS ABOVE MADE ME SELECT THESE THRESHOLDS
test = options[year(date)>= 2018 & expected_returns >0.8  & delta > 0.35 &
                 delta < 0.65 & maturity < 0.16 & margin > 250 & margin < 1000] 

# INVEST DAILY A MAXIMUM OF YOUR INITIAL CAPITAL (FIXED CAPITAL OVER TIME)
trading = test[order(-expected_returns),.SD,by=date]
trading[,cum_margin:=cumsum(margin),by=date]
trading2 = trading[cum_margin <= initial_capital]

# PERFORMANCE AGAINST BENCHMARK
trading3 = trading2[,list(ret=mean(real_returns)/multiplier),by=date]
benchmark =fread(benchmark_path)
benchmark[,benchmark:=(Adjusted_close - shift(Adjusted_close,1,type='lag'))/
            shift(Adjusted_close,1,type='lag')]
benchmark$benchmark[1] = 0
benchmark[,date:=Date]
combined = merge(trading3,benchmark[,c('date','benchmark'),with=F],by='date')
xt = xts(cbind(ret=combined[,2],benchmark=combined[,3]),order.by=as.Date(combined$date))
ratios = calculate_ratios(xt$ret,xt$benchmark,riskfree)
print(ratios)