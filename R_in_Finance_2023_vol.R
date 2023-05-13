# CREATE VOL SURFACE BY DATE: make a loop for each ORATS day: mclapply for each stock + do dividend yield
# CONCATENATE VOL SURFACE FOR EACH 400 STOCK
# MAKE PREDICTIONS
# FIT THE SURFACE BACK TO OPTIONS (+FUNCTION)
# INFER DIVIDEND YIELD
# OPTION PRICING
# BACKTEST (+FUNCTION)
# 
# option_data[,divRate:=optim(c(0.01), 
#                             get_dividend_yield,data_option=data_option,
#                             end_date=end_dates[i],
#                             lower=c(0))$par
#             ,by=asset]
