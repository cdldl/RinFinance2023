# FIXME: INFER DIVIDEND YIELD get_dividend_yield
# FIT THE SURFACE BACK TO OPTIONS (+FUNCTION)
# OPTION PRICING
# BACKTEST (+FUNCTION)


#: make a loop for each ORATS day: mclapply for each stock + do dividend yield
# 
# option_data[,divRate:=optim(c(0.01), 
#                             get_dividend_yield,data_option=data_option,
#                             end_date=end_dates[i],
#                             lower=c(0))$par
#             ,by=asset]