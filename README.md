# RinFinance2023: Ensemble Option Forecasting

## 1. Forecasting Stock returns

*S*<sub>*t*</sub>:

1.  Features as per Bryan Kelly’s work (ExpectedReturns library soon)
2.  Missing values (ImputeFin library)
3.  Standardization (FactorAnalytics library)
4.  Making predictions (lightgbm library)

## 2. Forecasting variance

*σ*<sup>2</sup>:

1.  Filtration (bid available, exclude ITM and so on)
2.  Volatility surface according to OptionMetrics
3.  Making predictions (lightgbm library)
4.  Curve fitting to match with respective option maturities and delta

## *σ*<sup>2</sup>: Volatility Surface

Kernel function of Smoothed volatility:

$$ \Phi(x,y,z)= \frac{1}{\sqrt{2\pi}}e^{\[(x^2/2h\_1)+(y^2/2h\_2)+(z^2/2h\_3)\]} $$

$$ \hat{\sigma}_j = \frac{\sum V\_i \sigma\_i \Phi(x\_{ij},y\_{ij},z\_{ij})}{\sum V\_i \Phi(x\_{ij},y\_{ij},z\_{ij}}$$

## 3. Forecasting Options

1.  Infer implied dividend yield
2.  Option Pricing (RQuantLib library)
3.  Option Ranking
4.  Backtest Results
