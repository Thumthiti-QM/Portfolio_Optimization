> summary(capm_equal_weight)

Call:
lm(formula = ret_equal_weight ~ Mkt.RF, data = df_combined_premod)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.078943 -0.017158 -0.001138  0.017367  0.111294 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 7.931e-03  3.252e-04   24.39   <2e-16 ***
Mkt.RF      8.310e-03  6.111e-05  135.99   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.02898 on 7990 degrees of freedom
Multiple R-squared:  0.6983,	Adjusted R-squared:  0.6983 
F-statistic: 1.849e+04 on 1 and 7990 DF,  p-value: < 2.2e-16

> summary(capm_market_value)

Call:
lm(formula = ret_market_value ~ Mkt.RF, data = df_combined_premod)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.09771 -0.02133 -0.00189  0.01977  0.14953 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 9.700e-03  4.193e-04   23.14   <2e-16 ***
Mkt.RF      1.069e-02  7.878e-05  135.70   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.03736 on 7990 degrees of freedom
Multiple R-squared:  0.6974,	Adjusted R-squared:  0.6974 
F-statistic: 1.841e+04 on 1 and 7990 DF,  p-value: < 2.2e-16

> summary(capm_book_value)

Call:
lm(formula = ret_book_value ~ Mkt.RF, data = df_combined_premod)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.115350 -0.021106 -0.002174  0.020490  0.151561 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 7.055e-03  4.574e-04   15.43   <2e-16 ***
Mkt.RF      1.131e-02  8.594e-05  131.61   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.04076 on 7990 degrees of freedom
Multiple R-squared:  0.6843,	Adjusted R-squared:  0.6843 
F-statistic: 1.732e+04 on 1 and 7990 DF,  p-value: < 2.2e-16

> summary(capm_max_sharpe)

Call:
lm(formula = ret_max_sharpe ~ Mkt.RF, data = df_combined_premod)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.106127 -0.020167 -0.000743  0.018177  0.128690 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 8.132e-03  3.848e-04   21.13   <2e-16 ***
Mkt.RF      9.547e-03  7.231e-05  132.02   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.0343 on 7990 degrees of freedom
Multiple R-squared:  0.6857,	Adjusted R-squared:  0.6856 
F-statistic: 1.743e+04 on 1 and 7990 DF,  p-value: < 2.2e-16

> summary(capm_min_var)

Call:
lm(formula = ret_min_var ~ Mkt.RF, data = df_combined_premod)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.079979 -0.017382 -0.000813  0.018892  0.080344 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 7.037e-03  3.133e-04   22.46   <2e-16 ***
Mkt.RF      7.134e-03  5.887e-05  121.19   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.02792 on 7990 degrees of freedom
Multiple R-squared:  0.6477,	Adjusted R-squared:  0.6476 
F-statistic: 1.469e+04 on 1 and 7990 DF,  p-value: < 2.2e-16

> summary(ff_equal_weight)

Call:
lm(formula = ret_equal_weight ~ Mkt.RF + SMB + HML + RMW + CMA, 
    data = df_combined_premod)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.080503 -0.016149 -0.001032  0.017916  0.104641 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 7.227e-03  3.521e-04  20.528  < 2e-16 ***
Mkt.RF      8.802e-03  7.244e-05 121.507  < 2e-16 ***
SMB         5.664e-04  7.038e-05   8.047 9.68e-16 ***
HML         2.469e-04  4.962e-05   4.975 6.67e-07 ***
RMW         1.557e-04  2.156e-04   0.722     0.47    
CMA         3.358e-03  1.833e-04  18.315  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.0282 on 7912 degrees of freedom
  (74 observations deleted due to missingness)
Multiple R-squared:  0.7164,	Adjusted R-squared:  0.7162 
F-statistic:  3997 on 5 and 7912 DF,  p-value: < 2.2e-16

> summary(ff_market_value)

Call:
lm(formula = ret_market_value ~ Mkt.RF + SMB + HML + RMW + CMA, 
    data = df_combined_premod)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.092727 -0.022491 -0.000773  0.019335  0.143258 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  7.622e-03  4.591e-04  16.602  < 2e-16 ***
Mkt.RF       1.112e-02  9.446e-05 117.759  < 2e-16 ***
SMB         -4.756e-04  9.178e-05  -5.182 2.25e-07 ***
HML          2.549e-04  6.471e-05   3.938 8.28e-05 ***
RMW          1.123e-03  2.812e-04   3.995 6.53e-05 ***
CMA          3.588e-03  2.391e-04  15.009  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.03677 on 7912 degrees of freedom
  (74 observations deleted due to missingness)
Multiple R-squared:  0.7079,	Adjusted R-squared:  0.7077 
F-statistic:  3835 on 5 and 7912 DF,  p-value: < 2.2e-16

> summary(ff_book_value)

Call:
lm(formula = ret_book_value ~ Mkt.RF + SMB + HML + RMW + CMA, 
    data = df_combined_premod)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.120007 -0.022402 -0.001346  0.020357  0.149324 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  6.029e-03  5.022e-04  12.004  < 2e-16 ***
Mkt.RF       1.159e-02  1.033e-04 112.118  < 2e-16 ***
SMB         -2.230e-04  1.004e-04  -2.221   0.0264 *  
HML          4.391e-04  7.079e-05   6.202 5.85e-10 ***
RMW         -2.667e-04  3.076e-04  -0.867   0.3859    
CMA          3.273e-03  2.615e-04  12.514  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.04023 on 7912 degrees of freedom
  (74 observations deleted due to missingness)
Multiple R-squared:  0.6942,	Adjusted R-squared:  0.694 
F-statistic:  3592 on 5 and 7912 DF,  p-value: < 2.2e-16

> summary(ff_max_sharpe)

Call:
lm(formula = ret_max_sharpe ~ Mkt.RF + SMB + HML + RMW + CMA, 
    data = df_combined_premod)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.107375 -0.019315 -0.000054  0.019198  0.121037 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  7.806e-03  4.190e-04  18.630  < 2e-16 ***
Mkt.RF       1.003e-02  8.621e-05 116.285  < 2e-16 ***
SMB          7.379e-04  8.376e-05   8.810  < 2e-16 ***
HML          4.218e-04  5.906e-05   7.141 1.01e-12 ***
RMW         -7.324e-05  2.566e-04  -0.285    0.775    
CMA          3.234e-03  2.182e-04  14.819  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.03356 on 7912 degrees of freedom
  (74 observations deleted due to missingness)
Multiple R-squared:  0.7008,	Adjusted R-squared:  0.7006 
F-statistic:  3706 on 5 and 7912 DF,  p-value: < 2.2e-16

> summary(ff_min_var)

Call:
lm(formula = ret_min_var ~ Mkt.RF + SMB + HML + RMW + CMA, data = df_combined_premod)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.081008 -0.015950 -0.000501  0.016604  0.073101 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 5.956e-03  3.382e-04  17.611  < 2e-16 ***
Mkt.RF      7.680e-03  6.959e-05 110.367  < 2e-16 ***
SMB         5.407e-04  6.761e-05   7.997 1.45e-15 ***
HML         1.435e-05  4.767e-05   0.301   0.7634    
RMW         5.223e-04  2.071e-04   2.521   0.0117 *  
CMA         3.485e-03  1.761e-04  19.789  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.02709 on 7912 degrees of freedom
  (74 observations deleted due to missingness)
Multiple R-squared:  0.6709,	Adjusted R-squared:  0.6706 
F-statistic:  3225 on 5 and 7912 DF,  p-value: < 2.2e-16
