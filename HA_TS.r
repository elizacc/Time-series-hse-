library(tseries)
library(tidyverse)
library(forecast)
library(gridExtra)
library(urca)
library(tibbletime)
library(fpp)
library(vars)
library(readxl)
library(VARsignR)
library(lattice)
library(matrixcalc)
library(xts)

# Data
# setwd("/Users/elizaveta.makhneva/downloads")
df <- read.csv("econ_time_series.csv")
head(df)

df$index <- as.Date(paste(df$index), format = "%d.%m.%Y")
df$SBER <- as.double(gsub(",", ".", df$SBER))
df$VTBR <- as.double(gsub(",", ".", df$VTBR))
df$X1year_rate <- as.double(gsub(",", ".", df$X1year_rate))
df$exchange_rate <- as.double(gsub(",", ".", df$exchange_rate))

# Time-series data
dfts <- xts(df[, 2:ncol(df)], order.by = df$index, frequency = 7)
dfts$date = df$date
head(dfts)

# Charts of our time-series
# SBER
ggplot(data = dfts[, "SBER"]) + geom_line(aes(y = SBER, x = df$index)) +
  xlab("Date")+ ylab("Stock price") +
  ggtitle("SBER stock price dynamics")
# We can see different variance for different time-periods
# We can use Box-Cox transformation
ggplot(data = BoxCox(dfts[, "SBER"], lambda = -1/2)) + geom_line(aes(y = SBER, x = df$index)) +
  xlab("Date")+ ylab("Stock price") +
  ggtitle("SBER stock price with Box-Cox transformation dynamics")
# That's not perfect but better
dfts$SBER_BC = BoxCox(dfts[, "SBER"], lambda = -1/2)

# Charts of first differences
ggplot(data = diff(dfts)) +
  geom_line(aes(y = SBER, x = tail(df$index, nrow(diff(dfts))))) +
  xlab("Date") + ylab("Stock price") + 
  ggtitle("SBER stock price dynamics, first-diference")
# for SBER_BC
ggplot(data = diff(dfts)) +
  geom_line(aes(y = SBER_BC, x = tail(df$index, nrow(diff(dfts))))) +
  xlab("Date") + ylab("Stock price") + 
  ggtitle("SBER stock price with Box-Cox transformation dynamics, first-diference")

# Charts of second differences
ggplot(data = diff(dfts, differences = 2)) +
  geom_line(aes(y = SBER, x = tail(df$index, nrow(diff(dfts, differences = 2))))) +
  xlab("Date") + ylab("Stock price") + 
  ggtitle("SBER stock price dynamics, second-diference")
# for SBER_BC
ggplot(data = diff(dfts, differences = 2)) +
  geom_line(aes(y = SBER_BC, x = tail(df$index, nrow(diff(dfts, differences = 2))))) +
  xlab("Date") + ylab("Stock price") + 
  ggtitle("SBER stock price with Box-Cox transformation dynamics, second-diference")
# Variance is still not perfect but it's better than with original time-series
# It's not obvious which one of differenced time-series to use
# We will chose one of them in the following analysis

# VTBR
ggplot(data = dfts) + geom_line(aes(y = VTBR, x = df$index)) +
  xlab("Date")+ ylab("Stock price") +
  ggtitle("VTBR stock price dynamics")

# Chart of first differences
ggplot(data = diff(dfts)) +
  geom_line(aes(y = VTBR, x = tail(df$index, nrow(diff(dfts))))) +
  xlab("Date") + ylab("Stock price") + 
  ggtitle("VTBR stock price dynamics, first-diference")
# This time-series is strange but seems like variance is almost constant

# Chart of second differences
ggplot(data = diff(dfts, differences = 2)) +
  geom_line(aes(y = VTBR, x = tail(df$index, nrow(diff(dfts, differences = 2))))) +
  xlab("Date") + ylab("Stock price") + 
  ggtitle("VTBR stock price dynamics, second-diference")
# This time-series seems to be I(1), second-differences don't look good
# Let's take series with first differences

#One-year rate
ggplot(data = dfts) +
  geom_line(aes(y = X1year_rate, x = df$index)) +
  xlab("Date")+ ylab("Interest rate, %") +
  ggtitle("Interest rate dynamics")
# We can see different variance for different time-periods
# We can use Box-Cox transformation
ggplot(data = BoxCox(dfts[, "X1year_rate"], lambda = -1/2)) + geom_line(aes(y = X1year_rate, x = df$index)) +
  xlab("Date")+ ylab("Interest rate") +
  ggtitle("Interest rate with Box-Cox transformation dynamics")
# There are still different variances but not in previous scale
dfts$X1year_rate_BC = BoxCox(dfts[, "X1year_rate"], lambda = -1/2)

# Charts of first differences
ggplot(data = diff(dfts)) +
  geom_line(aes(y = X1year_rate, x = tail(df$index, nrow(diff(dfts))))) +
  xlab("Date") + ylab("Interest rate, %") + 
  ggtitle("Interest rate dynamics, first-diference")
# for X1year_rate_BC
ggplot(data = diff(dfts)) +
  geom_line(aes(y = X1year_rate_BC, x = tail(df$index, nrow(diff(dfts))))) +
  xlab("Date") + ylab("Interest rate") + 
  ggtitle("Interest rate with Box-Cox transformation dynamics, first-diference")

# Charts of second differences
ggplot(data = diff(dfts, differences = 2)) +
  geom_line(aes(y = X1year_rate, x = tail(df$index, nrow(diff(dfts, differences = 2))))) +
  xlab("Date") + ylab("Interest rate, %") + 
  ggtitle("Interest rate dynamics, second-diference")
ggplot(data = diff(dfts, differences = 2)) +
  geom_line(aes(y = X1year_rate_BC, x = tail(df$index, nrow(diff(dfts, differences = 2))))) +
  xlab("Date") + ylab("Interest rate") + 
  ggtitle("Interest rate dynamics with Box-Cox transformation dynamics, second-diference")
# It's not obvious which one of differenced time-series to use
# We will chose one of them in the following analysis

# Exchange rate
ggplot(data = dfts) +
  geom_line(aes(y = exchange_rate, x = df$index)) +
  xlab("Date")+ ylab("Exchange rate, RUB/USD") +
  ggtitle("Exchange rate RUB/USD dynamics")
# We can see different variance for different time-periods
# We can use Box-Cox transformation
ggplot(data = BoxCox(dfts[, "exchange_rate"], lambda = -1/2)) + geom_line(aes(y = exchange_rate, x = df$index)) +
  xlab("Date")+ ylab("Exchange rate, RUB/USD") +
  ggtitle("Exchange rate with Box-Cox transformation RUB/USD dynamics")
# Variance is almost constant besides one specific period around 2015 year
dfts$exchange_rate_BC = BoxCox(dfts[, "exchange_rate"], lambda = -1/2)

# Charts of first differences
ggplot(data = diff(dfts)) +
  geom_line(aes(y = exchange_rate, x = tail(df$index, nrow(diff(dfts))))) +
  xlab("Date") + ylab("Exchange rate, RUB/USD") + 
  ggtitle("Exchange rate RUB/USD dynamics, first-diference")
# for exchange_rate_BC
ggplot(data = diff(dfts)) +
  geom_line(aes(y = exchange_rate_BC, x = tail(df$index, nrow(diff(dfts))))) +
  xlab("Date") + ylab("Exchange rate, RUB/USD") + 
  ggtitle("Exchange rate RUB/USD with Box-Cox transformation dynamics, first-diference")

# Charts of second differences
ggplot(data = diff(dfts, differences = 2)) +
  geom_line(aes(y = exchange_rate, x = tail(df$index, nrow(diff(dfts, differences = 2))))) +
  xlab("Date") + ylab("Exchange rate, RUB/USD") + 
  ggtitle("Exchange rate RUB/USD dynamics, second-diference")
ggplot(data = diff(dfts, differences = 2)) +
  geom_line(aes(y = exchange_rate_BC, x = tail(df$index, nrow(diff(dfts, differences = 2))))) +
  xlab("Date") + ylab("Exchange rate, RUB/USD") + 
  ggtitle("Exchange rate RUB/USD with Box-Cox transformation dynamics, second-diference")
# We can see that it's better to use series with Box-Cox transformation
# as there we can see less variance range in different time periods
# But it's not obvious whether we should take first or second difference

# Differences of time-series (without first observations containing NAs)

diff_1 = na.omit(diff(dfts))
diff_2 = na.omit(diff(dfts,differences = 2))

# Let's make tests for unit root

u1 = ur.df(dfts$SBER, type = 'trend', lags = 8, selectlags = "BIC")
u2 = ur.df(diff_1$SBER, lags = 8, selectlags = "BIC")
u2_BC = ur.df(diff_1$SBER_BC, lags = 8, selectlags = "BIC")
u3 = ur.df(diff_2$SBER, lags = 8, selectlags = "BIC")
u3_BC = ur.df(diff_2$SBER_BC, lags = 8, selectlags = "BIC")


summary(u1) # not rejected
summary(u2) # rejected
summary(u2_BC) # rejected
summary(u3) # rejected
summary(u3_BC) # rejected

u1 = ur.df(dfts$VTBR, type = 'trend', lags = 8, selectlags = "BIC")
u2 = ur.df(diff_1$VTBR, lags = 8, selectlags = "BIC")
u3 = ur.df(diff_2$VTBR, lags = 8, selectlags = "BIC")

summary(u1) # not rejected
summary(u2) # rejected
summary(u3) # rejected

u1 = ur.df(dfts$X1year_rate, type = 'trend', lags = 8, selectlags = "BIC")
u2 = ur.df(diff_1$X1year_rate, lags = 8, selectlags = "BIC")
u2_BC = ur.df(diff_1$X1year_rate_BC, lags = 8, selectlags = "BIC")
u3 = ur.df(diff_2$X1year_rate, lags = 8, selectlags = "BIC")
u3_BC = ur.df(diff_2$X1year_rate_BC, lags = 8, selectlags = "BIC")

summary(u1) # not rejected
summary(u2) # rejected
summary(u2_BC) # rejected
summary(u3) # rejected
summary(u3_BC) # rejected

u1 = ur.df(dfts$exchange_rate, type = 'trend', lags = 8, selectlags = "BIC")
u2 = ur.df(diff_1$exchange_rate_BC, lags = 8, selectlags = "BIC")
u3 = ur.df(diff_2$exchange_rate-BC, lags = 8, selectlags = "BIC")

summary(u1) # not rejected
summary(u2) # rejected
summary(u3) # rejected

# And more tests

kpss1 <- ur.kpss(dfts$SBER, type = "tau", lags = "long")
kpss2 <- ur.kpss(diff_1$SBER, type = "mu", lags = "long")
kpss2_BC <- ur.kpss(diff_1$SBER_BC, type = "mu", lags = "long")
kpss3 <- ur.kpss(diff_2$SBER, type = "mu", lags = "long")
kpss3_BC <- ur.kpss(diff_2$SBER_BC, type = "mu", lags = "long")

summary(kpss1) # rejected
summary(kpss2) # not rejcted
summary(kpss2_BC) # not rejcted
summary(kpss3) # not rejcted
summary(kpss3_BC) # not rejcted

kpss1 <- ur.kpss(dfts$VTBR, type = "tau", lags = "long")
kpss2 <- ur.kpss(diff_1$VTBR, type = "mu", lags = "long")
kpss3 <- ur.kpss(diff_2$VTBR, type = "mu", lags = "long")

summary(kpss1) # rejected
summary(kpss2) # not rejected
summary(kpss3) # not rejected

kpss1 <- ur.kpss(dfts$X1year_rate, type = "tau", lags = "long")
kpss2 <- ur.kpss(diff_1$X1year_rate, type = "mu", lags = "long")
kpss2_BC <- ur.kpss(diff_1$X1year_rate_BC, type = "mu", lags = "long")
kpss3 <- ur.kpss(diff_2$X1year_rate, type = "mu", lags = "long")
kpss3_BC <- ur.kpss(diff_1$X1year_rate_BC, type = "mu", lags = "long")

summary(kpss1) # rejected
summary(kpss2) # not rejected
summary(kpss2_BC) # not rejected
summary(kpss3) # not rejected
summary(kpss3_BC) # not rejected

kpss1 <- ur.kpss(dfts$exchange_rate, type = "tau", lags = "long")
kpss2 <- ur.kpss(diff_1$exchange_rate_BC, type = "mu", lags = "long")
kpss3 <- ur.kpss(diff_2$exchange_rate_BC, type = "mu", lags = "long")

summary(kpss1) # rejected
summary(kpss2) # not rejected
summary(kpss3) # not rejected

# For each time-series in the tests above we can take first difference
# as they are seems to be I(1). And as we have no evidence that series with
# Box-Cox transformation better than original one (in difference or not;
# besides exchange rate) we can take them without any transformation

# Multivariate forecasting

# Let's look at ACF/PACF
# VTBR prices are really strange, so I won't take it for VAR model, only 3 left

ggAcf(diff_1$SBER) + ggtitle("Autocorrelation for SBER stock price, first-difference")
ggPacf(diff_1$SBER)+ ggtitle("Partial autocorrelation for SBER stock price, first-difference")

ggAcf(diff_1$X1year_rate) + ggtitle("Autocorrelation for one-year rate, first-difference")
ggPacf(diff_1$X1year_rate)+ ggtitle("Partial autocorrelation for one-year rate, first-difference")

ggAcf(diff_1$exchange_rate_BC) + ggtitle("Autocorrelation for Exchange rate RUB/USD, first-difference")
ggPacf(diff_1$exchange_rate_BC)+ ggtitle("Partial autocorrelation for Exchange rate RUB/USD, first-difference")
# For each time-series we see that we can check 16 lags for model selection:
# others lags are not so significant

df_var <- data.frame("SBER_1diff" = diff_1[, "SBER"],
                "1year_rate_1diff" = diff_1[, "X1year_rate"],
                "exchange_rate_BC_1diff" = diff_1[, "exchange_rate_BC"])

data = ts(df_var,frequency = 4)
train = head(data, nrow(data)-12)
test = tail(data,12)


VARselect(train, lag.max = 16, type = "const")$criteria
VARselect(train, lag.max = 16, type = "const")$selection

# The best model is with 1 lag (via IC)

var1 <- VAR(train, p = 1, type = "const")
summary(var1)

serial.test(var1, lags.pt = 10, type = "PT.asymptotic") # rejected
serial.test(var1, lags.pt = 10, type = "PT.adjusted") # rejected
serial.test(var1, lags.pt = 10, type = "BG") # rejected

# Let's also look at 2 lags model
var2 <- VAR(train, p = 2, type = "const")
serial.test(var2, lags.pt = 10, type = "PT.asymptotic") # rejected
serial.test(var2, lags.pt = 10, type = "PT.adjusted") # rejected
serial.test(var2, lags.pt = 10, type = "BG") # not rejected at 1%

#Check resuduals normality
normality.test(var1, multivariate.only = FALSE) # rejected
normality.test(var2, multivariate.only = FALSE) # rejected

# Check causality
causality(var1, 'SBER') # rejected
causality(var1, 'X1year_rate') # rejected
causality(var1, 'exchange_rate_BC') # rejected

causality(var2, 'SBER') # rejected
causality(var2, 'X1year_rate') # rejected
causality(var2, 'exchange_rate_BC') # rejected

# There is no Granger causality in any model

# Let's estimate forecasts

var_fcst1 <- forecast(var1, h = 12)
var_fcst2 <- forecast(var2, h = 12)
var_fcst3 <- forecast(var3, h = 12)

acc_1 = accuracy(var_fcst1, x = test, D = 0, d = 1)
acc_2 = accuracy(var_fcst2, x = test, D = 0, d = 1)
acc_3 = accuracy(var_fcst3, x = test, D = 0, d = 1)
 
acc_1 = acc_1[endsWith(row.names(acc_1), "Test set"), ]
acc_2 = acc_2[endsWith(row.names(acc_2), "Test set"), ]
acc_3 = acc_3[endsWith(row.names(acc_3), "Test set"), ]

row.names(acc_1) = stringr::str_replace(row.names(acc_1), "\\Test set$",  "VAR(1)")
row.names(acc_2) = stringr::str_replace(row.names(acc_2), "\\Test set$",  "VAR(2)")
row.names(acc_3) = stringr::str_replace(row.names(acc_3), "\\Test set$",  "VAR(3)")

all_acc = rbind(acc_1, acc_2, acc_3)

acc_sber = all_acc[startsWith(row.names(all_acc), "SBER"), ]
acc_vtbr = all_acc[startsWith(row.names(all_acc), "VTBR"), ]
acc_rate = all_acc[startsWith(row.names(all_acc), "X1year_rate"), ]
acc_ex_rate = all_acc[startsWith(row.names(all_acc), "exchange_rate"), ]
print(rbind(acc_sber, acc_vtbr, acc_rate, acc_ex_rate))

autoplot(var_fcst1)
autoplot(var_fcst2)
autoplot(var_fcst3)
