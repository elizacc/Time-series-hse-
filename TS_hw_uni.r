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

setwd("~/Documents/kekonom")
options(scipen = 999) 


#data preparation
df <- read.csv("econ_times_series_new.csv", sep = ",")

df$index <- as.Date(paste(df$index), format = "%d.%m.%Y")
df$SBER <- as.double(gsub(",", ".", df$SBER))
df$VTBR <- as.double(gsub(",", ".", df$VTBR))
df$X1year_rate <- as.double(gsub(",", ".", df$X1year_rate))
df$exchange_rate <- as.double(gsub(",", ".", df$exchange_rate))


# Time-series data
dfts <- xts(df[, 2:ncol(df)], order.by = df$index, frequency = 7)
dfts$date = df$date
head(dfts)

#devide data on test and training sets

sber_train = head(dfts$SBER, nrow(dfts$SBER) - 12)
sber_test = tail(dfts$SBER, 12)

# ACF/PACF of train part without difference
ggAcf(sber_train) + ggtitle("Autocorreation of Sber stock price")
ggPacf(sber_train) + ggtitle("Partial autocorreation of Sber stock price")

#  ACF/Pacf with difference
diff_1 = na.omit(diff(dfts))
sber_diff_train = head(diff_1$SBER, nrow(diff_1$SBER) - 12)
sber_diff_test = tail(diff_1$SBER, 12)

ggAcf(sber_diff_train) + ggtitle("Autocorreation of Sber stock price")
ggPacf(sber_diff_train) + ggtitle("Partial autocorreation of Sber stock price")

# There is no sense to take too many lags, 
#so we can say the maximum number of lags we can examinate is 10. Others are insagnificant
# (Unfortunately, due to small computing power, computer break the process and stop calculating models with lags higher than 10)

a <- rep(0, times = 121)
A <- matrix(a, nrow = 11)
maxim = 3000

b <- rep(0, times = 121)
B <- matrix(b, nrow = 11)
maxim_2 = 3000
for (i in 1:11) {
  for (j in 1:11) {
    model <-  Arima(sber_diff_train, order = c(i - 1, 0, j - 1), include.drift = FALSE)
    if ((model$aicc) < maxim){
      maxim = model$aicc
    }
    if ((model$bic) < maxim_2){
      maxim_2 = model$bic
    }
    B[i,j] <- model$bic
    A[i,j] <- model$aicc
  }
}

# A - matcix of AICc's
A
# B - matrix of BIC's
B

min(A)
min(B)

# Best model by AICc ARMA(4,4)
# Best model by BIC ARMA(0,0)



# also let's check model with 11 and 12 lags, because on graphs this lag looks big too. (just in case)
add = Arima(sber_diff_train, order = c(11, 0, 11), include.drift = FALSE)
summary(add)
add = Arima(sber_diff_train, order = c(12, 0, 12), include.drift = FALSE)
summary(add)
# models with bigger number of lags seem to be worse than with littel

# Let's estimate choosen models
best1 = Arima(sber_diff_train, order = c(4,0,4), include.drift = FALSE)
summary(best1)

# Best model by BIC ARMA(0,0)
best2 = Arima(sber_diff_train, order = c(0,0,0), include.drift = FALSE)
summary(best2)

best_auto = auto.arima(sber_diff_train)
summary(best_auto) # Best model by auto raima is ARIMA (0,0,0)

# Lets check residuals

ggAcf(residuals(best1)) # spickes are smal
ggAcf(residuals(best2)) # some spickes looks big, here may be problems with residuals

Box.test(residuals(best1), lag = 15, fitdf = 3,
         type = 'Ljung') # not rejected -> no autocorrelation in residuals

Box.test(residuals(best2), lag = 15, fitdf = 3,
         type = 'Ljung') # not rejected at 1% but rejected at 5%-> may be autocorrelation in residuals


# Lets test nonnormality

jarque.bera.test(residuals(best1)) # rejected, doesn't behave like normal distribution
jarque.bera.test(residuals(best2)) # rejected

# Plotting forecasts

fore1 <- forecast(best1, h = 12)
autoplot(fore1) 

fore2 <- forecast(best2, h = 12)
autoplot(fore2) 


scores = rbind(accuracy(forecast(best1, h = 12), sber_diff_test),
               accuracy(forecast(best2, h = 12), sber_diff_test))

row.names(scores) = c("ARIMA(4,0,4) train", "ARIMA(4,0,4) test", "ARIMA(0,0,0) train",
                      "ARIMA(0,0,0) test")
scores
