library(xts)
library(forecast)
library(ggplot2)
library(gridExtra)

options(scipen = 999)          

# Function for interpolation of some NA's.
inter = function(ts){
  for (i in (1:length(ts))){
    if (is.na(ts[i])){
      ts[i] = mean(c(ts[i-1],ts[i+1]))
    }
  }
  return(ts)
}


# Data preparation

dd <- read.csv("data.csv", sep = ";")
dd$BRENT = inter(dd$BRENT)
dd$DATE <- as.Date(paste(dd$DATE), format = "%d.%m.%Y")

ddts <- xts(dd[, 2:ncol(dd)], order.by = dd$DATE, frequency = 4)
ddts$DATE = dd$DATE

YNames <- colnames(ddts)[substr(colnames(ddts), 1, 2) == "P_"]
XNames <- colnames(ddts)[substr(colnames(ddts), 1, 2) != "P_"]

# Leaving out all exogeneous variables with NA's
ddts = ddts[,colSums(is.na(ddts)) == 0]

# Plotting our coal time series

ggplot(data = ddts[,YNames]) + geom_line(aes(y = P_COAL_FOB_NEWCASTLE, x = dd$DATE, color="Newcastle")) +
         geom_line(aes(y = P_COAL_API2_NWEUR, x = dd$DATE,color='Rotterdam'))  + xlab("Date")+ ylab("Spot price")+ 
ggtitle("Coal spot prices in Newcastle and Rotterdam")  +
  scale_colour_manual("",      values = c("Newcastle"="coral", "Rotterdam"="steelblue"))




# Let's cut off 2008's crisis

ddts = window(ddts, start = "2009-01-04")


# In univariate analysis let's work with Newcastle prices
# Plot in levels

ggplot(data = ddts[,YNames]) + geom_line(aes(y = P_COAL_FOB_NEWCASTLE, x = tail(dd$DATE, nrow(ddts))), color="coral") +
  geom_line(aes(y = P_COAL_API2_NWEUR, x = tail(dd$DATE, nrow(ddts))),color='steelblue')  + xlab("Date")+ ylab("Spot price")+ 
  ggtitle("Spot coal prices in Newcastle (coral) and Rotterdam (blue)")


# Plot in first differences
ggplot(data = diff(ddts[,YNames])) + geom_line(aes(y = P_COAL_FOB_NEWCASTLE, x = tail(dd$DATE, nrow(diff(ddts)))), color="steelblue") + xlab("Date")+ ylab("Spot price")+ 
  ggtitle("Spot coal prices in Newcastle, first-diference")


# Plot in second differences
ggplot(data = diff(ddts[,YNames],differences = 2)) + geom_line(aes(y = P_COAL_FOB_NEWCASTLE, x = tail(dd$DATE, nrow(diff(ddts,differences = 2)))), color="steelblue")+ xlab("Date")+ ylab("Spot price")+ 
  ggtitle("Spot coal prices in Newcastle, second-diference")

# Seeng volatility clustering

# Let's save diff data, omitting first NA-observation
diff_1 = na.omit(diff(ddts[,YNames]))
diff_2 = na.omit(diff(ddts[,YNames],differences = 2))



# Let's determine integration level
  

library(urca)

u1 = ur.df(ddts$P_COAL_FOB_NEWCASTLE, type = 'trend', lags = 8, selectlags = "BIC")
u2 = ur.df(diff_1$P_COAL_FOB_NEWCASTLE, type = 'trend', lags = 8, selectlags = "BIC")
u3 = ur.df(diff_2$P_COAL_FOB_NEWCASTLE, type = 'trend', lags = 8, selectlags = "BIC")

summary(u1)
summary(u2)
summary(u3)


kpss1 <- ur.kpss(ddts$P_COAL_FOB_NEWCASTLE, type = "mu",
                 lags = "long")

kpss2 <- ur.kpss(diff_1$P_COAL_FOB_NEWCASTLE, type = "mu",
                 lags = "long")

kpss3 <- ur.kpss(diff_2$P_COAL_FOB_NEWCASTLE, type = "mu",
                 lags = "long")
summary(kpss1)
summary(kpss2)
summary(kpss3)

# Tests results:
  
  # adf: not rejected, rejected, rejected
  # KPSS: not rejected (on the border of 5%), not rejected, not rejected

# So, integration level is I(1)

# Let's use first-diff data and check ACF/PACF

ggAcf(diff_1$P_COAL_FOB_NEWCASTLE) + ggtitle("Autocorrelation for Newcastle coal prices")
ggPacf(diff_1$P_COAL_FOB_NEWCASTLE)+ ggtitle("Partial autocorrelation for Newcastle coal prices")



#Univariate forecasting
# Let's prepare our data. The "data" variable will contain two coal prices,
# Brent price and USD to CNY exchange rate. Coal prices are first-diffrence.

c(YNames, "FX_USD_CNY", "BRENT")

data = diff_1[, YNames]
data$FX_USD_CNY = ddts$FX_USD_CNY[2:nrow(ddts)]
data$BRENT = ddts$BRENT[2:nrow(ddts)]

# Plotting data for coal prices

g_c1 = ggplot(data = data) + geom_line(aes(y = P_COAL_FOB_NEWCASTLE, x = tail(dd$DATE, nrow(data))), color="coral") + xlab("Date")+ ylab("Spot price")+ 
  ggtitle("Spot coal prices in Newcastle, first-diference")


g_c2 = ggplot(data = data) + geom_line(aes(y = P_COAL_API2_NWEUR, x = tail(dd$DATE, nrow(data))),color='steelblue')+ xlab("Date")+ ylab("Spot price")+ 
  ggtitle("Spot coal prices in Rotterdam, first-diference")

grid.arrange(g_c1, g_c2)

# Here we separate our newcastle ts on train and test.

coal = data$P_COAL_FOB_NEWCASTLE
coal_train = head(coal, nrow(coal) - 12)
coal_test = tail(coal, 12)

# ACF/PACF of train part
ggAcf(coal_train) + ggtitle("Autocorreation of Newcastle coal prices")
ggPacf(coal_train) + ggtitle("Partial autocorreation of Newcastle coal prices")

# Assuming lag not higher than 5 by ACF/PACF

a <- rep(0, times = 36)
A <- matrix(a, nrow = 6)
maxim = 3000

b <- rep(0, times = 36)
B <- matrix(b, nrow = 6)
maxim_2 = 3000
for (i in 1:6) {
  for (j in 1:6) {
    model <-  Arima(coal_train, order = c(i-1,0,j-1), include.drift = FALSE)
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

# Let's estimate three models. Best by AICc, best by BIC and auto-arima model.

best_hand_1 = Arima(coal_train, order = c(2,0,3), include.drift = FALSE)

summary(best_hand_1)

best_hand_2 = Arima(coal_train, order = c(1,0,0), include.drift = FALSE)
summary(best_hand_2)

best_auto = auto.arima(coal_train)
summary(best_auto)

# Count metrics

scores = rbind(accuracy(best_hand_1), accuracy(best_hand_2), accuracy(best_auto))
rownames(scores) = c("ARIMA(2,0,3)", "ARIMA(1,0,0)", "AUTO_ARIMA(0,0,1)")

scores

# Best model by most of in-sample scores - "ARIMA(2,0,3)"

out_scores = rbind(accuracy(forecast(best_hand_1, h = 12), coal_test)[2,],
accuracy(forecast(best_hand_2, h = 12), coal_test)[2,],
accuracy(forecast(best_auto, h = 12), coal_test)[2,])

row.names(out_scores) = c("ARIMA(2,0,3)", "ARIMA(1,0,0)", "AUTO_ARIMA(0,0,1)")
out_scores

# Best model by most of out_of_sample scores - "ARIMA(0,0,1)"

# Plotting forecasts

g_c1 = ggplot(data = tail(coal, 50)) + geom_line(aes(y = P_COAL_FOB_NEWCASTLE, x = tail(dd$DATE, 50))) + 
  geom_line(data = data.frame(forecast(best_hand_1, h = 12)), aes(y = Point.Forecast, x = tail(dd$DATE, 12), colour = "ARIMA(2,0,3)")) +
  geom_line(data = data.frame(forecast(best_hand_2, h = 12)), aes(y = Point.Forecast, x = tail(dd$DATE, 12), color="ARIMA(1,0,0)")) +
  geom_line(data = data.frame(forecast(best_auto, h = 12)), aes(y = Point.Forecast, x = tail(dd$DATE, 12), color = "AUTO_ARIMA(0,0,1)"))+
  scale_colour_manual("", 
                      values = c("ARIMA(2,0,3)"="green", "ARIMA(1,0,0)"="red", 
                                 "AUTO_ARIMA(0,0,1)"="blue")) +
  ggtitle("ARIMA forecasts") + xlab("Date") + ylab("Coal spot price in Newcastle")

  
plot(g_c1)


# Let's make the same provedure for exogeneous variables as for coal. Choose integration order and make ts stationary.
dd$BRENT = inter(dd$BRENT)
dd$FX_USD_CNY = inter(dd$FX_USD_CNY)

BR_names <- colnames(ddts)[substr(colnames(ddts), 1, 2) == "BR"]
FX_names <- colnames(ddts)[substr(colnames(ddts), 1, 10) == "FX_USD_CNY"]

# Exogeneous variables
ggplot(data = ddts[,c(BR_names, FX_names)]) + geom_line(aes(y = BRENT, x = tail(dd$DATE, nrow(ddts))), color="coral") + xlab("Date") + ylab("Price")+
  ggtitle("Brent price")
ggplot(data = ddts[,c(BR_names, FX_names)]) + geom_line(aes(y = FX_USD_CNY, x = tail(dd$DATE, nrow(ddts))), color="steelblue")+ xlab("Date") + ylab("Rate")+
  ggtitle("CNY to USD exchange rate")

# Exogeneous variables, first-difference
ggplot(data = diff(ddts[,BR_names])) + geom_line(aes(y = BRENT, x = tail(dd$DATE, nrow(diff(ddts)))), color="steelblue") + xlab("Date") + ylab("Price")+
  ggtitle("First-diference Brent price")
ggplot(data = diff(ddts[,FX_names])) + geom_line(aes(y = FX_USD_CNY, x = tail(dd$DATE, nrow(diff(ddts)))), color="steelblue")+ xlab("Date") + ylab("Rate")+
  ggtitle("First-difference CNY to USD exchange rate")

# Exogeneous variables, second-difference
ggplot(data = diff(ddts[,BR_names], differences = 2)) + geom_line(aes(y = BRENT, x = tail(dd$DATE, nrow(diff(ddts,differences = 2)))), color="steelblue") + xlab("Date") + ylab("Price")+
  ggtitle("Second-diference Brent price")
ggplot(data = diff(ddts[,FX_names], differences = 2)) + geom_line(aes(y = FX_USD_CNY, x = tail(dd$DATE, nrow(diff(ddts,differences = 2)))), color="steelblue")+ xlab("Date") + ylab("Rate")+
  ggtitle("Second-difference CNY to USD exchange rate")



diff_1_BR = na.omit(diff(ddts[,BR_names]))

diff_2_BR = na.omit(diff(ddts[,BR_names],differences = 2))

diff_1_FX = na.omit(diff(ddts[,FX_names]))

diff_2_FX = na.omit(diff(ddts[,FX_names],differences = 2))

# Unit-root tests for Brent price

urtest_BR <- ur.df(ddts$BRENT, lags = 7, type = 'trend')
summary(urtest_BR)

u1 = ur.df(ddts$BRENT, type = 'trend', lags = 8, selectlags = "BIC")
u2 = ur.df(diff_1_BR$BRENT, type = 'trend', lags = 8, selectlags = "BIC")
u3 = ur.df(diff_2_BR$BRENT, type = 'trend', lags = 8, selectlags = "BIC")

summary(u1)
summary(u2)
summary(u3)

kpss1_BR <- ur.kpss(ddts$BRENT, type = "mu",
                    lags = "long")

kpss2_BR <- ur.kpss(diff_1_BR$BRENT, type = "mu",
                    lags = "long")

kpss3_BR <- ur.kpss(diff_2_BR$BRENT, type = "mu",
                    lags = "long")

summary(kpss1_BR)
summary(kpss2_BR)
summary(kpss3_BR)


acf(diff_1_BR$BRENT)
pacf(diff_1_BR$BRENT)

# Unit-root tests for Exchange rate

urtest_FX <- ur.df(ddts$FX_USD_CNY, lags = 7, type = 'trend')
summary(urtest_FX)

u1_FX = ur.df(ddts$FX_USD_CNY, type = 'trend', lags = 8, selectlags = "BIC")
u2_FX = ur.df(diff_1_FX$FX_USD_CNY, type = 'trend', lags = 8, selectlags = "BIC")
u3_FX = ur.df(diff_2_FX$FX_USD_CNY, type = 'trend', lags = 8, selectlags = "BIC")

summary(u1_FX)
summary(u2_FX)
summary(u3_FX)

kpss1_FX <- ur.kpss(ddts$FX_USD_CNY, type = "mu",
                    lags = "long")

kpss2_FX <- ur.kpss(diff_1_FX$FX_USD_CNY, type = "mu",
                    lags = "long")

kpss3_FX <- ur.kpss(diff_2_FX$FX_USD_CNY, type = "mu",
                    lags = "long")
summary(kpss1_FX)
summary(kpss2_FX)
summary(kpss3_FX)


# Make dataset of 4 variables in consistance with the order of integration

data = diff_1[, YNames]
data$FX_USD_CNY = diff_1_FX[, FX_names]
data$BRENT = diff_1_BR[, BR_names]


# VAR forecasting

# Again, splitting data on train and test

data = ts(data.frame(data),frequency = 4)
train = head(data, nrow(data)-12)
test = tail(data,12)


library(fpp)
library(vars)
#Lag selection

VARselect(train, lag.max = 12, type = "const")$criteria

VARselect(train, lag.max = 12, type = "const")$selection

#Resulting lags : 1, 2, 11

var1 <- VAR(train, p = 1, type = "const")
summary(var1)

serial.test(var1, lags.pt = 10, type = "PT.asymptotic")
serial.test(var1, lags.pt = 10, type = "PT.adjusted")
serial.test(var1, lags.pt = 10, type = "BG")

var2 <- VAR(train, p = 2, type = "const")
serial.test(var2, lags.pt = 10, type = "PT.asymptotic")
serial.test(var2, lags.pt = 10, type = "BG")

var11 <- VAR(train, p = 11, type = "const")
serial.test(var11, lags.pt = 20, type = "PT.asymptotic")
serial.test(var11, lags.pt = 20, type = "BG")

#Check resuduals normality
normality.test(var1, multivariate.only = FALSE)
normality.test(var2, multivariate.only = FALSE)
normality.test(var11, multivariate.only = FALSE)

# Check causality
causality(var1, 'BRENT')
causality(var1, 'FX_USD_CNY')

causality(var2, 'BRENT')
causality(var2, 'FX_USD_CNY')

causality(var11, 'BRENT')
causality(var11, 'FX_USD_CNY')


# Let's estimate forecasts

# Out-of-sample metrics

var_fcst1 <- forecast(var1, h = 12)
var_fcst2 <- forecast(var2, h = 12)
var_fcst11 <- forecast(var11, h = 12)

acc_1 = accuracy(var_fcst1, x = test, D = 0, d = 0)
acc_2 = accuracy(var_fcst2, x = test, D = 0, d = 0)
acc_11 = accuracy(var_fcst11, x = test, D = 0, d = 0)

# Filtering just out-of-sample metrics

acc_1 = acc_1[endsWith(row.names(acc_1), "Test set"), ]
acc_2 = acc_2[endsWith(row.names(acc_2), "Test set"), ]
acc_11 = acc_11[endsWith(row.names(acc_11), "Test set"), ]

row.names(acc_1) = stringr::str_replace(row.names(acc_1), "\\Test set$",  "VAR(1)")
row.names(acc_2) = stringr::str_replace(row.names(acc_2), "\\Test set$",  "VAR(2)")
row.names(acc_11) = stringr::str_replace(row.names(acc_11), "\\Test set$",  "VAR(11)")


all_acc = rbind(acc_1, acc_2, acc_11)
# Grouping by time series and models. One table per ts

acc_newcastle = all_acc[startsWith(row.names(all_acc), "P_COAL_FOB_NEWCASTLE"), ]
acc_rotterdam = all_acc[startsWith(row.names(all_acc), "P_COAL_API2_NWEUR"), ]
acc_brent = all_acc[startsWith(row.names(all_acc), "BRENT"), ]
acc_rate = all_acc[startsWith(row.names(all_acc), "FX_USD_CNY"), ]
print(rbind(acc_newcastle, acc_rotterdam, acc_brent, acc_rate))

# Plotting forecasts


var_fcst1 <- var_fcst1$forecast
var_fcst2 <- var_fcst2$forecast
var_fcst11 <- var_fcst11$forecast

newcastle = ggplot(data = data.frame(tail(data,50))) + geom_line(aes(y = P_COAL_FOB_NEWCASTLE, x = tail(dd$DATE, 50))) + 
  geom_line(data = data.frame(var_fcst1$P_COAL_FOB_NEWCASTLE), aes(y = Point.Forecast, x = tail(dd$DATE, 12), colour = "VAR(1)")) +
  geom_line(data = data.frame(var_fcst2$P_COAL_FOB_NEWCASTLE), aes(y = Point.Forecast, x = tail(dd$DATE, 12), color="VAR(2)")) +
  geom_line(data = data.frame(var_fcst11$P_COAL_FOB_NEWCASTLE), aes(y = Point.Forecast, x = tail(dd$DATE, 12), color = "VAR(11)"))+
  scale_colour_manual("", 
                      values = c("VAR(1)"="green", "VAR(2)"="red", 
                                 "VAR(11)"="blue")) +
  ggtitle("VAR forecasts, coal spot price, Newcastle") + xlab("Date") + ylab("Price, first-diff")

plot(newcastle)




rotterdam = ggplot(data = data.frame(tail(data,50))) + geom_line(aes(y = P_COAL_API2_NWEUR, x = tail(dd$DATE, 50))) + 
  geom_line(data = data.frame(var_fcst1$P_COAL_API2_NWEUR), aes(y = Point.Forecast, x = tail(dd$DATE, 12), colour = "VAR(1)")) +
  geom_line(data = data.frame(var_fcst2$P_COAL_API2_NWEUR), aes(y = Point.Forecast, x = tail(dd$DATE, 12), color="VAR(2)")) +
  geom_line(data = data.frame(var_fcst11$P_COAL_API2_NWEUR), aes(y = Point.Forecast, x = tail(dd$DATE, 12), color = "VAR(11)"))+
  scale_colour_manual("", 
                      values = c("VAR(1)"="green", "VAR(2)"="red", 
                                 "VAR(11)"="blue")) +
  ggtitle("VAR forecasts, coal spot price, Rotterdam") + xlab("Date") + ylab("Price, first-diff")

plot(rotterdam)

brent = ggplot(data = data.frame(tail(data,50))) + geom_line(aes(y = BRENT, x = tail(dd$DATE, 50))) + 
  geom_line(data = data.frame(var_fcst1$BRENT), aes(y = Point.Forecast, x = tail(dd$DATE, 12), colour = "VAR(1)")) +
  geom_line(data = data.frame(var_fcst2$BRENT), aes(y = Point.Forecast, x = tail(dd$DATE, 12), color="VAR(2)")) +
  geom_line(data = data.frame(var_fcst11$BRENT), aes(y = Point.Forecast, x = tail(dd$DATE, 12), color = "VAR(11)"))+
  scale_colour_manual("", 
                      values = c("VAR(1)"="green", "VAR(2)"="red", 
                                 "VAR(11)"="blue")) +
  ggtitle("VAR forecasts, Brent price") + xlab("Date") + ylab("price, first-diff")

plot(brent)

rate = ggplot(data = data.frame(tail(data,50))) + geom_line(aes(y = FX_USD_CNY, x = tail(dd$DATE, 50))) + 
  geom_line(data = data.frame(var_fcst1$FX_USD_CNY), aes(y = Point.Forecast, x = tail(dd$DATE, 12), colour = "VAR(1)")) +
  geom_line(data = data.frame(var_fcst2$FX_USD_CNY), aes(y = Point.Forecast, x = tail(dd$DATE, 12), color="VAR(2)")) +
  geom_line(data = data.frame(var_fcst11$FX_USD_CNY), aes(y = Point.Forecast, x = tail(dd$DATE, 12), color = "VAR(11)"))+
  scale_colour_manual("", 
                      values = c("VAR(1)"="green", "VAR(2)"="red", 
                                 "VAR(11)"="blue")) +
  ggtitle("VAR forecasts, USD to CNY rate") + xlab("Date") + ylab("Rate, first-diff")

plot(rate)

grid.arrange(newcastle, rotterdam, rate, brent) 


# SVAR

amat = diag(4)
diag(amat) = NA
amat[1,3] = NA
amat[1,4] = NA
amat[2,3] = NA
amat[2,4] = NA
# amat[3,4] = NA
amat[4,3] = NA
amat

svar_coal = SVAR(var11, Amat = amat)
svar_coal[["A"]]

#Impulse response
irf1 <- irf(svar_coal, impulse = "BRENT",response = c( "P_COAL_FOB_NEWCASTLE", "P_COAL_API2_NWEUR"), n.ahead = 12, cumulative = TRUE)
irf2 <- irf(svar_coal, impulse = "FX_USD_CNY", response = c( "P_COAL_FOB_NEWCASTLE", "P_COAL_API2_NWEUR"), n.ahead = 12, cumulative = TRUE)
plot(irf1)
plot(irf2)
#Forward error variance decomposition
fevd(svar_coal, n.ahead = 12)