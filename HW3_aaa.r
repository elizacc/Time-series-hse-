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
setwd("/Users/Archaedas/Desktop/Time-series")
dk <- read.csv("econ_times_series_new.csv")
head(dk)

df <- subset(dk, select = c("index", "SBER", "X1year_rate", "exchange_rate"))

df$index <- as.Date(paste(df$index), format = "%d.%m.%Y")
df$SBER <- as.double(gsub(",", ".", df$SBER))
df$X1year_rate <- as.double(gsub(",", ".", df$X1year_rate))
df$exchange_rate <- as.double(gsub(",", ".", df$exchange_rate))

# Time-series data
dfts <- xts(df[, 2:ncol(df)], order.by = df$index, frequency = 1)
dfts$date = df$date
head(dfts)

dfts$exchange_rate_BC = BoxCox(dfts[, "exchange_rate"], lambda = -1/2)
diff_1 = na.omit(diff(dfts))
diff_2 = na.omit(diff(dfts,differences = 2))

df_var <- data.frame("SBER_1diff" = diff_1[, "SBER"],
                     "1year_rate_1diff" = diff_1[, "X1year_rate"],
                     "exchange_rate_BC_1diff" = diff_1[, "exchange_rate_BC"])

data = ts(df_var,frequency = 4)

# Simple VAR
var_1 <- VAR(data, p = 1, type = "both")

#Matriсes for SVAR
amat1 <- diag(3)
amat1
diag(amat1) <- NA
amat1[1,3] = NA
amat1[2,3] = NA
amat1

bmat <- diag(3)
diag(bmat) = NA
bmat

svar_st_ir_fx <- SVAR(var_1, Amat = amat1, Bmat = bmat, max.iter = 3000)

irf_SBER_X1 <- irf(svar_st_ir_fx, impulse = "X1year_rate", response = c("SBER"), n.ahead = 10, cumulative = FALSE)
plot(irf_SBER_X1)
irf_SBER_exch <- irf(svar_st_ir_fx, impulse = "exchange_rate_BC", response = c("SBER"), n.ahead = 10, cumulative = FALSE)
plot(irf_SBER_exch)

fevd(svar_st_ir_fx, n.ahead = 10)

# Reordering

df_2 <- subset(dk, select = c("index", "SBER", "exchange_rate", "X1year_rate"))

df_2$index <- as.Date(paste(df_2$index), format = "%d.%m.%Y")
df_2$SBER <- as.double(gsub(",", ".", df_2$SBER))
df_2$X1year_rate <- as.double(gsub(",", ".", df_2$X1year_rate))
df_2$exchange_rate <- as.double(gsub(",", ".", df_2$exchange_rate))

dfts_2 <- xts(df_2[, 2:ncol(df)], order.by = df_2$index, frequency = 1)
dfts_2$date = df_2$date
head(dfts_2)

dfts_2$exchange_rate_BC = BoxCox(dfts[, "exchange_rate"], lambda = -1/2)
diff_3 = na.omit(diff(dfts_2))
diff_4 = na.omit(diff(dfts_2,differences = 2))

df_var_2 <- data.frame("SBER_1diff" = diff_3[, "SBER"],
                     "1year_rate_1diff" = diff_3[, "X1year_rate"],
                     "exchange_rate_BC_1diff" = diff_3[, "exchange_rate_BC"])

data_2 = ts(df_var_2,frequency = 4)

var_2 <- VAR(data_2, p = 1, type = "both")

# Reorder the assumptions in new matrix

amat2 <- diag(3)
amat2
diag(amat2) <- NA
amat2[1,2] = NA
amat2[2,3] = NA
amat2

bmat2 <- diag(3)
diag(bmat2) = NA
bmat2

svar_st_ir_fx_2 <- SVAR(var_2, Amat = amat2, Bmat = bmat2, max.iter = 3000)

irf_SBER_X1_2 <- irf(svar_st_ir_fx_2, impulse = "X1year_rate", response = c("SBER"), n.ahead = 10)
plot(irf_SBER_X1_2)
irf_SBER_exch_2 <- irf(svar_st_ir_fx_2, impulse = "exchange_rate_BC", response = c("SBER"), n.ahead = 10)
plot(irf_SBER_exch_2)

fevd(svar_st_ir_fx_2, n.ahead = 10)