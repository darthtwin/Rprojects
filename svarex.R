library(urca) 
library(vars)
library(mFilter) 
library(tseries) 
library(TSstudio) 
library(forecast) 
library(tidyverse)

macro <- read.csv("~/Documents/Rprojects/svardataex.csv")
head(macro)

y <- ts(macro$output_gap, start = c(1970,1,1), frequency = 4) 
pi <- ts(macro$core_cpi, start = c(1970,1,1), frequency = 4) 
r <- ts(macro$fed_funds_rate, start = c(1970,1,1), frequency = 4)

plot(y, main = "Output Gap", xlab = "Time", ylab = "Output Gap") 
plot(pi, main = "Inflation Rate", xlab = "Time", ylab = "CPI") 
plot(r, main = "Federal Funds Rate", xlab = "Time", ylab = "FFR")

amat <- diag(3)
amat

amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA
amat

svar1 <- cbind(y, pi, r)
colnames(svar1) <- c('OutputGap', 'CPI', 'FFR')
head(svar1)

lagselect <- VARselect(svar1, lag.max = 8, type = "both")
lagselect$selection

varmodel1 <- VAR(svar1, p = 5, season = NULL, exog = NULL, type = "const")
varmodel1

svarmodel1 <- SVAR(varmodel1, Amat = amat, Bmat = NULL, hessian = TRUE, 
              estmethod = c("scoring", "direct"))
svarmodel1

svarog <- irf(svarmodel1, impulse = "OutputGap", response = "OutputGap")
svarog
plot(svarog)

svarinf <- irf(svarmodel1, impulse = "OutputGap", response = "CPI")
svarinf
plot(svarinf)

svarffr <- irf(svarmodel1, impulse = "CPI", response = "FFR")
svarffr
plot(svarffr)
