# Case Study on Apple & Microsoft
# Comprehensive Analysis, from a technical and quantitative view
# Prof. Enrique Ascordebeitia, 2018 


library(moments)
library(quantmod)

DataFBOK34.SA = read.csv(file.choose())
DataTWTR = read.csv(file.choose())
class(DataFBOK34.SA)
class(DataTWTR)


## Quick look of data
head(DataFBOK34.SA)
summary(DataTWTR)

plot(DataFBOK34.SA$Adj.Close,type="l",col=2,ylim=c(0,130),xlab="Days",ylab="Price",las=1,main="Stock Quotes - FBOK34.SA & TWTR")
par(new=T)
plot(DataTWTR$Adj.Close,type="l",col=3,ylim=c(0,130),xlab="Days",ylab="Price",las=1)

# Technical Analysis
# 
# getSymbols("FBOK34.SA")
# candleChart(FBOK34.SA,multi.col=TRUE,theme='white') 
# addRSI(n=14,maType = "EMA")
# addMACD(fast = 12, slow = 26, signal = 9, type = "EMA")

## Returns

ret_FBOK34.SA =0
ret_TWTR= 0

for (i in 2:length(DataFBOK34.SA$Adj.Close)){
  ret_FBOK34.SA[i-1] = log(DataFBOK34.SA$Adj.Close[i]/DataFBOK34.SA$Adj.Close[i-1])
}

for (i in 2:length(DataTWTR$Adj.Close)){
  ret_TWTR[i-1] = log(DataTWTR$Adj.Close[i]/DataTWTR$Adj.Close[i-1])
}

plot(ret_FBOK34.SA,type="l",col=2,ylim=c(-0.2,0.2),xlab="Days",ylab="RET",las=1,main="Cont. Daily Returns - FBOK34.SA & TWTR")
par(new=T)
plot(ret_TWTR,type="l",col=3,ylim=c(-0.2,0.2),xlab="Days",ylab="RET",las=1)

# plot(ret_FBOK34.SA,ret_TWTR,col=c(2,3),main="ret_TWTR Vs ret_FBOK34.SA")
# 
# mymodel= lm(ret_TWTR~ret_FBOK34.SA)
# summary(mymodel)
# #See Also "anova.lm" for the ANOVA table. AND The generic functions coef, effects, residuals, fitted, vcov.
# # predict.lm (via predict) for prediction, including confidence and prediction intervals;

## Stats

mu_FBOK34.SA    = mean(ret_FBOK34.SA)
sigma_FBOK34.SA = sd(ret_FBOK34.SA)
skewness_FBOK34.SA = skewness(ret_FBOK34.SA)
kurtosis_FBOK34.SA = kurtosis(ret_FBOK34.SA)

mu_TWTR    = mean(ret_TWTR)
sigma_TWTR = sd(ret_TWTR)
skewness_TWTR = skewness(ret_TWTR)
kurtosis_TWTR = kurtosis(ret_TWTR)

par(mfrow=c(1,2))
hist(ret_FBOK34.SA,col="blue",las=1)
hist(ret_TWTR,col="blue",las=1)

# DO THEY LOOK NORMAL??
# jarque.test(ret_TWTR)
# plot(density(ret_TWTR),xlim=c(-0.15,0.15))

# ANNUALIAZED RISK & RETURN
# --------------------------

mu_FBOK34.SA*250
mu_TWTR*250

sigma_FBOK34.SA*sqrt(250)
sigma_TWTR*sqrt(250)

# Parametric VaRs

VaRp_FBOK34.SA = sigma_FBOK34.SA*-1.65*100
VaRp_TWTR = sigma_TWTR*-1.65*100

# Historical VaRs

VaRh_FBOK34.SA =quantile(ret_FBOK34.SA,0.05)*100
VaRh_TWTR =quantile(ret_TWTR,0.05)*100
