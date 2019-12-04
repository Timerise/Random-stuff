############
## E.G. Timerise
## Random time series modeling code
## Noting of any great importance , just wanted to store some functions
#######################################################################
#######################################################################
setwd("C:/Users/manem/Desktop/")
getwd()
###
# some useful packages
library('aTSA')
library('forecast')
library("astsa")
library('rio')
library('tseries')
library('tidyverse')
#################
beast<-read.csv("dragon.csv")
##################

#########################
plot(diff(log(beast$number)))
plot(ts(diff(log(beast$number))))

acf(diff(log(beast$number)),100)
# MA 1 maybe 2
pacf(diff(log(beast$number)),100)
# AR 3
#library('aTSA')

adf.test(diff(log(beast$number)))
# small p value indicates strong evidence against the null
# for this test..
# ...the null is that its Not statioary
# taking the log and diff will cause it to have a strong evidence for stationary 
# , plus it will make our number range readable

# When converting the predicted values back recall 
# pred apprended to known, the diff(rev())
# also , log base 10 converted back is ..
# base rasied to y  ... aka  diff(rev(10^(valueset )))
# , where value set is predicted attached to known actual observatio list


fit<-arima(ts(diff(log(beast$number))),order=c(3,0,1))
### *** notice that I already diff data once, thus it seems that its not needed 
### to place within the code so its listed as 0=d , if I removed dif(), then I would
## put a 1=d
## noticed this after reviewing method and observations
summary(fit)
## AIC of 302.46, 
## Root mean square error 2.1
## Mean abslote error 1.726

tsdisplay(residuals(fit),lag.max=80,main='(AR 3,d 1,MA 1) model residuals')
# the autocorrelation of the residuals seems a bit high
# lets check out the auto version
#################################################

#####################################################
Box.test(resid(fit),lag=2,type="Ljung")

# large p value indicates weak evidence against the null hypothesis
# further test are needed to reject the null hpyothesis
# for the Box-Ljung test, the null hypothesis is that we have a good fit 
# are the residuals iid( indpendently identically distributed) )
############################
############################
pred<-predict(fit,n.ahead=3)

pred$pred



ts.plot(ts(diff(log(beast$number))),pred$pred,lty=c(1,3))


##########################


###############
###############
###############

###########################
#####################################################
#library("astsa")
sarima(ts(diff(log(beast$number))),3,0,1,0,0,0)
sarima(ts(diff(log(beast$number))),2,0,2,0,0,0)
# AIC 297 
sarima(ts(log(beast$number)),2,1,2,0,0,0)

########################
########################
#library('forecast')

fit2<-arima(ts(diff(log(beast$number))),order=c(2,0,2))
summary(fit2)
# aic =297.09 
# variance= 3.838
# root mean square error 1.959051
# Mean ablsote error 1.61
tsdisplay(residuals(fit2),lag.max=80)
summary(fit)
# aic=302.94
# variance= 4.507
# root mean sqaure error 2.123082
# mean ablsote error 1.72
###############################################
# Going to go with model  ARIMA(2,1,2) for stock data
pred2<-predict(fit2,n.ahead=3)

pred2$pred
ts.plot(ts(diff(log(beast$number))),pred2$pred,lty=c(1,3))
tim_forcas<-forecast(fit2,h=3)
plot(tim_forcas)

ko<-forecast(fit,h=3)
plot(ko)
########################
#######################
pred$pred[1]
pred2$pred[1]
### Important!!!!!
###################################################
###################################################
## Reversing diff and box cox transformation "log"
## Thus you can see the actual mean predicted response , or just predicted value
## all based in your orignal stock data


x=diff(log(beast$number))

x=append(x,pred$pred[1:3],after=length(x))
exp(1)^(diffinv(x,xi=log("lost value")))

u=diff(log(beast$number))
u=append(u,pred2$pred[1:3],after=length(u))
exp(1)^(diffinv(u,xi=log("lost value")))
## I only used the sarima model functions because I liked the quick visual outputs
## I  based my  decisions on the arima functions/codes

pred$pred[1:3]
