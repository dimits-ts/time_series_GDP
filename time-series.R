library(readxl)
library(nortest)
library(car)

data <- read_excel("EULN1NOR.xlsx", skip=1)
data

# we are dealing with GDP data, therefore we would like to use their log
gdp <- ts(data$`RGDP NOR`, frequency = 4, start=c(2001,1))[-(0:1)]
gdp

# review RGDP by itself
plot(gdp, type="l", col='red', lwd=1, 
      main="Norwegian GDP % change by year", 
      ylab="% GDP change in real terms")
hist(gdp, nclass=50, main="Histogram of Norway's GDP % Growth")

# test for normality
shapiro.test(gdp)
lillie.test(gdp)
qqnorm(gdp)
qqline(gdp)


# ===== BOX JENKINS =====

acf(gdp, 48, main="ACF of Norwegian RGDP % Growth") #ma(1)
pacf(gdp, 48, main="PACF of Norwegian RGDP % Growth") # ar(1,2)

# ===== MA(1) =====
ma1 = arima(gdp, order=c(0,0,1))
ma1 # all ok

# evaluate
ma1res = ts(ma1$residuals, frequency = 4, start=c(2001,1))
acf(ma1res, 48, main="ACF of residuals")
pacf(ma1res, 48, main="PACF of residuals")

shapiro.test(ma1res)
lillie.test(ma1res)
qqnorm(ma1res)
qqline(ma1res)

acf(ts(ma1res^2), 48, main="ACF of squared residuals")
pacf(ts(ma1res^2), 48, main="PACF of squared residuals")


# ===== MA({1,4}) =====
ma14 = arima(gdp, order=c(0, 0, 4), fixed=c(NA,0,0,NA,NA))
ma14 # ar1 probs not, ma1 ok, intercept ok

# evaluate
ma14res = ts(ma14$residuals, frequency = 4, start=c(2001,1))
acf(ma14res, 48, main="ACF of residuals")
pacf(ma14res, 48, main="PACF of residuals")
shapiro.test(ma14res)
lillie.test(ma14res)
qqnorm(ma14res)
qqline(ma14res)

acf(ts(ma14res^2,), 48, main="ACF of squared residuals")
pacf(ts(ma14res^2), 48, main="PACF of squared residuals")


# ===== ARΜΑ(1,1) =====
ar1ma1 = arima(gdp, order=c(1, 0, 1))
ar1ma1 # ar1 probs not, ma1 ok, intercept ok

# evaluate
ar1ma1res = ts(ar1ma1$residuals, frequency = 4, start=c(2001,1))
acf(ar1ma1res, 48, main="ACF of residuals")
pacf(ar1ma1res, 48, main="PACF of residuals")
shapiro.test(ar1ma1res)
lillie.test(ar1ma1res)
qqnorm(ar1ma1res)
qqline(ar1ma1res)

acf(ts(ar1ma1res^2,), 48, main="ACF of squared residuals")
pacf(ts(ar1ma1res^2), 48, main="PACF of squared residuals")

# We choose MA(1)


# ===== SIMPLE OLS MODEL ===== 
lm_data = data[-(0:9),]
model <- lm(`RGDP NOR` ~ .-`...1`, data=lm_data)
summary(model)

# evaluate hypotheses
model_res = model$residuals
acf(model_res, 48, main="ACF of residuals")
pacf(model_res, 48, main="PACF of residuals")
shapiro.test(model_res)
lillie.test(model_res)
qqnorm(model_res)
qqline(model_res)

# check for multicolinearity
# this will notify us of variables that most likely need to be removed
# after we fit our mixed model
vif(model)


# ===== MIXED ARIMA MODEL =====

# create mixed ARIMA model
lm_ma1 = arima(model_res, order=c(0,0,1), include.mean=F)
lm_ma1

acf(lm_ma1$residuals, 48, main="ACF of residuals")
pacf(lm_ma1$residuals, 48, main="PACF of residuals")


mixed_model = arima(lm_data$`RGDP NOR`, order=c(0,0,1), include.mean=F, 
               xreg=cbind(lm_data[,-(0:2)]))
mixed_model

# check if stationarity, normality, heteroscedasticity indicators are normal 
mixed_model_res = mixed_model$residuals
acf(ts(mixed_model_res), 48, main="ACF of residuals")
pacf(ts(mixed_model_res), 48, main="PACF of residuals")
shapiro.test(mixed_model_res)
lillie.test(mixed_model_res)
qqnorm(mixed_model_res)
qqline(mixed_model_res)

acf(ts(mixed_model_res^2,), 48, main="ACF of squared residuals")
pacf(ts(mixed_model_res^2), 48, main="PACF of squared residuals")               

# TODO: check whether other tests are applicable


# ===== MANUAL PARAMETER SELECTION FOR MULTIPLE REGRESSION MA(1) ===== 

m0 = arima(lm_data$`RGDP NOR`, order=c(0,0,1), 
           xreg=cbind(lm_data[,-(0:2)]))
m0

m1 = arima(lm_data$`RGDP NOR`, order=c(0,0,1), 
           xreg=cbind(lm_data[,c(4,5,6,7,8,9,10,11,14,15)]))
m1

m2 = arima(lm_data$`RGDP NOR`, order=c(0,0,1), 
           xreg=cbind(lm_data[,c(4,6,7,9,10,11,15)]))
m2

m3 = arima(lm_data$`RGDP NOR`, order=c(0,0,1), 
                    xreg=cbind(lm_data[,c(6,7,9,10,11,15)]))
m3

m4 = arima(lm_data$`RGDP NOR`, order=c(0,0,1), 
           xreg=cbind(lm_data[,c(6,10,14,15)]))
m4 

m5 = arima(lm_data$`RGDP NOR`, order=c(0,0,1), 
           xreg=cbind(lm_data[,c(6,10,14)]))
m5

# choose m4 based on loglikelihood, AIC
final_model = m4




