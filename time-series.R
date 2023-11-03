library(readxl)
library(nortest)
library(car)
library(stargazer)


# ===== HELPER FUNCTIONS FOR SAVING PLOTS AND TABLES =====

RESOURCE_PATH = "resources"

# Utility function to get a relative file path (including file extension)
# from a file name.
filepath_png <- function(name) {
  return(file.path(RESOURCE_PATH, paste(name, ".png", sep = "")))
}

# Utility function to save a plot to the disk.
my_save_plot <- function(name, plot_func, ...) {
  # display plot
  plot_func(...)
  # save plot
  filepath = filepath_png(name)
  png(filepath)
  plot_func(...)
  dev.off() # COMMENT THIS LINE TO PREVENT PLOT SAVING
}


# ===== ARIMA MODELS =====

data <- read_excel("EULN1NOR.xlsx", skip=1)
data

gdp <- ts(data$`RGDP NOR`[-(0:1)], frequency = 4, start=c(2001,1))
gdp

# review RGDP by itself
my_save_plot("gdp", plot, gdp, type="l", col='red', lwd=1, 
      main="Norwegian GDP % change by year", 
      ylab="% GDP change in real terms")
hist(gdp, nclass=50, main="Histogram of Norway's GDP % Growth")

# test for normality
shapiro.test(gdp)
lillie.test(gdp)
qqnorm(gdp)
qqline(gdp)


# ===== BOX JENKINS =====

acf(gdp, 24, main="ACF of Norwegian RGDP % Growth") #ma(1)
pacf(gdp, 24, main="PACF of Norwegian RGDP % Growth") # ar(1,2)

# ===== MA(1) =====
ma1 = arima(gdp, order=c(0,0,1))
ma1 # all ok

# evaluate
ma1res = ts(ma1$residuals, frequency = 4, start=c(2001,1))
acf(ma1res, 24, main="ACF of residuals")
pacf(ma1res, 24, main="PACF of residuals")

shapiro.test(ma1res)
lillie.test(ma1res)
qqnorm(ma1res)
qqline(ma1res)

acf(ts(ma1res^2), 24, main="ACF of squared residuals")
pacf(ts(ma1res^2), 24, main="PACF of squared residuals")

Box.test(ma1res,lag=24,type="Ljung")
Box.test(ma1res^2,lag=24,type="Ljung")

# ===== MA({1,4}) =====
ma14 = arima(gdp, order=c(0, 0, 4), fixed=c(NA,0,0,NA,NA))
ma14 # ma1 significant, ma4 not significant, intercept ok

# evaluate
ma14res = ts(ma14$residuals, frequency = 4, start=c(2001,1))
acf(ma14res, 48, main="ACF of residuals")
pacf(ma14res, 48, main="PACF of residuals")
shapiro.test(ma14res)
lillie.test(ma14res)
qqnorm(ma14res)
qqline(ma14res)

acf(ts(ma14res^2), 48, main="ACF of squared residuals")
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

acf(ts(ar1ma1res^2,), 48, main="ACF of squared residuals")
pacf(ts(ar1ma1res^2), 48, main="PACF of squared residuals")

Box.test(model_res,lag=24,type="Ljung")
Box.test(model_res^2,lag=24,type="Ljung")


# ===== MANUAL PARAMETER SELECTION FOR MULTIPLE REGRESSION ===== 


# try including a MA1 term since it proved significant previously
m0 = arima(lm_data$`RGDP NOR`, order=c(0,0,1), 
           xreg=cbind(lm_data[,-(0:2)]))
m0

# MA(1) significant

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

# MA(1) term stops being significant whereas AIC keeps improving

m5 = arima(lm_data$`RGDP NOR`, order=c(0,0,1), 
           xreg=cbind(lm_data[,c(6,7,9,10,11)]))
m5

m6 = arima(lm_data$`RGDP NOR`, order=c(0,0,1), 
           xreg=cbind(lm_data[,c(7,9,10,11)]))
m6

m7 = arima(lm_data$`RGDP NOR`, order=c(0,0,1), 
           xreg=cbind(lm_data[,c(3,7,9,10,11)]))
m7

# Discard MA(1) term 

m8 = lm(`RGDP NOR` ~ `LAG1 NOR` + `LAG4 NOR` + `RPROD NOR` + `DPPI NOR` + 1, data = lm_data)
summary(m8)
AIC(m8)

m9 = lm(`RGDP NOR` ~ `RPROD NOR` + LEADNOR + 1, data = lm_data)
summary(m9)
AIC(m9)

# also try automatic selection
fullModel = model
nullModel = lm(`RGDP NOR` ~ 1, data = lm_data) 
step_model = step(
  model,
  direction = 'both', 
  scope = list(upper = fullModel, 
               lower = nullModel), 
  trace = 0, # do not show the step-by-step process of model selection
  k=1) #choose by AIC as we want the best predictive, not explanatory model 

summary(step_model)
AIC(step_model)

# choose model with lowest AIC
final_model = m9
summary(final_model)
AIC(final_model)

# evaluate hypotheses on final model
model_res = final_model$residuals
acf(model_res, 48, main="ACF of residuals")
pacf(model_res, 48, main="PACF of residuals")

shapiro.test(model_res)
lillie.test(model_res)
qqnorm(model_res)
qqline(model_res)

# check for multicolinearity
vif(model)

# volatility through time
acf(ts(ar1ma1res^2), 48, main="ACF of squared residuals")
pacf(ts(ar1ma1res^2), 48, main="PACF of squared residuals")

Box.test(model_res,lag=24,type="Ljung")
Box.test(model_res^2,lag=24,type="Ljung")

# check linearity
plot(final_model, 1)

stargazer(final_model, 
          type="latex", 
          title="Linear regression model predicting real GDP Growth (\\%).",
          ci=T, 
          label="tab::gdp_model",
          df=T,
          out=paste(RESOURCE_PATH, "/gdp_model.tex", sep=""), 
          report=('vc*p'),
          add.lines=list(c("AIC", round(AIC(final_model),1))),
          no.space=TRUE)


# ===== FORECTASTING STEP ===== 

# get the data for the last 12 quarters and assume the same pattern
xreg_forecast = cbind(lm_data[-(1:70), c(6,10,14,15)])
forecast = predict(final_model, n.ahead=12, newxreg=xreg_forecast)   
forecast_ts = ts(forecast$pred, frequency=4, start=c(2023, 3))   

UL = ts(forecast$pred+forecast$se, frequency=4, start=c(2023, 3))
LL = ts(forecast$pred-forecast$se, frequency=4, start=c(2023, 3))

# plot of forecasts with 1 s.e.
minx = min(gdp, LL)
maxx = max(gdp, UL) 

ts.plot(gdp, forecast_ts, ylim=c(minx,maxx))
abline(v=2023.25, col="purple")
lines(UL, col="blue", lty="dashed") 
lines(LL, col="red", lty="dashed")
