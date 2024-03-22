## USE FORECAST LIBRARY.

library(forecast)
library(zoo)


# Create data frame.
electricity.data <- read.csv("Electric_Production.csv")

# See the first 6 records of the file.
head(electricity.data)

# See the last 6 records of the file.
tail(electricity.data)

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION.

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
production.ts <- ts(electricity.data$Value, 
                   start = c(1985, 1), end = c(2018, 1), freq = 12)

production.ts

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
Acf(production.ts, lag.max = 12, main = "Autocorrelation for Amtrak production")


production.stl <- stl(production.ts, s.window = "periodic")
plot(production.ts, 
     xlab = "Period", ylab = "Production(kwh)", 
     ylim = c(50, 200), xaxt = 'n',
     main = "Electricity Production")
axis(1, at = seq(1985, 2020, 1), labels = format(seq(1985, 2020, 1)))

autoplot(production.stl, main = "Electricity Production Time Series Components.")


# identify autocorrelation and plot autocorrelation for different lags.

autocor <- Acf(production.ts, lag.max = 12, 
               main = "Autocorrelation for Electricity Production")

#Check Predictability of the data

# Use Arima() function to fit AR(1) model 
production.ar1<- Arima(production.ts, order = c(1,0,0))
summary(production.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.8784
s.e. <- 0.0246
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}
#p-value less than 0.05, hence, null hypothesis is rejected. 
#Which means data is predictable.

# Create first differenced data using lag1 to check predictability
diff.production.ts <- diff(production.ts, lag = 1)

# Use Acf() function to identify autocorrealtion for first differenced data, 
#and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(diff.production.ts, lag.max = 12, 
    main = "Autocorrelation for Electricity Production")

#since the coefficients are significant, the data is predictable
length(train.ts)
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 110 
nTrain <- length(production.ts) - nValid
train.ts <- window(production.ts, start = c(1985, 1), end = c(1985, nTrain))
valid.ts <- window(production.ts, start = c(1985, nTrain + 1), 
                   end = c(1985, nTrain + nValid))

#APPLYING FORECASTING METHODS

# Develop two-level forecast by combining regression forecast 
#and trailing MA forecast for residuals.

# Develop Regression model with linear trend and seasonality for training partition
trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)

# Identify and display regression residuals for training
# partition (differences between actual and regression values 
# in the same periods).
trend.seas.res <- trend.seas$residuals
trend.seas.res

# Apply trailing MA for residuals with window width k = 4
# for training partition.
ma.trail.res <- rollmean(trend.seas.res, k = 4, align = "right")
ma.trail.res


## Forecast using regression and trailing MA for validation partition

# Create regression forecast with trend and seasonality for validation period.
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred

# Regression residuals in validation period.
trend.seas.res.valid <- valid.ts - trend.seas.pred$mean
trend.seas.res.valid

# Create residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

##PLOTS
# Plot original data and regression forecast for training and 
# validation partitions.
plot(production.ts, 
     xlab = "Time", ylab = "Production", ylim = c(50, 180), 
     bty = "l", xlim = c(1985, 2022), xaxt = "n",
     main = "Regression Forecast in Training and Validation Partitions ") 
axis(1, at = seq(1985, 2020, 1), labels = format(seq(1985, 2020, 1)))
lines(trend.seas$fitted, col = "blue", lwd = 2, lty = 1)
lines(trend.seas.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1985,180, legend = c("Electricity production", 
                             "Regression Forecast, Training Partition", 
                             "Regression Forecast, Validation Partition"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2009, 2009), c(0, 150))
lines(c(2018, 2018), c(0, 150))
text(1994, 140, "Training")
text(2014, 140, "Validation")
text(2020, 140, "Future")


# Plot residuals and MA residuals forecast in training and validation partitions. 
plot(trend.seas.res, 
     xlab = "Time", ylab = "Production", ylim = c(-30, 30), 
     bty = "l", xlim = c(1985, 2022), xaxt = "n",
     main = "Regression Residuals and Trailing MA for Residuals", 
     col = "brown", lwd =2) 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(trend.seas.res.valid, col = "brown", lwd = 2, lty = 2)
lines(ma.trail.res, col = "blue", lwd = 2, lty = 1)
lines(ma.trail.res.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1983,30, legend = c("Regression Residuals, Training Partition", 
                             "Regression Residuals, Validation Partition",
                             "MA Forecast (k=4), Training Partition", 
                             "MA forecast (k=4), Validation Partition"), 
       col = c("brown", "brown", "blue", "blue"), 
       lty = c(1, 2, 1, 2), lwd =c(2, 2, 2, 2), bty = "n")

lines(c(2009, 2009), c(-40, 30))
lines(c(2018, 2018), c(-40, 30))
text(1994, 8, "Training")
text(2014, 8, "Validation")
text(2020, 8, "Future")

#Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(fst.2level, valid.ts), 3)

##EXPONENTIAL SMOOTHING

## SIMPLE EXPONENTIAL SMOOTHING (SES) WITH PARTITIONED DATA, ALPHA = 0.2.

# Create simple exponential smoothing (SES) for training data.
#model = "ANN", i.e., additive error(A), no trend (N) & no seasonality (N).
ses.orig <- ets(train.ts, model = "ANN", alpha = 0.2)
ses.orig

# make predictions using  SES model for validation period. 
ses.orig.pred <- forecast(ses.orig, h = nValid, level = 0)
ses.orig.pred

#Accuracy for validation partition
round(accuracy(ses.orig.pred$mean, valid.ts), 3)

## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA. 
## OPTIMAL PARAMETERS FOR ALPHA, BETA, AND GAMMA.

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "AAA", i.e., additive error(A), 
# additive trend (A), & additive seasonality (A). 
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.AAA <- ets(train.ts, model = "AAA")
hw.AAA

#make predictions using this HW model for validation period. 
hw.AAA.pred <- forecast(hw.AAA, h = nValid, level = 0)
hw.AAA.pred

# Plot HW predictions for HW additive model (AAA) optimal smoothing parameters.
plot(hw.AAA.pred$mean, 
     xlab = "Time", ylab = "Production", ylim = c(50, 180), 
     bty = "l", xlim = c(1985, 2022), xaxt = "n",
     main = "Holt-Winter's Additive Model with Optimal Smoothing Parameters", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(hw.AAA.pred$fitted, col = "blue", lwd = 2)
lines(production.ts)
legend(1985,180, 
       legend = c("Electricity Production", 
                  "Holt-Winter's Additive Model for Training Period",
                  "Holt-Winter's Additive Model for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2009, 2009), c(0, 150))
lines(c(2018, 2018), c(0, 150))
text(1994, 140, "Training")
text(2014, 140, "Validation")
text(2020, 140, "Future")

#accuracy for validation data
round(accuracy(hw.AAA.pred$mean, valid.ts), 3)


# Create Holt-Winter's (HW) exponential smoothing for partitioned data with model = "ZZZ"
#i.e., automatic selection of error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

#make predictions using this HW model with validation period. 
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Production", ylim = c(50, 180), 
     bty = "l", xlim = c(1985, 2022), xaxt = "n",
     main = "Holt-Winter's Additive Model with Optimal Smoothing Parameters", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(production.ts)
legend(1985,180, 
       legend = c("Electricity Production", 
                  "Holt-Winter's Additive Model for Training Period",
                  "Holt-Winter's Additive Model for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2009, 2009), c(0, 150))
lines(c(2018, 2018), c(0, 150))
text(1994, 140, "Training")
text(2014, 140, "Validation")
text(2020, 140, "Future")

#accuracy for validation partition
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)

##REGRESSION BASED MODELS

#Regression Model with Seasonality for training dataset
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)


# make predictions for ts with seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)

# Plot ts data, regression model with seasonality, and forecast for validation period.
plot(train.season.pred$mean, 
     xlab = "Time", ylab = "Production",
     ylim = c(50, 180), bty = "l",
     xlim = c(1985, 2022), xaxt = "n",
     main = "Regression Model with Seasonality ", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(train.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(1984,180, legend = c("Electricity Production Time Series", "Seasonality Model for Training Data",
                             "Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2009, 2009), c(0, 150))
lines(c(2018, 2018), c(0, 150))
text(1994, 140, "Training")
text(2014, 140, "Validation")
text(2020, 140, "Future")

#accuracy for validation partition
round(accuracy(train.season.pred$mean, valid.ts), 3)


#Regression Model with Linear Trend and Seasonality for training partition
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)


# make predictions for ts with linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)


# Plot ts data, linear trend and seasonality data, and predictions for validation period.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Production", 
     ylim = c(50, 180), bty = "l",
     xlim = c(1985, 2022), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)
legend(1985,180, legend = c("Electricity Production Time Series", 
                             "Linear Trend and Seasonality Model for Training Data",
                             "Linear Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(2, 2, 2), bty = "n")
# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2009, 2009), c(0, 150))
lines(c(2018, 2018), c(0, 150))
text(1994, 140, "Training")
text(2014, 140, "Validation")
text(2020, 140, "Future")

#accuracy for validation data 
round(accuracy(train.lin.season.pred$mean, valid.ts),3)


#Regression model with quadratic trend and seasonality for training partition
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.season)

# make predictions for ts with trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.quad.season.pred$mean, 
     xlab = "Time", ylab = "Production", 
     ylim = c(50, 180), bty = "l",
     xlim = c(1985, 2022), xaxt = "n", 
     main = "Regression Model with Quadratic Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(train.quad.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)
legend(1985,180, legend = c("Electricity Production Time Series", 
                             "Quadratic Trend and Seasonality Model for Training Data",
                             "Quadratic Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2009, 2009), c(0, 150))
lines(c(2018, 2018), c(0, 150))
text(1994, 140, "Training")
text(2014, 140, "Validation")
text(2020, 140, "Future")

#Accuracy for validation partition
round(accuracy(train.quad.season.pred$mean, valid.ts),3)



## CREATE TWO-LEVEL MODEL WITH LINEAR TREND AND SEASONALITY MODEL AND AR(1) RESIDUALS.


# create linear trend and seasonal model for training partition
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(train.lin.season)

# make predictions for ts with linear trend and seasonal model in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets)
Acf(train.lin.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Electricity Production Training Residuals")
Acf(valid.ts - train.lin.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Electricity Production Validation Residuals")

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.

res.ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(res.ar1)

# make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# identify autocorrelation for the training residual of residuals 
#plot autocorrelation for different lags 
Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Electricity Training Residuals of Residuals")

# Create two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for validation period. 
valid.two.level.pred <- train.lin.season.pred$mean + res.ar1.pred$mean


# plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
plot(valid.two.level.pred, 
     xlab = "Time", ylab = "Production", ylim = c(50, 180), 
     bty = "l", xlim = c(1985, 2022), xaxt = "n",
     main = "Two-Level Forecast: Regression with Trend
             and Seasonlity + AR(1) for Residuals", lwd = 2,
     col = "blue", lty = 2) 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lwd = 2, lty = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1985,180, legend = c("Electricity Production Time Series", "Regression for Training Data",
                             "Two Level Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2009, 2009), c(0, 150))
lines(c(2018, 2018), c(0, 150))
text(1994, 140, "Training")
text(2014, 140, "Validation")
text(2020, 140, "Future")

#Accuracy for validation data
round(accuracy(valid.two.level.pred, valid.ts), 3)


###ARIMA MODELS

##ARIMA (2,1,2)

# Use Arima() function to fit ARIMA(2,1,2) model.
# Use summary() to show ARIMA model and its parameters.
train.arima <- Arima(train.ts, order = c(2,1,2)) 
summary(train.arima)

# make predictions for ts with ARIMA model in validation set.    
train.arima.pred <- forecast(train.arima, h = nValid, level = 0)
train.arima.pred

# Using Acf() function, create autocorrelation chart of ARIMA(2,1,2) model residuals.
Acf(train.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of ARIMA(2,1,2) Model Residuals")

# Plot ts data, ARIMA model, and predictions for validation period.
plot(train.arima.pred, 
     xlab = "Time", ylab = "Production", 
     ylim = c(50, 180), xaxt = "n",
     bty = "l", xlim = c(1985, 2022), 
     main = "ARIMA(2,1,2) Model", lwd = 2, flty = 5) 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(train.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1985,180, legend = c("Electricity Production Time Series", 
                             "ARIMA Forecast for Training Period",
                             "ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2009, 2009), c(0, 150))
lines(c(2018, 2018), c(0, 150))
text(1994, 140, "Training")
text(2014, 140, "Validation")
text(2020, 140, "Future")


#Accuracy for validation data
round(accuracy(train.arima.pred$mean, valid.ts), 3)

##AUTO ARIMA MODEL

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# make predictions for ts with auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "production Sold", 
     ylim = c(50, 180), xaxt = "n", 
     bty = "l", xlim = c(1985, 2022), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(1985,180, legend = c("Electricity Production Time Series", 
                             "Auto ARIMA Forecast for Training Period",
                             "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2009, 2009), c(0, 150))
lines(c(2018, 2018), c(0, 150))
text(1994, 140, "Training")
text(2014, 140, "Validation")
text(2020, 140, "Future")

#Accuracy for validation dataset
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

#### After comparing the accuracy, below models would be used for future forecasting:
#Two level model with Linear trend and seasonality + Trailing MA
#Holt-Winter's ZZZ model
#Regression Model with Seasonality
#ARIMA (2,1,2)
#Auto ARIMA


##Using Two level model with Linear trend and seasonality + Trailing MA for entire dataset

# Fit a regression model with linear trend and seasonality for
# entire data set.
tot.trend.seas <- tslm(production.ts ~ trend  + season)
summary(tot.trend.seas)

# Create regression forecast for future 24 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 24, level = 0)
tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 4, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 24 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 24, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 24 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

# Plot original time series data and regression model.
plot(production.ts, 
     xlab = "Time", ylab = "production", ylim = c(50, 180), 
     bty = "l", xlim = c(1985, 2022), lwd =1, xaxt = "n",
     main = "Electricity production Data and Regression with Trend and Seasonality") 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(tot.trend.seas$fitted, col = "blue", lwd = 2)
lines(tot.trend.seas.pred$mean, col = "blue", lty =5, lwd = 2)
legend(1984,180, legend = c("production", "Regression",
                             "Regression Forecast for Future 24 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")
# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.

lines(c(2018, 2018), c(0, 150))
text(2000, 140, "DataSet")
text(2020, 140, "Future")




# Plot regression residuals data and trailing MA based on residuals.
plot(tot.trend.seas.res, 
     xlab = "Time", ylab = "production", ylim = c(-30, 30), 
     bty = "l", xaxt = "n", xlim = c(1985, 2022), lwd =1, col = "brown", 
     main = "Regression Residuals and Trailing MA for Residuals") 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(tot.ma.trail.res, col = "blue", lwd = 2, lty = 1)
lines(tot.ma.trail.res.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1984, 30, legend = c("Regresssion Residuals", 
                              "Trailing MA (k=4) for Residuals", 
                              "Trailing MA Forecast (k=4) for Future 24 Periods"), 
       col = c("brown", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")


lines(c(2018, 2018), c(-30, 150))
text(2000, 140, "DataSet")
text(2020, 140, "Future")


#Accuracy for the forecast
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, production.ts), 3)
round(accuracy((snaive(production.ts))$fitted, production.ts), 3)


## FORECAST WITH HOLT-WINTER'S ZZZ MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 24 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full data set. 
HW.ZZZ <- ets(production.ts, model = "ZZZ")
HW.ZZZ

#make predictions using this HW model for 24 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 24 , level = 0)
HW.ZZZ.pred

# plot HW predictions for original data, optimal smoothing parameters.
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "production", ylim = c(50, 180), 
     bty = "l", xlim = c(1985, 2022), xaxt = "n",
     main = "Holt-Winter's ZZZ Model to Forecast Future 24 Periods", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(production.ts)
legend(1984,180, 
       legend = c("Electricity production", 
                  "Holt-Winter'sModel for Entire Data Set",
                  "Holt-Winter's Model Forecast, Future 24 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")
lines(c(2018, 2018), c(0, 150))
text(2000, 140, "DataSet")
text(2020, 140, "Future")

#Accuracy for forecast
round(accuracy(HW.ZZZ.pred$fitted, production.ts), 3)



## FORECAST WITH REGRESSION MODEL WITH SEASONALITY USING ENTIRE DATA SET INTO
## THE FUTURE FOR 24 PERIODS.

#Regression Model with Seasonality for training dataset
data.season <- tslm(production.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(data.season)


# make predictions for ts with seasonality data in validation set.  
data.season.pred <- forecast(data.season, h = 24, level = 0)

# Plot ts data, regression model with seasonality, and forecast for validation period.
plot(data.season.pred$mean, 
     xlab = "Time", ylab = "production", ylim = c(50, 180), 
     bty = "l", xlim = c(1985, 2022), xaxt = "n",
     main = "Regression Model with Seasonality to Forecast Future 24 Periods", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(data.season.pred$fitted, col = "blue", lwd = 2)
lines(production.ts)
legend(1984,180, 
       legend = c("production", 
                  "Regression model with seasonality for Entire Data Set",
                  "Regression model with seasonality, Future 24 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")
lines(c(2018, 2018), c(0, 150))
text(2000, 140, "DataSet")
text(2020, 140, "Future")

#accuracy for forecast
round(accuracy(data.season.pred$fitted, production.ts), 3)


## FORECAST WITH ARIMA(2,1,2) USING ENTIRE DATA SET INTO
## THE FUTURE FOR 24 PERIODS.

# Use arima() function to fit seasonal ARIMA(2,1,2) model 
# for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
arima.seas <- Arima(production.ts, order = c(2,1,2))
summary(arima.seas)

# Apply forecast() function to make predictions for ts with ARIMA (2,1,2) model 
#for the future 24 periods. 
arima.seas.pred <- forecast(arima.seas, h = 24, level = 0)
arima.seas.pred


# Use Acf() function to create autocorrelation chart of ARIMA (2,1,2)
# model residuals.
Acf(arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of Seasonal ARIMA (2,1,2) Model Residuals")


# Plot historical data, predictions for historical data, and seasonal 
# ARIMA(2,1,2) forecast for 24 future periods.
plot(production.ts, 
     xlab = "Time", ylab = "production", 
     ylim = c(50, 180), xaxt = "n",
     bty = "l", xlim = c(1985, 2022), lwd = 2,
     main = "ARIMA(2,1,2) Model for Entire Data Set") 
axis(1, at = seq(1985, 2024, 1), labels = format(seq(1985, 2024, 1)))
lines(arima.seas$fitted, col = "blue", lwd = 2)
lines(arima.seas.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1984,180, legend = c("Electricity production Time Series", 
                             "ARIMA(2,1,2) Forecast", 
                             "ARIMA(2,1,2) Forecast for 24 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
lines(c(2018, 2018), c(0, 150))
text(2000, 140, "DataSet")
text(2020, 140, "Future")

#Accuracy for the forecast
round(accuracy(arima.seas.pred$fitted, production.ts), 3)



###AUTO ARIMA
# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(production.ts)
summary(auto.arima)


# make predictions for ts with auto ARIMA model for the future 24 periods. 
auto.arima.pred <- forecast(auto.arima, h = 24, level = 0)
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 24 future periods.
plot(production.ts, 
     xlab = "Time", ylab = "production", 
     ylim = c(50, 180), xaxt = "n", 
     bty = "l", xlim = c(1985, 2022), lwd = 2,
     main = "Auto ARIMA Model for Entire Dataset") 
axis(1, at = seq(1985, 2024, 1), labels = format(seq(1985, 2024, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1984,180, legend = c("production Sold Series", 
                             "Auto ARIMA Forecast", 
                             "Auto ARIMA Forecast for 24 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
lines(c(2018, 2018), c(0, 150))
text(2000, 140, "DataSet")
text(2020, 140, "Future")

#Accuracy for forecast
round(accuracy(auto.arima.pred$fitted, production.ts), 3)

#Quadratic trend with seasonality
# Use tslm() function to create linear trend model.
quad.trend <- tslm(production.ts ~ trend + I(trend^2) + season)

# See summary of linear trend equation and associated parameters.
summary(quad.trend)

# Apply forecast() function to make predictions for ts with 
# linear trend  data in 12 future periods.
quad.trend.pred <- forecast(quad.trend, h = 12, level = 0)

# Plot ts data, regression model with seasonality, and forecast for validation period.
plot(quad.trend.pred$mean, 
     xlab = "Time", ylab = "production", ylim = c(50, 180), 
     bty = "l", xlim = c(1985, 2022), xaxt = "n",
     main = "Regression Model with Seasonality to Forecast Future 24 Periods", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(1985, 2022, 1), labels = format(seq(1985, 2022, 1)))
lines(quad.trend.pred$fitted, col = "blue", lwd = 2)
lines(production.ts)
legend(1984,180, 
       legend = c("production", 
                  "Regression model with Quadractic trend and seasonality for Entire Data Set",
                  "Regression model with Quadractic trend and seasonality, Future 24 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")
lines(c(2018, 2018), c(0, 150))
text(2000, 140, "DataSet")
text(2020, 140, "Future")

#Accuracy for quad trend with seasonality
round(accuracy(quad.trend.pred$fitted, production.ts), 3)



#Comparing accuracies
round(accuracy(quad.trend.pred$fitted, production.ts), 3)
round(accuracy(auto.arima.pred$fitted, production.ts), 3)
round(accuracy(arima.seas.pred$fitted, production.ts), 3)
round(accuracy(data.season.pred$fitted, production.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, production.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, production.ts), 3)
round(accuracy((snaive(production.ts))$fitted, production.ts), 3)
round(accuracy((naive(production.ts))$fitted, production.ts), 3)

