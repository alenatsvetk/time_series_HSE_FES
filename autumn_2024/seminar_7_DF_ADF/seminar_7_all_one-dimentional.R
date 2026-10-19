###############################
####### TSE ###################
##### Class 7 Autumn 2024 #####
###############################
###############################

#install.packages('rio')
#library('rio')
#install.packages('tidyverse')
library('tidyverse')
#install.packages("forecast")
library('forecast')
#install.packages('plm')
library('plm')
#install.packages('gridExtra')
library('gridExtra')
#install.packages('tseries')
library('tseries')
#install.packages('urca')
library('urca')

######################################################################################
###########!!!ACF and PACF!!##########################################################
######################################################################################
######################################################################################

rus_data <- read_csv('rus_data.csv')
class(rus_data)
rus_data <- select(rus_data, -time) #delete time column
rus_data_ts <- ts(rus_data, start = c(1995, 1), freq = 12)
class(rus_data_ts)

# ARMA models: simulation
# simulating AR(1) model
ar1 <- arima.sim(model = list(ar = 0.8, order = c(1, 0, 0)), n = 200)
# simulating ARMA(1,1) model
arma11 <- arima.sim(model = list(ar = 0.4, ma = 0.5, order = c(1, 0, 1)), n = 200)
# simulating MA(2) model
ma2 <- arima.sim(model = list(ma = c(0.5, -0.3), order = c(0, 0, 2)), n = 200)

# TS visualization
autoplot(cbind(ar1, arma11,ma2), facets = TRUE) + ylab('')

# ACF and PACF for AR(1) model
ggAcf(ar1)
ggPacf(ar1)

# ACF and PACF for MA(2)
ggAcf(ma2)
ggPacf(ma2)

# ACF and PACF for ARMA(1,1)
ggAcf(arma11)
ggPacf(arma11)





######################################################################################
#######################Ljun-Box test##################################################
######################################################################################
######################################################################################

#importing and plotting data 

RUS_oil <-  select(rus_data, oil_price)
Rus_oil_ts <- ts(RUS_oil, start = c(1995, 1), freq = 4)
autoplot(Rus_oil_ts) + ylab('') + ggtitle('Rus_oil growth')

# ACF and PACF
ggAcf(Rus_oil_ts) + ggtitle('Rus_oil growth')
ggPacf(Rus_oil_ts) + ggtitle('Rus_oil growth')

# Ljung-Box test for US investment growth series 
# The Ljun-Box test is a hypothesis test that checks if a time series contains
# an autocorrelation. The null Hypothesis H0 is that the residuals
# are independently distributed. The alternative hypothesis is that
# the residuals are not independently distributed and exhibit a serial correlation.
Box.test(Rus_oil_ts, lag = 12, fitdf = 0, type = 'Lj')

# estimaton(1) 
Arima(Rus_oil_ts, order = c(1, 0, 2), include.constant = TRUE)
Arima(Rus_oil_ts, order = c(3, 0, 4), include.constant = TRUE)
Arima(Rus_oil_ts, order = c(5, 0, 6), include.constant = TRUE)

# lag order choice
model1 <- Arima(Rus_oil_ts, order = c(1, 0, 2), 
                include.constant = TRUE)
model1$aic
model1$bic

# automatic ARIMA
auto.arima(Rus_oil_ts, d = 0, seasonal = FALSE)



######################################################################################
#########################All ARIMAs#########################################################
######################################################################################
######################################################################################


# data
fed_funds <- read_csv('fed_funds.csv')
class(fed_funds)
str(fed_funds)

ffr <- arrange(fed_funds, Date) %>% select(-Date) %>%
  ts(start = c(1960,1), frequency = 12)

class(ffr)
head(ffr, 36)
#window is a generic function which extracts the subset of the object x
#observed between the times start and end. If a frequency is specified,
# the series is then re-sampled at the new frequency.
ffr2 <- window(ffr, end = c(2008, 12))

autoplot(ffr2) + ggtitle('Federal Funds rate') + xlab('') + ylab('')

dffr <- diff(ffr2)
autoplot(dffr) + ggtitle('Diff Federal Funds rate') + xlab('') + ylab('')
ggAcf(dffr)
ggPacf(dffr)

A <- matrix(0, nrow = 5, ncol = 5)
for (i in 0:4)
  {
  for (j in 0:4)
    {
    model <- Arima(dffr, order = c(i, 0, j), include.constant = TRUE)
    A[i + 1, j + 1] <- model$aic
  }
}      

min(A)
rownames(A) <- c('p = 0', 'p = 1', 'p = 2', 'p = 3', 'p = 4')
colnames(A) <- c('q = 0', 'q = 1', 'q = 2', 'q = 3', 'q = 4')
A
A['p = 0', 'q = 0']
A['p = 3', 'q = 0'] #best choice?


######################################################################################
######################!!Seasonality and Forecasts!!###########################################
######################################################################################
######################################################################################
######################################################################################

# preliminary transformation
invest_index <- import('invest_index.csv')
invest <- ts(invest_index, start = c(1994, 1), freq = 12)
autoplot(invest) + ggtitle('Investment index')

# Transformed series
plot1 <- autoplot(sqrt(invest)) + ylab('')+
  ggtitle('Square root of monthly investment index')
plot1
plot2 <- autoplot(invest^(1/3)) + ylab('')+
  ggtitle('Cube root of monthly investment index')
plot2
plot3 <- autoplot(log(invest)) + ylab('')+
  ggtitle('Log of monthly investment index')
plot3
plot4 <- autoplot(invest^(-1)) + ylab('')+
  ggtitle('Inverse of monthly investment index')
plot4
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)

# forecasts: example
y <- arima.sim(model = list(ar = c(0.4, 0.5), ma = 0.4, 
                            order = c(2, 0, 1), mean = 3), 
               n = 200)
fit <- Arima(y,order = c(2, 0, 1), include.constant = TRUE)

# forecasts: code(1)
forecast(fit, h = 12, level = 95)
autoplot(forecast(fit, h = 120)) + ylab('')

# accuracy measures:code
US_inv <- read_csv('US_investment.dat')
US_inv_ts <- ts(US_inv, start = c(1947, 2), freq = 4)
train <- window(US_inv_ts, end = c(1969,4))
fit1 <- Arima(train, order = c(4,0,4))
fore1 <- forecast(fit1, h = 12)
fit2 <- auto.arima(train, d = 0, seasonal = FALSE)
fore2 <- forecast(fit2, h = 12)
options(digits = 3)
accuracy(fore1, US_inv_ts)
accuracy(fore2, US_inv_ts)


######################################################################################
######################################################################################

# trends in economic time series
empl_manuf <- import('empl_manuf.csv')
empl <- ts(empl_manuf, start = c(1992, 1), freq = 12)
autoplot(empl) + ylab('') + ggtitle('Employment in Manufacturing')



######################################################################################
#######################!!seasonal differencing!!######################################
######################################################################################
######################################################################################

invest_index <- import('invest_index.csv')
invest <- ts(invest_index, start = c(1994, 1), freq = 12)
plot1 <- autoplot(invest) + ggtitle('Investment index')
plot2 <- autoplot(diff(invest,12)) +
  ggtitle('Annual change in monthly investment index')
grid.arrange(plot1, plot2, nrow = 2)

# simulated series
phi1 <- 2.3
phi2 <- -1.7
phi3 <- 0.4
y3 <- rep(0, times = 1003)
u <- rnorm(1003)
for (t in 4:1003) {
  y3[t] <- phi1 * y3[t - 1] + phi2 * y3[t - 2] + phi3 * y3[t - 3] + u[t]
}
y3 <- y3[-(1:3)]
t <- 1:1000
y3 <- ts(y3)
autoplot(y3) + ylab('') + ggtitle('Simulated series')

# ACF and PACF for the simulated series
plot1 <- ggAcf(y3) + ggtitle('')
plot2 <- ggPacf(y3) + ggtitle('')
grid.arrange(plot1, plot2, nrow = 2)



# ADF test
#Performs the Augmented Dickey-Fuller test for the null hypothesis of a unit root of a univarate time series
adf.test(y3, k = 10)

#Perform the augmented Dickey-Fuller unit root test.
#urtest <- ur.df(y3, type = 'trend', lags = 12, selectlags = "BIC")
#summary(urtest)
