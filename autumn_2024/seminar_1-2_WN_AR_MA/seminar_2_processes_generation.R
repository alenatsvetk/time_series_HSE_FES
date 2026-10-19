#######################################
############### TSE  ###############
############### Class 2 ###############
#######################################


install.packages('forecast')
library('forecast') # ts functions
#install.packages('quantmod')
#library('quantmod') # collect data from yahoo finance

# some stationary process
T <- 10000 # time horizon
beta <- 0.99
y0 <- 0 # value of y at the moment t = 0
y <- rep(NA, T) # set empty vector of y
y <- c(y0, y) # combine y0 and y
u <- rnorm(n = T,
     mean = 0,
     sd = 1) # vector of WN
for (t in 1:T)
{
  y[t + 1] <- beta * y[t] + u[t]
}
plot(y, type = 'l', xlab = 'time', ylab = 'y')

# random walk (non-stationary process)
T <- 10000 # time horizon
beta <- 1
y0 <- 0 # value of y at the moment t = 0
y <- rep(NA, T) # set empty vector of y
y <- c(y0, y) # combine y0 and y
u <- rnorm(n = T,
           mean = 0,
           sd = 1) # vector of WN
for (t in 1:T)
{
  y[t + 1] <- beta * y[t] + u[t]
}
plot(y, type = 'l', xlab = 'time', ylab = 'y')

# non-stationary process
T <- 10000 # time horizon
beta <- 1.01
y0 <- 0 # value of y at the moment t = 0
y <- rep(NA, T) # set empty vector of y
y <- c(y0, y) # combine y0 and y
u <- rnorm(n = T,
           mean = 0,
           sd = 1) # vector of WN
for (t in 1:T)
{
  y[t + 1] <- beta * y[t] + u[t]
}
plot(y, type = 'l', xlab = 'time', ylab = 'y')

# MA(1) process, y(t) = 1 + u(t) + 0.85 * u(t - 1)
# u ~ i.i.d. N(0, 1)
T <- 1000
mu <- 1
alpha <- 0.85
y <- c(0, rep(NA, T))
u <- rnorm(n = T + 1,
           mean = 0,
           sd = 1)
for (t in 1:T)
{
  y[t + 1] <- mu + u[t + 1] + alpha * u[t]
}
plot(y, type = 'l', xlab = 'time', ylab = 'y')
Acf(x = y)
Pacf(x = y)

# MA(1) process, y(t) = 1 + u(t) - 0.85 * u(t - 1)
# u ~ i.i.d. N(0, 1)
T <- 1000
mu <- 1
alpha <- -0.85
y <- c(0, rep(NA, T))
u <- rnorm(n = T + 1,
           mean = 0,
           sd = 1)
for (t in 1:T)
{
  y[t + 1] <- mu + u[t + 1] + alpha * u[t]
}
plot(y, type = 'l', xlab = 'time', ylab = 'y')
Acf(x = y)
Pacf(x = y)

# MA(3) process, y(t) = 3 + u(t) − 0.5 * u(t-1) + 0.06 * u(t-2) + 0.01 * u(t-3),
# u ~ i.i.d. N(0, 1)
T <- 1000
mu <- 3
alpha1 <- -0.5
alpha2 <- 0.06
alpha3 <- 0.01
y <- c(0, rep(NA, T))
u <- rnorm(n = T + 3,
           mean = 0,
           sd = 1)
for (t in 1:T)
{
  y[t + 1] <- mu + u[t + 3] + alpha1 * u[t + 2] + alpha2 * u[t + 1] + alpha2 * u[t]
}
plot(y, type = 'l', xlab = 'time', ylab = 'y')
Acf(x = y)
Pacf(x = y)

# AR(1) process, y(t) = 1 + 0.85 * y(t - 1) + u(t)
# u ~ i.i.d. N(0, 1)
T <- 1000
mu <- 1
beta <- 0.85
y <- c(0, rep(NA, T))
u <- rnorm(n = T,
           mean = 0,
           sd = 1)
for (t in 1:T)
{
  y[t + 1] <- mu + beta * y[t] + u[t]
}
plot(y, type = 'l', xlab = 'time', ylab = 'y')
Acf(x = y)
Pacf(x = y)

# AR(1) process, y(t) = 1 - 0.85 * y(t - 1) + u(t)
# u ~ i.i.d. N(0, 1)
T <- 1000
mu <- 1
beta <- -0.85
y <- c(0, rep(NA, T))
u <- rnorm(n = T,
           mean = 0,
           sd = 1)
for (t in 1:T)
{
  y[t + 1] <- mu + beta * y[t] + u[t]
}
plot(y, type = 'l', xlab = 'time', ylab = 'y')
Acf(x = y)
Pacf(x = y)

# AR(2) process, y(t) = -2 + 0.6 * y(t - 1) - 0.2 * y(t - 2) + u(t)
# u ~ i.i.d. N(0, 1)
T <- 1000
mu <- -2
beta1 <- 0.6
beta2 <- -0.2
y <- c(0, 1, rep(NA, T))
u <- rnorm(n = T,
           mean = 0,
           sd = 1)
for (t in 1:T)
{
  y[t + 2] <- mu + beta1 * y[t + 1] + beta2 * y[t] + u[t]
}
plot(y, type = 'l', xlab = 'time', ylab = 'y')
Acf(x = y)
Pacf(x = y)

# ARMA(1, 1) process, y(t) = 3 + 0.7 * y(t - 1) + u(t) - 0.7 * u(t - 1)
# u ~ i.i.d. N(0, 1)
T <- 1000
mu <- 3
beta <- 0.7
alpha <- -0.7
y <- c(0, rep(NA, T))
u <- rnorm(n = T + 1,
           mean = 0,
           sd = 1)
for (t in 1:T)
{
  y[t + 1] <- mu + beta * y[t] + u[t + 1] + alpha * u[t]
}
plot(y, type = 'l', xlab = 'time', ylab = 'y')
Acf(x = y)
Pacf(x = y)




# AR(2) equation with trend, y(t) = -2 + 0.5 * sqrt(t) + 0.6 * y(t - 1) - 0.2 * y(t - 2) + u(t),
# u ~ i.i.d. N(0, 1)
T <- 1000
mu <- -2
gamma <- 0.5
beta1 <- 0.6
beta2 <- -0.2
y <- c(0, 1, rep(NA, T))
u <- rnorm(n = T,
           mean = 0,
           sd = 1)
for (t in 1:T)
{
  y[t + 2] <- mu + gamma * sqrt(t) + beta1 * y[t + 1] + beta2 * y[t] + u[t]
}
plot(y, type = 'l', xlab = 'time', ylab = 'y')
Acf(x = y)
Pacf(x = y)

# ARMA(1, 1) process, y(t) = 3 + 0.7 * y(t - 1) + u(t) - 0.7 * u(t - 1)
# u ~ i.i.d. N(0, 1)
y <- arima.sim(model = list(ar = 0.7, ma = -0.7), n = 1000)
tsdisplay(y)

# random walk (non-stationary process)
y <- arima.sim(model = list(order = c(0, 1, 0)), n = 100)
tsdisplay(y)
y_diff <- diff(y)
tsdisplay(y_diff)