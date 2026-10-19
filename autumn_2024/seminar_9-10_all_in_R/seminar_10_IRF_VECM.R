###################
####### TSE #######
##### Class 10 #####
###################


library(vars)
library(tidyverse)
library(readxl)
library(fpp)


data(usconsumption, package = "fpp")
head(usconsumption)

var3 <- VAR(usconsumption, p = 3, type = "const")

## SVAR example: defining Amat
qrm_data <- read_excel("qrm_data.xlsx")
qrm_ts <- ts(qrm_data, start = c(1970, 1), frequency = 4)
var_qrm <- VAR(qrm_ts, p = 2, type = "both")


amat <- diag(3)
amat 
amat[1,2] = NA
amat[2,1] = NA
amat[2,3] = NA
amat

## SVAR example: defining Bmat
bmat <- diag(3)
diag(bmat) = NA
bmat

svar_islm <- SVAR(var_qrm, Amat = amat, Bmat = bmat)


## SVAR example: A and B estimates
svar_islm$A
svar_islm$B

##Impulse response functions
irf_o <- irf(svar_islm, n.ahead = 48, impulse = "output")
plot(irf_o)

##Impulse response functions
irf_m <- irf(svar_islm, n.ahead = 48, impulse = "money")
plot(irf_m)

## FEVD
fevd(svar_islm, n.ahead = 12)


#Blanchard and Quah (1989) model
bq_data <- read_excel("bq_data.xlsx")
bq_ts <- ts(bq_data, start = c(1948, 2), freq = 4)
var_bq <- VAR(bq_ts, p = 8, type = "const")
svar_bq = BQ(var_bq)
svar_bq$B
svar_bq$LRIM

## IRFs for BQ(1989) model:code

irf_bq_QQ <- irf(svar_bq, n.ahead = 40, cumulative =
                   TRUE,response = c("dQ"),impulse =c("dQ")) 
irf_bq_uQ <- irf(svar_bq, n.ahead = 40, cumulative =
                   TRUE,response = c("dQ"),impulse =c("u")) 

par(mfrow = c(2,1))
plotIRF(irf_bq_QQ, vlabels = "Real Output", 
        slabels = "supply shock")
plotIRF(irf_bq_uQ, vlabels = "Real Output", 
        slabels = "demand shock")

## IRFs for BQ(1989) model:code(2)

irf_bq_Qu <- irf(svar_bq, n.ahead = 40, 
                 response = "u",impulse ="dQ")
irf_bq_uu <- irf(svar_bq, n.ahead = 40, 
                 response = "u",impulse = "u") 
par(mfrow = c(2,1))
plotIRF(irf_bq_Qu, vlabels = "Unemployment", 
        slabels = "supply shock")
plotIRF(irf_bq_uu, vlabels = "Unemployment", 
        slabels = "demand shock")


## Forward-error variance decomposition
fevd_bq  <- fevd(svar_bq, n.ahead = 12)





################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

library(tidyverse)
library(forecast)
library(lattice)
library(gridExtra)
library(urca)
library(readxl)
library(vars)
library(matrixcalc)
## Cointegration: idea
T <- 200
u2 <- rnorm(T, mean = 0, sd = 2)
u3 <- rnorm(T, mean = 0, sd = 3)
r1 <- arima.sim(n = T - 1, model = list(order = c(1,1,0), ar = 0.6))
r2 <- 3 + 0.8*r1 + u2
r3 <- -4 + 1.2*r1 + u3
all_r <- tibble(r1 = r1, r2 = r2, r3 = r3, time = 1:T)

## Cointegration: idea(3)
plot2d <- autoplot(cbind(r1,r2,r3))
plot3d <- cloud(data = all_r, r3 ~ r1 * r2)


## Data graphs 
money_demand_germany <- read_excel("money_demand_germany.xlsx")
gd_ts <- ts(money_demand_germany, start = c(1975, 1), frequency = 4)
plotR <- autoplot(gd_ts[,"r"]) + ggtitle("Nominal LT interest rate")
plotY <- autoplot(gd_ts[,"y"]) + ggtitle("Real GNP (in logs)")
grid.arrange(plotR, plotY, ncol = 2)   
plotM <- autoplot(gd_ts[,"m"]) + ggtitle("Real M3(in logs)")
plotM

## Autocorrelation functions
plotR_acf <- ggAcf(gd_ts[,"r"]) + ggtitle("ACF for interest rate")
plotY_acf <- ggAcf(gd_ts[,"y"]) + ggtitle("ACF for GNP")
grid.arrange(plotR_acf, plotY_acf, ncol = 2)  
plotM_acf <- ggAcf(gd_ts[,"m"]) + ggtitle("ACF for M3")
plotM_acf

## Partial autocorrelation functions
plotR_pacf <- ggPacf(gd_ts[,"r"]) + ggtitle("PACF for interest rate")
plotY_pacf <- ggPacf(gd_ts[,"y"]) + ggtitle("PACF for GNP")
grid.arrange(plotR_pacf, plotY_pacf, ncol = 2) 
plotM_pacf <- ggPacf(gd_ts[,"m"]) + ggtitle("PACF for M3")
plotM_pacf

## ADF tests 
adf_r <- ur.df(gd_ts[,"r"], type = "drift",lags = 8, selectlags = "BIC")
summary(adf_r)
adf_dr <- ur.df(diff(gd_ts[,"r"]), type = "none",lags = 8, selectlags = "BIC")
summary(adf_dr)


## Determining the cointegration rank
unif_dum = rep(0, 96)

unif_dum[63] = 1
VARselect(gd_ts, lag.max = 10, type =  "both",
          season = 4, exogen = unif_dum)$select
coint_h1 <- ca.jo(gd_ts, type = "trace", K = 2, 
                  spec = "transitory", season = 4, 
                  dumvar = unif_dum)
coint_h2 <- ca.jo(gd_ts, type = "eigen", K = 2, 
                  spec = "transitory", season = 4, 
                  dumvar = unif_dum)
summary(coint_h1)
summary(coint_h2)

## Eigenvectors and cointegration relations
coint_h2@V

## Error correction term
ec = gd_ts[,"m"] + coint_h2@V[2,1]*gd_ts[,"y"] +
  coint_h2@V[3,1]*gd_ts[,"r"]
plot_ec <- autoplot(ec) + ggtitle("Error correction term")
acf_ec <- ggAcf(ec)
grid.arrange(plot_ec, acf_ec, ncol = 2 )

## Unit root and stationarity tests for $EC_t$
adf_ec <- ur.df(ec, type = "drift",lags = 8, selectlags = "BIC")
summary(adf_ec)

kpss_ec <- ur.kpss(ec, type = "mu",lags = "short")
summary(kpss_ec)

## Matrix decomposition
alpha_m <- coint_h2@W[,1] # Loading matrix 
alpha_m 
beta_m <- coint_h2@V[,1] # Cointegration matrix 
beta_m
pi_m <- alpha_m %*% t(beta_m)
pi_m
matrix.rank(pi_m)

## VECM estimation
gd_vecm <- cajorls(coint_h2, r = 1) 
gd_var <- vec2var(coint_h2, r = 1) 
gd_var2 <- VAR(gd_ts, p = 2, type = "const", 
               season = 4, exogen = unif_dum)

## Graph of residuals:  m - equation
res1_m <- ts(residuals(gd_var)[,1], start = c(1975, 3), frequency = 4)
res2_m <- ts(residuals(gd_var2)[,1], start = c(1975, 3), frequency = 4)
autoplot(cbind(res1_m,res2_m))

## Graph of residuals:  y - equation
res1_y <- ts(residuals(gd_var)[,2], start = c(1975, 3), frequency = 4)
res2_y <- ts(residuals(gd_var2)[,2], start = c(1975, 3), frequency = 4)
autoplot(cbind(res1_y,res2_y))

## Graph of residuals:  r - equation
res1_r <- ts(residuals(gd_var)[,3], start = c(1975, 3), frequency = 4)
res2_r <- ts(residuals(gd_var2)[,3], start = c(1975, 3), frequency = 4)
autoplot(cbind(res1_r,res2_r))
