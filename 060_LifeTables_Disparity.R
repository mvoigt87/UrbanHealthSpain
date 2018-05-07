### ------------------------------------------------------------------------------------------------- ### 
### Life Span Disparity Measures
### ------------------------------------------------------------------------------------------------- ### 

### 0.1. Loading data set
load("031_RETIND.RData")
# ----------------------------- 
# 637345 individuals
# -----------------------------

### 0.2 load necessary package
library(reshape)
library(tidyverse)
library(survival)
library(forcats)
library(data.table)
library(broom)
library(stargazer)

source("C:/Users/y4956294S/Documents/R/EDagger.R")


### Build a life table from the retirement data

# number of age specific deaths (65 +) and exposure
deaths.fem <- retire$exit.age[]


y <- unique(deaths$Year)
x <- unique(deaths$Age)
m <- length(x)
n <- length(y)

## Deaths and exposures in a matrix format (age x year)
D.Fem <- do.call(cbind,tapply(X = deaths$Female, INDEX = deaths$Year, FUN = identity))

E.Fem <- do.call(cbind,tapply(X = expos$Female, INDEX = expos$Year, FUN = identity))


# some E are equal to zer0, -> weights are necessary  (Wheight matrix)
W <- matrix(1, m, n)
W[E.Fem==0] <- 0


# subjective lambdas (previously optmize for swiss males by GC) ---> same for females??

fit.Fem <- Mort2Dsmooth(x = x, y = y,
                        Z = D.Fem, offset=log(E.Fem),
                        W=W,
                        method=3, lambdas=c(3.2, 32))
fit.Fem$lambdas

# plot raw age-specific mortality rates vs. smoothed rates
plot(fit.Fem)
# See the fit of log mortality (should be more or less on a straight line)
plot(fit.Fem$logmortality, log(D.Fem/E.Fem))

## compute the density from the fitted hazard at finer grid
## finer-grid age - decimal ages (0.1)
delta <- 0.1
xs <- seq(min(x), max(x), delta)
ms <- length(xs)

## new basis over new ages
xl <- min(x)
xr <- max(x)
xmax <- xr + 0.01 * (xr - xl)
xmin <- xl - 0.01 * (xr - xl)

# generate an equally-spaced B-Splines basis over the abscissa (applying Mort1Dsmooth)
Bxs.Fem <- MortSmooth_bbase(xs, xmin, xmax, fit.Fem$ndx[1], fit.Fem$deg[1])

## over years are the same
By.Fem <- fit.Fem$By
## fitted coefficients
betas.Fem <- fit.Fem$coef
## log-mortality (linear predictor) over new ages and years (output: log hazard)
ln.h.Fem <- MortSmooth_BcoefB(Bxs.Fem, By.Fem, betas.Fem)
## hazard
h.Fem <- exp(ln.h.Fem)
## cumulative hazard using cumsum
H.Fem <- matrix(0, ms, n)
for(i in 1:n){
  H.Fem[,i] <- cumsum(h.Fem[,i]*delta)
}

## fitted survival functions
S.Fem <- apply(H.Fem, 2, function(x){exp(-x)})
## fitted density functions
f.Fem <- h.Fem * S.Fem

image(t(f.Fem))


#### building life tables from the fx ####

dim(f.Fem)
# radix of the table by year
colSums(f.Fem)

# survival function by year (beautiful ;) )
matplot(S.Fem, type = "l", lty = 1, col = heat.colors(n = ncol(S.Fem)))

# rough estimate of the life expectancy at birth
colSums(S.Fem) / 10
plot(y, colSums(S.Fem) / 10, type = "b", las = 1, main = "e0")


# life table - reminder of the main functions
# library(MortHump)
# LT

# ----------------- #
# build life table
# ----------------- #
##  help variables
# 1001 age groups
N <- length(xs)
# the 0.1 differences
Widths <- rep(delta, length(xs))


## large data frame which contains values in HMD format
fem.smooth <- as.data.frame(rep(xs,n))
colnames(fem.smooth) <- "xs"  
fem.smooth <- fem.smooth %>% 
  mutate(Year=rep(min(y):max(y), times=1, each=1001)) %>% 
  # ax values (may be to be changed for the highest age groups)
  mutate(ax = rep(Widths / 2, times=n))
# ... step in between

# ------------------------------------------------------------
# obtain the mx values from the smoothed hazard function
dim(h.Fem)
## get the hx in the right format
h.new.Fem <- as.data.frame(h.Fem)
h.new.Fem <- data.frame(mx=unlist(h.new.Fem, use.names = FALSE))
# and add them as column to the femsmooth data frame
# ------------------------------------------------------------  

# continuing building the life table      
fem.smooth <- fem.smooth %>% bind_cols(h.new.Fem) %>% 
  # qx
  mutate(qx = (Widths * mx) / (1 + (Widths - ax) * mx)) 
# ------------------------------------------------------------
## make the last qx=1 with a little trick which would not work with a data frame
qx <- matrix(fem.smooth$qx)
qx[1:(0+1001)==(0+1001)] <- 1
# ------------------------------------------------------------      
fem.smooth <- fem.smooth %>% select(-qx) %>%  bind_cols(as.data.frame(qx))
colnames(fem.smooth)[5] <- "qx"
fem.smooth <- fem.smooth %>% mutate(qx = ifelse(qx>1,1,qx)) %>% 
  ## add the px
  mutate(px = 1 - qx)
## lx beginning at the estimated radix (sum f[,1])
# ------------------------------------------------------------   
## matrix operations: sum over the columns of the estimated f-values to obtain
## the base/radix for the life table
radix.mat <- as.data.frame(matrix(data=colSums (f.Fem, na.rm = FALSE, dims = 1), nrow = 1)) %>% 
  ## now making filling dummie values in between to make it the same length as the data frame
  bind_rows(as.data.frame(matrix(data = 0,nrow = 1000, ncol = 139))) 
## stack them in order and delete the extra variable
radix.mat <- stack(radix.mat) %>% select(-ind)
# ------------------------------------------------------------   

## now add them to the large life table
fem.smooth <- fem.smooth %>% bind_cols(as.data.frame(radix.mat))
colnames(fem.smooth)[7] <- "lx"
## use the dplyr group_by command to calculate the rest of the lx from the px
fem.smooth <- fem.smooth %>% group_by(Year) %>% mutate(lx = c(lx[1],lx[1] * cumprod(px))[1:N]) %>% 
  ## dx values from the lx (alternatively from the smoothing algorithm)
  group_by(Year) %>%  mutate(dx = c(-diff(lx),lx[N])) %>% 
  ## Create the Lx from the lx and the dx
  group_by(Year) %>% mutate(Lx = c(Widths[1:(N - 1)] * lx[2:N] + ax[1:(N - 1)] * dx[1:(N - 1)], lx[N] * ax[N])) %>% 
  ## account for infinite Lx and NA
  mutate(Lx = ifelse(is.infinite(Lx),1,Lx)) %>% mutate(Lx = ifelse(is.na(Lx),0,Lx)) %>% 
  ## Calculate the Tx from the Lx
  group_by(Year) %>% mutate(Tx = rev(cumsum(rev(Lx)))) %>% 
  ## Finally obtain the life expectancy from the Tx and lx
  group_by(Year) %>% mutate(ex = Tx / lx)

## Little change of the variable name for age to allow for universal use

cbind(colnames(fem.smooth))
colnames(fem.smooth)[1] <- "Age"


## test plot - looks believable

fem.smooth %>% filter(Age==10) %>% ggplot(aes(x=Year,y=ex)) +
  geom_line() +
  scale_y_continuous(name = "ex at age 10") +
  theme_bw()

ex <- as.vector(fem.smooth$ex[fem.smooth$xs == 10])
y 
summary(lm(ex ~ y))
