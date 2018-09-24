### -------------------------------------
### Prep - Survival Analysis in Andalusia
### -------------------------------------

# 0.1 packages 
library(tidyverse)
library(ggplot2)
library(survival)
library(memisc)
library(broom)
library(sp)
library(rgdal)
library(RColorBrewer)
library("gridExtra")
library("lattice")
library(BayesX)
# library(spBayesSurv)
require(spdep)
library("bamlss")


# 0.2 working directory and load data set
getwd()

setwd("C:/Users/Mathias/Documents/LongPop_Madrid/PhD/SP3_urban health")

load("data/025_INDMOR-CT.RData")

### Extract the age groups necessary for the analysis

INMO <- INMO.SC %>% dplyr::filter(age.entry>=35 & age.entry<=80)
# reduces data set to 351,769 individuals

ANDALUS.SC <- readOGR(dsn="C:/Users/y4956294S/Documents/LONGPOP/Subproject 3 - Urban Environment and Health/RCode/UrbanHealthSpain/data/mapas.capas/SecCensales2001",
                      layer = "andalusia.census.tract.2001")

### Graphical analysis


# 1.0 Prepare the spatial weight matrix (Adjacency Matrix)

  ## 1.1) from the shape file

       # d <- INMO.SC[order(INMO.SC$SC), ]
       # head(d)

#spplot(ANDALUS.SC)

      and.nb <- poly2nb(ANDALUS.SC)
      class(and.nb)
                    # ## need a different class for the Bayes package
                    # and.gra <- nb2gra(and.nb)
                    # bndANDA <- sp2bnd(ANDALUS.SC)
      
      and.nb <- nb2mat(and.nb, style = "B", zero.policy = TRUE)
      
      and.nb[and.nb > 0] <- -1
      diag(and.nb) <- apply(and.nb, 1, function(x) { sum(abs(x)) })
      colnames(and.nb) <- rownames(and.nb)
      
      
          # # remove missing districts
          # i <- colnames(and.nb) %in% levels(INMO$SC)
          # and.nb <- and.nb[i, i]
      
      # 
      # # row-standardised weight matrix - increased the weights of links from observation with few neighbours
      # # --- in this case the row sums of the weights are unity
      # adj.mat <- nb2mat(and.nb, style = "B")  
 
# Create an bnd file
 
 require(rgdal)
        # 
        #  ANDA.CT <- readOGR(dsn = "/data", layer = "ANDALUS_SC")
        #  str(depart, max.level = 2)
        #  
        # x <- read.dbf(system.file("C:/Users/Mathias/Documents/LongPop_Madrid/PhD/SP3_urban health/data/ANDALUS_SC.dbf", 
        #                           package="foreign")[1])
        #  
        # ANDA <- shp2bnd("C:/Users/Mathias/Documents/LongPop_Madrid/PhD/SP3_urban health/data/ANDALUS_SC", 
        #         regionnames="objectid", check.is.in = TRUE)
        #  
        # and <- read.bnd(system.file("data/andalusia.census.tract.2001.bnd",
        #                                       + package = "spBayesSurv"))

# adj.mat <- bnd2gra(bndANDA)

# adj.mat <- and.gra

            # E <- diag(diag(adj.mat)) - as.matrix(adj.mat)



### Trying Bamlss
####################################################################################################
# function temperature in dependence on time with spline
    
    # Example
    # -------
    # f <- list(
    #      Surv(arrivaltime) ~ ti(arrivaltime) + ti(arrivaltime,lon,lat),
    #      gamma ~ s(fsintens) + ti(daytime,bs="cc") + ti(lon,lat) +
    #            ti(daytime,lon,lat,bs=c("cc","cr"),d=c(1,2))
    #   )
###################################################################################################

cox.1 <- coxph(Surv(age.exit,event) ~ sexo, data = INMO)      
      
      
# Drawing a sample for further tests
      
d.2 <- INMO[sample(nrow(INMO), 5000), ]

d.2 <- d.2 %>% mutate(exit = ifelse(event==1,age.exit,0))




#### --------- 
#### Formulars
#### --------- 

# simple formula
f <- list(Surv(age.exit,event) ~ ti(age.exit), gamma ~ s(UI.N) + sexo)

# + UI.N + s(SC,bs="mrf",xt=list(penalty=adj.mat))


# Formula of the survival model, note
## that the baseline is given in the first formula by s(time).
f.2 <- list(
  Surv(age.exit,event) ~ s(age.exit)+ s(age.exit, by = UI.N),
  gamma ~ sexo + ecivil + s(DI.N)
)



# advanced
f.3 <- list(Surv(age.exit,event) ~ ti(age.exit) + ti(UI.N), 
            gamma ~ sexo + ecivil + s(DI.N) + s(SC,xt=list(penalty=and.nb)))


######## Run Model(s)

# Test with 5000 individuals
# ---------------------------
bayes.test <- bamlss(f, data = d.2, family="cox", optimizer = FALSE)

summary(bayes.test)
plot(bayes.test)

bayes.test.2 <- bamlss(f.2, data = d.2, family="cox", optimizer = FALSE)

summary(bayes.test.2)
plot(bayes.test.2)
plot(bayes.test.2, which="samples")

plot(bayes.test.2, model = "lambda", term = "s(age.exit)")


# a little different approach
##############################
##############################

    ## Create the bamlss.frame.
    bayes.test.2 <- bamlss.frame(f.2, family = "cox", data = d.2)
    bayes.test.2 <- with(bayes.test.2, surv.transform(x, y, data = model.frame,
                         family = family, is.cox = TRUE, subdivisions = 25))
    
    ## Extract the time grid design matrix for term s(time).
    X <- bayes.test.2$x$lambda$smooth.construct[["s(age.exit)"]]$fit.fun_timegrid(NULL)
    dim(X)
    
    ## Compute fitted values for each time point.
    grid <- attr(bayes.test.2$y[[1]], "grid")
    gdim <- c(length(grid), length(grid[[1]]))
    b <- runif(ncol(X))
    fit <- X 
    fit <- matrix(fit, nrow = gdim[1], ncol = gdim[2], byrow = TRUE)

    plot(as.vector(fit) ~ unlist(grid), type = "n",
         xlab = "Survival time", ylab = "Effect")
    for(j in seq_along(grid)) {
      lines(fit[j, ] ~ grid[[j]], lwd = 2, col = rgb(0.1, 0.1, 0.1, alpha = 0.3))
      points(grid[[j]][gdim[2]], fit[j, gdim[2]], col = "red")
    }

    
    
    



# Using all individuals
# ---------------------
bayesAND <- bamlss(f, data = INMO, family="cox", optimizer = FALSE)

print(bayesAND)

# results
summary(bayesAND)
bayesAND$model.stats
DIC(bayesAND)
plot(bayesAND, model = "lambda", term = "s(age.exit)")



plot(bayesAND)

plot(bayesAND, which="samples")


# More work necessary
# -------------------
# km.byes <- as.data.frame(bayesAND$y$`Surv(age.exit, event)`)








## 2.1 Because of problems with the requested 
# 
# set.seed(100)
#  mcmc <- list(nburn = 500, nsave = 1000, nskip = 10, ndisplay = 100)
#  prior <- list(maxL = 15)
#  ptm <- proc.time()
#  res1 <- survregbayes(formula = Surv(time = age.exit,
#                                       event = event) ~ UI.N + sexo +
#                           frailtyprior("car", SC), data = d, survmodel = "PO",
#                           dist = "loglogistic", mcmc = mcmc, prior = prior, Proximity = E)
#  
#  ## problem with size (draw random sample)
#  d.2 <- d[sample(nrow(d), 1000), ]
#  
#  res2 <- survregbayes(formula = Surv(time = age.exit,
#                                      event = event) ~ UI.N + sexo + dependiente + PCT_OCUPADOS, data = d.2, survmodel = "PH",
#                       dist = "loglogistic", mcmc = mcmc, prior = prior, Proximity = E)
#  # See results
#  (sfit1 <- summary(res2))
 