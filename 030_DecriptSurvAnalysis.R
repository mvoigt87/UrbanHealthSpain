#### PAA - Urban Environment and Mortality Differentials in South Spain ####
#### ------------------------------------------------------------------ ####

### ---------------------------------------------------------------------------------------- ###
### Plan
###
### 1. Descriptive Exploration
### 2. Check for Spatial Auto-Correlation
### 3. Survival Analysis (Kaplan-Meier, simple PH model)
### 4. Mixed Effects Cox Model
### ---------------------------------------------------------------------------------------- ###

# 0.1 packages 
library(tidyverse)
library(ggplot2)
library(data.table)
library(survival)
library(stargazer)
library(memisc)
library(broom)
library(readxl)
library(sp)
library(rgdal)


# 0.2 load data set
load("data/025_INDMOR-CT.RData")

###
### Extract the age groups necessary for the analysis
###
    INMO.SC <- INMO.SC %>% dplyr::filter(age.entry>=35 & age.entry<=95)
    # reduces data set to 372007 individuals

### ------------------------------------------------------------------------------- ###
###                          1. Descriptive Statistics                              ###
### ------------------------------------------------------------------------------- ###

# --------- #
#    Sex    #
# --------- #
# event distribution (deaths by sex)
round(prop.table(table(INMO.SC$event,INMO.SC$sexo),2),2)

## ------------------------- ##
##   Marital/Civil Status    ##
## ------------------------- ##
round(prop.table(table(INMO.SC$ecivil)),2)
# event distribution (deaths by marital status)
round(prop.table(table(INMO.SC$event,INMO.SC$ecivil),2),2) 
  # doesn´t make much sense without age structure

## ------------------------------- ##
##   Highest educational degree    ##
## ------------------------------- ##
round(prop.table(table(INMO.SC$estudios4)),2)
# event distribution (deaths by marital status)
round(prop.table(table(INMO.SC$event,INMO.SC$estudios4),2),2) 
  # same problem with the age structure (graph)

INMO.SC %>% ggplot(aes(age.entry, fill=estudios4)) +  
  geom_histogram(binwidth = 1) + 
  scale_fill_brewer(name=" ", palette="Set1")  +
  scale_x_continuous(name=" ") +
  scale_y_continuous(name=" ") +
  theme_bw()

## -------------------------------- ##
##   Occupation + Jubilado          ##
## -------------------------------- ##
round(prop.table(table(INMO.SC$OCCUP2)),2)
round(prop.table(table(INMO.SC$event,INMO.SC$OCCUP2),2),2) 
  # difficult to disentangle as no information on the retired population exists

## ------------------------------- ##
##         Nationality             ##
## ------------------------------- ##
round(prop.table(table(INMO.SC$NATIONAL)),2)
round(prop.table(table(INMO.SC$event,INMO.SC$NATIONAL),2),2)
  # again not very useful without age standardization

## ----------------------------------- ##
##         Wealth variables            ##
## ----------------------------------- ##
# ownership cars
round(prop.table(table(INMO.SC$vehic)),2)
round(prop.table(table(INMO.SC$event,INMO.SC$vehic),2),2) # relative strong differences

# household size
round(prop.table(table(INMO.SC$hhsize)),2)
round(prop.table(table(INMO.SC$event,INMO.SC$hhsize),2),2) # lowest group seems to be disadvantaged

# ownership house/apartment
round(prop.table(table(INMO.SC$tenen)),2)
round(prop.table(table(INMO.SC$event,INMO.SC$tenen),2),2) # small differences


## ----------------------------------------- ##
##         Environmental Factors             ##
## ----------------------------------------- ##

# Urban Indicator 
# (to change go back to "010_CreateContextData.R")
round(prop.table(table(INMO.SC$event,INMO.SC$UI.cat),2),2)
chisq.test(INMO.SC$event,INMO.SC$UI, simulate.p.value = FALSE)
  # Both extremes seem to have higher mortality - but it need to be checked for age and population structure

# Deprivation Indicator

INMO.SC %>% mutate(event = as.factor(ifelse(event==1,"dead","alive"))) %>% ggplot(aes(DI.N, fill=event)) +  
  geom_histogram(binwidth = 0.1) + 
  scale_fill_discrete(name=" ") + 
  scale_x_continuous(name=" ") +
  scale_y_continuous(name=" ") +
  theme_bw()



### Further Test
# green areas
round(prop.table(table(INMO.SC$event,INMO.SC$pverde),2),2)

# median age
INMO.SC %>% mutate(event = as.factor(ifelse(event==1,"dead","alive"))) %>% ggplot(aes(EDAD_MEDIA, fill=event)) +  
  geom_histogram(binwidth = 0.1) + 
  scale_fill_discrete(name=" ") + 
  scale_x_continuous(name=" ") +
  scale_y_continuous(name=" ") +
  theme_bw()

# Percentage Employed Peoples by census tract
INMO.SC %>% mutate(event = as.factor(ifelse(event==1,"dead","alive"))) %>% ggplot(aes(PCT_OCUPADOS, fill=event)) +  
  geom_histogram(binwidth = 1) + 
  scale_fill_discrete(name=" ") + 
  scale_x_continuous(name=" ") +
  scale_y_continuous(name=" ") +
  theme_bw()                          # close to normally distributed

# Percentage Workers Agriculture by census tract
INMO.SC %>% mutate(event = as.factor(ifelse(event==1,"dead","alive"))) %>% ggplot(aes(PCT_AGRIC, fill=event)) +  
  geom_histogram(binwidth = 1) + 
  scale_fill_discrete(name=" ") + 
  scale_x_continuous(name=" ") +
  scale_y_continuous(name=" ") +
  theme_bw()

### ----------------------------------------------------------------------------------------- ###
###                         2. Check for Spatial Autocorrelation                              ###
### ----------------------------------------------------------------------------------------- ###

### Load spatial data - shape file

ANDALUS.SC <- readOGR(dsn="C:/Users/y4956294S/Documents/Workshops and Conferences/Cambridge/Data/actual_Urb_Index/spatDat",
                      layer = "da08_seccion_censal")

# Population count by province
pop.prov <- by(ANDALUS.SC$POBLACION, ANDALUS.SC$PROVINCIA, sum)

# Extreme Values Provincial Level
pop.prov[which.max(pop.prov)]  # Sevilla - 1.939.775
pop.prov[which.min(pop.prov)]  # Huelva  - 519.596 

# Extreme Values Municipality Level
pop.mun <- by(ANDALUS.SC$POBLACION, ANDALUS.SC$MUNICIPIO, sum)
pop.mun[which.max(pop.mun)]  # Sevilla -  690.566

# Map by population
# spplot(ANDALUS.SC,"POBLACION")

# access the dataframe of the spatial data frame
  
d <- ANDALUS.SC@data

    ### ---------------------------------------------------------------------------------------- ###
      # Extract variables for spatial tests
      # Urban indicator
      # standardized single indicators
      # number of events / SMR
        load("data/015_ContextAndUI.RData")
        load("data/SMRsc.RData")
        
        SMRsc <- SMRsc %>% dplyr::filter(sex=="Both") %>% mutate(SC=census.tract) %>% dplyr::select(SC,SMRe)
        
        # make the transformation to the final format
        
        SMRsc$SC <- as.character(as.factor(SMRsc$SC))
        table(nchar(SMRsc$SC))

        SPAT.SC <- SCCON %>% dplyr::select(SC, PCT_OCUPADOS, EDAD_MEDIA, PCT_AGRIC, ArtSurfA, Service.area.popacc, pop.den,
                                           POPDEN.I.SD, ARTSURF.I.SD, ROADDEN.I.SD, SERAREA.I.SD, UI, UI.cat, UI.N, UI.logit,
                                           DI, DI.N)
       
        table(nchar(SPAT.SC$SC))
        # known error! - add the zero to the "SC"
        
        SPAT.SC$SC[nchar(SPAT.SC$SC)==9] <- paste0("0", SPAT.SC$SC[nchar(SPAT.SC$SC)==9])
        
        # test if the census tracts coincide
        sum(SMRsc$SC %in% unique(SPAT.SC$SC)) # looks alright
        
        SPAT.SC$SC <- as.factor(SPAT.SC$SC)        
        SPAT.SC <- SPAT.SC %>% left_join(SMRsc, by="SC")
        
        
        ##### Needs to be doublechecked
        # SPAT.IND <- INMO.SC %>% dplyr::select(SC,sexo,estudios4, tenen, NATIONAL, event)
        # 
        # SPAT.IND$SC <- as.factor(SPAT.IND$SC)
        # 
        # SPAT.IND <- SPAT.IND %>% group_by(SC) %>%
        #             summarise_all(funs(sum), event=event)
        
        
    ### ---------------------------------------------------------------------------------------- ###
      
sum(d$RT_CODIGO %in% unique(SPAT.SC$SC))
# problem: more census sections because of administrative changes  
# 5054 census tracts are the same

# join what you get
d <- d %>% mutate(SC = RT_CODIGO) %>%
            left_join(SPAT.SC, by="SC")

### Now the data goes back to the spatial object
ANDALUS.SC@data <- d

# Map by SMR
spplot(ANDALUS.SC,"SMRe") ### Be careful with the missings
# Map by UI
spplot(ANDALUS.SC,"UI.N") ### Be careful with the missings
# Map by DI
spplot(ANDALUS.SC,"DI.N") ### Be careful with the missings ----------------------- !!!

###### %%%% For now listwise deletion of the census tracts we don´t have information for

ANDALUS.SC <- subset(ANDALUS.SC, !is.na(UI))

require(maptools)
require(spdep)
require(rgdal)

writePolyShape(ANDALUS.SC, "data/ANDALUS_SC")
### ---------------------------------------------------------------------------------------- ###

  # Check degree of urbanization by zooming into Seville (Province)

  SEV <- subset(ANDALUS.SC,
              PROVINCIA %in%  c("Sevilla")
  )

  spplot(SEV, "UI.N")

  # Municipality level (biggest municipality - population-wise)
  MAL <- subset(ANDALUS.SC, MUNICIPIO %in% c("MÃ¡laga"))
  spplot(MAL, "UI.N")
  spplot(MAL,"DI.N")
  
  # Municipality 2 - Sevilla
  SEV.2 <- subset(ANDALUS.SC, MUNICIPIO %in% c("Sevilla"))
  SEV.UI.plot <- spplot(SEV.2, "UI.N")
  SEV.DI.plot <- spplot(SEV.2,"DI.N")
  
  library("gridExtra")
  library("lattice")
  
  grid.arrange(SEV.UI.plot, SEV.DI.plot, ncol=2, nrow=1)
  
  ######################################################### UI - seems to measure urbanicity quite well
  
  
### Test for Spatial Autocorrelation

## a) Adjacency Matrix from the shape file

and.nb <- poly2nb(ANDALUS.SC)

# row-standardised weight matrix - increased the weights of links from observation with few neighbours
and.lw <- nb2listw(and.nb, style = "W", zero.policy=TRUE)

#################
### Moran's I ###
#################

## Spatial autocorrelation test by single variable


  # Population
  moran.test(ANDALUS.SC$POBLACION, and.lw, zero.policy=TRUE)
  # SMR                                           ### Be careful with the missings
  ANDALUS.SC$SMRe <- as.numeric(ANDALUS.SC$SMRe)
  moran.test(ANDALUS.SC$SMRe, and.lw, zero.policy=TRUE)
  # Urbanicity indicator
  moran.test(ANDALUS.SC$UI.N, and.lw, zero.policy=TRUE)
  # Deprivation indicator
  moran.test(ANDALUS.SC$DI.N, and.lw, zero.policy=TRUE)
  
###  Lee’s statistic for comparison of two variables:
  n <- nrow(ANDALUS.SC)
  lee(ANDALUS.SC$UI, ANDALUS.SC$SMRe, and.lw, n, zero.policy=TRUE)$L
  # Urban - Mortality Spatial
  lee.test(ANDALUS.SC$UI.N, ANDALUS.SC$SMRe, and.lw, alt = "two.sided", zero.policy=TRUE)
  # Deprivation - Mortality Spatial
  lee.test(ANDALUS.SC$DI.N, ANDALUS.SC$SMRe, and.lw, alt = "two.sided", zero.policy=TRUE)
  


### ---------------------------------------------------------------------------------------- ###
###                          3. Survival Analysis (Kaplan-Meier, simple PH Model)            ###
### ---------------------------------------------------------------------------------------- ###

  
  ### Change reference categories

  # sex
  INMO.SC <- within(INMO.SC, sexo <- relevel(sexo, ref = "female"))  
  # Civil status
  INMO.SC <- within(INMO.SC, ecivil <- relevel(ecivil, ref = "Married"))
  # dependency variable
  INMO.SC <- INMO.SC %>% mutate(dep = as.factor(ifelse(dependiente=="No","not dependent", "dependent")))
  INMO.SC <- within(INMO.SC, dep <- relevel(dep, ref = "not dependent"))
  # Housing ownership
  INMO.SC <- within(INMO.SC, tenen <- relevel(tenen, ref = "Owns House/Apartment"))
  
  
# Note: Using two time points accounts for left-truncation

# -----------
# a) General Survival Distribution - Parametric Estimate (KME)

KM1 <- survfit(coxph(Surv(time  = age.entry,
                          time2 = age.exit,
                          event = event)~1, data = INMO.SC),type="kaplan-meier")
KM1

# plot
  tidy(KM1) %>% ggplot(aes(x=time,y=estimate)) +
    geom_step() +
    scale_x_continuous(name = "Age") +
    theme_bw()
  
# -----------
# b) Survival estimates by sex
  
KM.female <- survfit(Surv(time = age.entry,
                            time2 = age.exit,
                            event = event) ~ 1, data = subset(INMO.SC,sexo=="female"), type="kaplan-meier")
  
KM.male <- survfit(Surv(time = age.entry,
                                time2 = age.exit,
                                event = event) ~ 1, data = subset(INMO.SC,sexo=="male"), type="kaplan-meier")  

  KM.FEM <- tidy(KM.female) %>% dplyr::select(time,estimate) %>% mutate(sex = "female")
  KM.MAL <- tidy(KM.male) %>% dplyr::select(time, estimate) %>% mutate(sex = "male")
  
  KM.SEX <- union(KM.FEM,KM.MAL)
  
  KM.SEX %>% ggplot(aes(x=time,y=estimate,color=sex)) +
    geom_step() +
    scale_colour_manual(values = c("orange", "darkgrey"), name="")     +
    scale_x_continuous(name = "Age") +
    scale_y_continuous(name = "Estimated Survival Probability") +
    theme_bw()

# -----------
# c) Survival estimates by "urbanicity"
  
  KM2.rural <- survfit(Surv(time = age.entry,
                            time2 = age.exit,
                            event = event) ~ 1, data = subset(INMO.SC,UI.cat=="Rural"), type="kaplan-meier")
  
  KM2.perirural <- survfit(Surv(time = age.entry,
                                time2 = age.exit,
                                event = event) ~ 1, data = subset(INMO.SC,UI.cat=="Predom. Rural"), type="kaplan-meier")
  
  KM2.periurban <- survfit(Surv(time = age.entry,
                                time2 = age.exit,
                                event = event) ~ 1, data = subset(INMO.SC,UI.cat=="Peri-Urban"), type="kaplan-meier")
  
  KM2.urban <- survfit(Surv(time = age.entry,
                            time2 = age.exit,
                            event = event) ~ 1, data = subset(INMO.SC,UI.cat=="Urban"), type="kaplan-meier")
  
  KM2.R <- tidy(KM2.rural) %>% dplyr::select(time,estimate) %>% mutate(UI = "rural")
  KM2.PR <- tidy(KM2.perirural) %>% dplyr::select(time, estimate) %>% mutate(UI = "predom. rural")
  KM2.PU <- tidy(KM2.periurban) %>% dplyr::select(time, estimate) %>% mutate(UI = "peri-urban")
  KM2.U <- tidy(KM2.urban) %>% dplyr::select(time, estimate) %>% mutate(UI = "urban")
  
  KM.UI <- union(KM2.R,KM2.PR) %>% union(KM2.PU) %>% union(KM2.U)
  
        # delete: 
        rm(KM2.rural,KM2.perirural,KM2.periurban,KM2.urban)
        
        
  # plot
  KM.UI %>% ggplot(aes(x=time,y=estimate,color=UI)) +
            geom_step() +
            scale_color_brewer(name=" ", palette = "Set1") +
            scale_x_continuous(name = "Age") +
            scale_y_continuous(name = "Estimated Survival Probability") +
            theme_bw()
  ## there seems to be some higher mortality for urban dwellers until age 75 (minimal but proportional to the rest)
  
  # -----------
  # d) Survival estimates by "urbanicity" and sex
  
  # female - rural
  KM2.rural.f <- survfit(Surv(time = age.entry,
                            time2 = age.exit,
                            event = event) ~ 1, data = subset(INMO.SC,UI.cat=="Rural" & sexo=="female"), type="kaplan-meier")
  # female - predom. rural  
  KM2.perirural.f <- survfit(Surv(time = age.entry,
                                time2 = age.exit,
                                event = event) ~ 1, data = subset(INMO.SC,UI.cat=="Predom. Rural" & sexo=="female"), type="kaplan-meier")
  # female - peri-urban
  KM2.periurban.f <- survfit(Surv(time = age.entry,
                                time2 = age.exit,
                                event = event) ~ 1, data = subset(INMO.SC,UI.cat=="Peri-Urban" & sexo=="female"), type="kaplan-meier")
  # female - urban
  KM2.urban.f <- survfit(Surv(time = age.entry,
                            time2 = age.exit,
                            event = event) ~ 1, data = subset(INMO.SC,UI.cat=="Urban" & sexo=="female"), type="kaplan-meier")
  # male - rural
  KM2.rural.m <- survfit(Surv(time = age.entry,
                            time2 = age.exit,
                            event = event) ~ 1, data = subset(INMO.SC,UI.cat=="Rural" & sexo=="male"), type="kaplan-meier")
  # male - predom. rural
  KM2.perirural.m <- survfit(Surv(time = age.entry,
                                time2 = age.exit,
                                event = event) ~ 1, data = subset(INMO.SC,UI.cat=="Predom. Rural" & sexo=="male"), type="kaplan-meier")
  # male - peri-urban
  KM2.periurban.m <- survfit(Surv(time = age.entry,
                                time2 = age.exit,
                                event = event) ~ 1, data = subset(INMO.SC,UI.cat=="Peri-Urban" & sexo=="male"), type="kaplan-meier")
  # male - urban
  KM2.urban.m <- survfit(Surv(time = age.entry,
                            time2 = age.exit,
                            event = event) ~ 1, data = subset(INMO.SC,UI.cat=="Urban"), type="kaplan-meier")
  
  KM2.R.F <- tidy(KM2.rural.f) %>% dplyr::select(time,estimate) %>% mutate(UI = "rural") %>% mutate(sex="female")
  KM2.PR.F <- tidy(KM2.perirural.f) %>% dplyr::select(time, estimate) %>% mutate(UI = "predom. rural") %>% mutate(sex="female")
  KM2.PU.F <- tidy(KM2.periurban.f) %>% dplyr::select(time, estimate) %>% mutate(UI = "peri-urban") %>% mutate(sex="female")
  KM2.U.F <- tidy(KM2.urban.f) %>% dplyr::select(time, estimate) %>% mutate(UI = "urban") %>% mutate(sex="female")
  
  KM2.R.M <- tidy(KM2.rural.m) %>% dplyr::select(time,estimate) %>% mutate(UI = "rural") %>% mutate(sex="male")
  KM2.PR.M <- tidy(KM2.perirural.m) %>% dplyr::select(time, estimate) %>% mutate(UI = "predom. rural") %>% mutate(sex="male")
  KM2.PU.M <- tidy(KM2.periurban.m) %>% dplyr::select(time, estimate) %>% mutate(UI = "peri-urban") %>% mutate(sex="male")
  KM2.U.M <- tidy(KM2.urban.m) %>% dplyr::select(time, estimate) %>% mutate(UI = "urban") %>% mutate(sex="male")
  
  KM.UI.SEX <- union(KM2.R.F,KM2.PR.F) %>% union(KM2.PU.F) %>% union(KM2.U.F) %>% union(KM2.R.M) %>% union(KM2.PR.M) %>% 
                union(KM2.PU.M) %>% union(KM2.U.M)
  
  # delete: 
  rm(KM2.rural.f,KM2.perirural.f,KM2.periurban.f,KM2.urban.f, KM2.rural.m,KM2.perirural.m,KM2.periurban.m,KM2.urban.m,
     KM2.PR.F,KM2.PR.M,KM2.PR, KM2.PU.F, KM2.PU.M, KM2.PU, KM2.U.F, KM2.U.M, KM2.U, KM2.R.M, KM2.R.F, KM2.R)
  
  # plot
  KM.UI.SEX %>% ggplot(aes(x=time,y=estimate,color=UI)) +
    geom_step() +
    facet_grid(.~ sex) +
    scale_color_brewer(name=" ", palette = "Set1") +
    scale_x_continuous(name = "Age") +
    scale_y_continuous(name = "Estimated Survival Probability") +
    theme_bw()                                                        # that is very confusing: survival advantage of urban males
                                                                      # needs to be controlled for wealth and others
  
  
  
### -------------------------------------------------------------------------------- ###
###                          4. Mixed Effects Cox Model                              ###
### -------------------------------------------------------------------------------- ###
  
library(coxme)  

stem(table(INMO.SC$SC))  

### Simple model without random effects (only "Urbanicity" as covariate)
fit.1 <- coxph(Surv(time = age.entry,
                    time2 = age.exit,
                    event = event) ~ UI.N, data=INMO.SC)

### Simple model WITH random effects (only "Urbanicity" as covariate)
   
  # Logic - intercept (effect) per Census Tract (group)

fit.2 <- coxme(Surv(time = age.entry,
                    time2 = age.exit,
                    event = event) ~ UI.N + (1|SC), data=INMO.SC)

print(fit.2)
### Compare the log-partial likelihood to the model without random effects

summary(fit.1)


anova(fit.1, fit.2)

## compare AICs
AIC(fit.2) - AIC(fit.1)
# difference of AICs in favor of the model which accounts spatial

  ### --------------------------------------------------------------------------------------------------------------------- ###
  ### --------------------------------------------------------------------------------------------------------------------- ###
  ### --------------------------------------------------------------------------------------------------------------------- ###
  ### Summary graphs (code by Thernau 2017)

  # event counts by SC 
  nevents <- with(INMO.SC, tapply(event, SC, sum, na.rm=T))
  # person years by SC
  per.years <- with(INMO.SC, tapply(age.exit - 12, SC, sum, na.rm=T))
  count <- with(INMO.SC, tapply(event, SC, 
                              (function(x) sum(!is.na(x)))))

  ### Plotting events per census tracts by estimated risks (random effects model)
  plot(nevents, exp(ranef(fit.2)[[1]]), log='y',
     xlab="Number of events per Census Tract",
     ylab="Estimated risk by CT")
  abline(h=1, lty=2)
  ### --------------------------------------------------------------------------------------------------------------------- ###
  ### --------------------------------------------------------------------------------------------------------------------- ###
  ### --------------------------------------------------------------------------------------------------------------------- ###

######################
### Testing models ###
######################

  
### Model 1 - sex, dependency, civil status, cohort
  Mod.1 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ sexo + dependiente + ecivil + fnac, data = INMO.SC)
  
  Mod.1.ran <- coxme(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ sexo + dependiente + ecivil + fnac + (1|SC), data = INMO.SC)
  
  ## Compare Model Fit
  AIC(Mod.1.ran)-AIC(Mod.1)
  
### Model 2 - Model 1 + education, car ownership, housing ownership
  
  Mod.2 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ sexo + dep + ecivil + fnac + estudios4 + tenen + vehic,
                      data = INMO.SC)
  summary(Mod.2)
  
  AIC(Mod.2)-AIC(Mod.1) # Model 2 fits the data much better than Model 1 (difference 1370.697)
  
  Mod.2.ran <- coxme(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ sexo + dep + ecivil + fnac + estudios4 + tenen + vehic + (1|SC),
                 data = INMO.SC)
  
  AIC(Mod.2.ran)-AIC(Mod.2) # Model with random effects is more likely to minimize the information loss
  
  
### Model 3 - Model 2 + Urbanicity effects (UI.N)

  Mod.3 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ sexo + dep + ecivil + estudios4 + tenen + vehic + UI.N,
                 data = INMO.SC)
  summary(Mod.3)  
  
  AIC(Mod.3)-AIC(Mod.2) # Model 3 fits the data much better than Model 2 (difference 879.484)
  
  
  Mod.3.ran <- coxme(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ sexo + dep + ecivil + estudios4 + tenen + vehic + OCCUP2 + fnac  + UI.N + (1|SC),
                     data = INMO.SC)
  
  AIC(Mod.3.ran)-AIC(Mod.3) # Model with random effects is more likely to minimize the information loss (difference 888.4247)
  
### Model 4 - Model 3 + Area Deprivation (DI.N)
  
  Mod.4 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ sexo + dep + ecivil + estudios4 + tenen + vehic + UI.N + DI.N,
                 data = INMO.SC)
  summary(Mod.4)  
  
  AIC(Mod.4)-AIC(Mod.3) # Model 4 fits the data much better than Model 3 (difference 75.80463)
  
  
  Mod.4.ran <- coxme(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ sexo + dep + ecivil + estudios4 + tenen + vehic + fnac  + UI.N + DI.N + (1|SC),
                     data = INMO.SC)
  
  AIC(Mod.4.ran)-AIC(Mod.4) # Model with random effects is more likely to minimize the information loss (difference 865.8751)
  
  
  AIC(Mod.4.ran) - AIC(Mod.3.ran) # just 53 difference
  
  

  