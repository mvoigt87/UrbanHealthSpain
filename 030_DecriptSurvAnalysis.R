#### PAA - Urban Environment and Mortality Differentials in South Spain ####
#### ------------------------------------------------------------------ ####

### ---------------------------------------------------------------------------------------- ###
### Plan
###
### 1. Descriptive Exploration
### 2. Check for Spatial Auto-Correlation
### 3. Survival Analysis (Kaplan-Meier, simple Cox PH model)
### 4. Mixed Effects Cox Model + Model Comparison
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
library(RColorBrewer)
library("gridExtra")
library("lattice")



# 0.2 load data set
load("data/025_INDMOR-CT.RData")

###
### Extract the age groups necessary for the analysis
###
    # INMO.SC <- INMO.SC %>% dplyr::filter(age.entry>=35 & age.entry<=95)
    # reduces data set to 372007 individuals

    INMO.SC <- INMO.SC %>% dplyr::filter(age.entry>=35 & age.entry<=80)
    # reduces data set to 351,769 individuals
    
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

## Urban Indicator 
## ---------------

# (to change go back to "010_CreateContextData.R")
round(prop.table(table(INMO.SC$event,INMO.SC$UI.cat),2),2)
chisq.test(INMO.SC$event,INMO.SC$UI, simulate.p.value = FALSE)
  # Both extremes seem to have higher mortality - but it need to be checked for age and population structure


## Environmental factors
## ---------------------

# Noise
INMO.SC %>% mutate(event = as.factor(ifelse(event==1,"dead","alive"))) %>% ggplot(aes(INMO.SC$IP_RUIDOS, fill=event)) +  
  geom_histogram(binwidth = 0.1) + 
  scale_fill_discrete(name=" ") + 
  scale_x_continuous(name=" ") +
  scale_y_continuous(name=" ") +
  theme_bw()

# Pollution
summary(INMO.SC$IP_CONTAM[INMO.SC$event==0])
summary(INMO.SC$IP_CONTAM[INMO.SC$event==1])

# Cleanness
summary(INMO.SC$IP_LIMPIEZA[INMO.SC$event==0])
summary(INMO.SC$IP_LIMPIEZA[INMO.SC$event==1])


## Socio-economic factors
## ---------------------
# Delinquency
summary(INMO.SC$IP_DELINC[INMO.SC$event==0])
summary(INMO.SC$IP_DELINC[INMO.SC$event==1])


# Percentage Workers Agriculture by census tract
INMO.SC %>% mutate(event = as.factor(ifelse(event==1,"dead","alive"))) %>% ggplot(aes(PCT_TENENPROP, fill=event)) +  
  geom_histogram(binwidth = 1) + 
  scale_fill_discrete(name=" ") + 
  scale_x_continuous(name=" ") +
  scale_y_continuous(name=" ") +
  theme_bw()

# IP_HOGMONOP, IP_EDBAJO_TOTAL

## Further Test
## ------------

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
### ------------------------------------------------------------------------------------------------ ###
### Change the employed to unemployed
    summary(INMO.SC$PCT_OCUPADOS)
  
    INMO.SC <- INMO.SC %>% mutate(PCT_UNEMPL = 100 - PCT_OCUPADOS)
    
    INMO.SC %>% mutate(event = as.factor(ifelse(event==1,"dead","alive"))) %>% ggplot(aes(PCT_UNEMPL, fill=event)) +  
      geom_histogram(binwidth = 1) + 
      scale_fill_discrete(name=" ") + 
      scale_x_continuous(name=" ") +
      scale_y_continuous(name=" ") +
      theme_bw()
    
### ------------------------------------------------------------------------------------------------ ###


# Percentage Workers Agriculture by census tract
INMO.SC %>% mutate(event = as.factor(ifelse(event==1,"dead","alive"))) %>% ggplot(aes(PCT_AGRIC, fill=event)) +  
  geom_histogram(binwidth = 1) + 
  scale_fill_discrete(name=" ") + 
  scale_x_continuous(name=" ") +
  scale_y_continuous(name=" ") +
  theme_bw()


### ------------------------------------------------------------------------------------------------ ###
###                          2. Spatial Autocorrelation => see file 035                              ###
### ------------------------------------------------------------------------------------------------ ###


### ---------------------------------------------------------------------------------------- ###
###                          3. Survival Analysis (Kaplan-Meier, simple PH Model)            ###
### ---------------------------------------------------------------------------------------- ###

  
  ### Change reference categories

## 0.5 Change reference categories for individual variables
# sex
INMO.SC.2 <- within(INMO.SC.2, sexo <- relevel(sexo, ref = "female"))  
# Civil status
INMO.SC.2 <- within(INMO.SC.2, ecivil <- relevel(ecivil, ref = "Married"))
# dependency variable
INMO.SC.2 <- INMO.SC.2 %>% mutate(dep = as.factor(ifelse(dependiente=="No","not dependent", "dependent")))
INMO.SC.2 <- within(INMO.SC.2, dep <- relevel(dep, ref = "not dependent"))
# Housing ownership
INMO.SC.2 <- within(INMO.SC.2, tenen <- relevel(tenen, ref = "Owns House/Apartment"))

  
  ##########################################################################################################################
  # Number of deaths by census tract (should be more than 1 for variability)
  
      # aggdata <-aggregate(INMO.SC$event==1, by=list(INMO.SC$SC),FUN=sum, na.rm=TRUE)
      # summary(aggdata$x)
      # 
      # table(aggdata$x>=2) # there are less than 2 deaths in 74 census tracts over the years!
      # 
      # # Extract the cases with less than 2 cases
      # 
      # colnames(aggdata) <- c("SC","agg.death")
      # 
      # INMO.SIN2 <- INMO.SC %>% left_join(aggdata, by="SC") %>% 
      #   dplyr::filter(agg.death>=2)
      # 
      # rm(aggdata)
  ###########################################################################################################################
  
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
  UI.plot <- KM.UI %>% ggplot(aes(x=time,y=estimate,color=UI)) +
            geom_step() +
            scale_color_manual(values=c("green2", "goldenrod2", "dodgerblue1", "orangered"), name=" ",  
                                 breaks=c("rural","predom. rural","peri-urban","urban")) +
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
  
 
  # -----------
  # e) Survival estimates by development indicator
  
  # three categories by 3-rds
  quantile(INMO.SC$DI.N, c(.33, .66))
  
  INMO.SC <- INMO.SC %>% mutate(DI.cat = as.factor(ifelse(DI.N<=-0.4606759, "low", ifelse(DI.N<=0.3668282,"medium","high"))))
  
  table(INMO.SC$DI.cat)
  
  KM4.low <- survfit(Surv(time = age.entry,
                            time2 = age.exit,
                            event = event) ~ 1, data = subset(INMO.SC,DI.cat=="low"), type="kaplan-meier")
  
  KM4.medium <- survfit(Surv(time = age.entry,
                                time2 = age.exit,
                                event = event) ~ 1, data = subset(INMO.SC,DI.cat=="medium"), type="kaplan-meier")
  
  KM4.high <- survfit(Surv(time = age.entry,
                                time2 = age.exit,
                                event = event) ~ 1, data = subset(INMO.SC,DI.cat=="high"), type="kaplan-meier")
  

  KM4.low <- tidy(KM4.low) %>% dplyr::select(time,estimate) %>% mutate(DI = "Low")
  KM4.med <- tidy(KM4.medium) %>% dplyr::select(time, estimate) %>% mutate(DI = "Medium")
  KM4.hig <- tidy(KM4.high) %>% dplyr::select(time, estimate) %>% mutate(DI = "High")
  
  KM.DI <- union(KM4.low,KM4.med) %>% union(KM4.hig)
  
  # delete: 
  rm(KM4.low, KM4.med, KM4.hig, KM4.medium, KM4.high)
  
  
  # plot
  DI.plot <- KM.DI %>% ggplot(aes(x=time,y=estimate,color=DI)) +
    geom_step() +
    scale_color_manual(values=c("green2", "goldenrod2", "dodgerblue1"), name=" ",  
                       breaks=c("Low","Medium","High")) +
    scale_x_continuous(name = "Age") +
    scale_y_continuous(name = " ") +
    theme_bw()
  
  grid.arrange(UI.plot, DI.plot, ncol=2)
  
### -------------------------------------------------------------------------------- ###
###                          4. Mixed Effects Cox Model                              ###
### -------------------------------------------------------------------------------- ###
  
library(coxme)  

stem(table(INMO.SC$SC))  

##########################################################
### %%%%%%%%%%%%% Models with Indicator effects only -  UI

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


###########################################################
                # ### %%%%%%%%%%%%% Models with Indicator effects only -  DI
                # 
                # 
                # ### Simple model without random effects (only "Urbanicity" as covariate)
                # fit.3 <- coxph(Surv(time = age.entry,
                #                     time2 = age.exit,
                #                     event = event) ~ DI.N, data=INMO.SC)
                # 
                # ### Simple model WITH random effects (only "Urbanicity" as covariate)
                # 
                # # Logic - intercept (effect) per Census Tract (group)
                # 
                # fit.4 <- coxme(Surv(time = age.entry,
                #                     time2 = age.exit,
                #                     event = event) ~ DI.N + (1|SC), data=INMO.SC)
                # 
                # print(fit.4)
                # ### Compare the log-partial likelihood to the model without random effects
                # 
                # summary(fit.3)
                # anova(fit.3, fit.4)
                # 
                # ## compare AICs
                # AIC(fit.4) - AIC(fit.3)
                #         # difference of AICs in favor of the model which accounts spatial



### Simple model without random effects (only "Urbanicity" as covariate)
fit.3 <- coxph(Surv(time = age.entry,
                    time2 = age.exit,
                    event = event) ~ DI.N, data=INMO.SC)

### Simple model WITH random effects (only "Urbanicity" as covariate)

# Logic - intercept (effect) per Census Tract (group)

fit.4 <- coxme(Surv(time = age.entry,
                    time2 = age.exit,
                    event = event) ~ DI.N + (1|SC), data=INMO.SC)

print(fit.4)
### Compare the log-partial likelihood to the model without random effects

summary(fit.3)
anova(fit.3, fit.4)

## compare AICs
AIC(fit.4) - AIC(fit.3)
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

  
  
  
                                            ####################
                                            ### Final Models ###

  
### Model 1 - Urban Indicator
  
  # Cox model
  Mod.1 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ UI.N, data = INMO.SC)
  
  # spatially distributed model
  Mod.1.ran <- coxme(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ UI.N + (1|SC), data = INMO.SC)
  
  ## Compare Model Fit
  AIC(Mod.1)-AIC(Mod.1.ran) # 323.0393 difference - spatial models reduces information loss

  AIC(Mod.1)-AIC(Mod.1.ran) # (difference 349.7346 - model with spatial effects performs better)

 
   # with age bound 95 (difference 349.7346 - model with spatial effects performs better)
  
                # ### Model 2 - Environment Effects
                #   
                #   Mod.2 <- coxph(Surv(time = age.entry,
                #                       time2 = age.exit,
                #                       event = event) ~ DI.N,
                #                       data = INMO.SC)
                #   summary(Mod.2)
                #   
                #   AIC(Mod.1)-AIC(Mod.2) # Model 2 fits the data much better than Model 1 (difference 46.87031)
                #   
                #   Mod.2.ran <- coxme(Surv(time = age.entry,
                #                       time2 = age.exit,
                #                       event = event) ~ DI.N + (1|SC),
                #                  data = INMO.SC)
                #   
                #   AIC(Mod.2)-AIC(Mod.2.ran) # Model with random effects is more likely to minimize the information loss (differences 339.2269)
  

  AIC(Mod.2)-AIC(Mod.2.ran) # Model with random effects is more likely to minimize the information loss (differences 339.2269)
  
  
### Model 3 - Urbanicity effects (UI.N) + Environment Effects at the same time

  Mod.3 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ UI.N + DI.N,
                 data = INMO.SC)
  summary(Mod.3)  
   
 AIC(Mod.2)-AIC(Mod.3) # Model 3 fits the data only slightly than Model 2 (difference 16.51)

  

### Model 2 - Urbanicity effects (UI.N) + Environment effects (cleanness, air pollution, Noise)
  
# Cox model
  Cox.2 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ UI.N + sexo + dep + ecivil + fnac + estudios4 + tenen + vehic, data = INMO.SC)
  
# spatially distributed model
  Mod.2.ran <- coxme(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ UI.N + sexo + dep + ecivil +  fnac + estudios4 + tenen + 
                                           vehic + (1|SC), data = INMO.SC)
  

  
### Model 3 - Model 2 + Socio-cultural effects (delinquency, percentage_employed, Pct_Agriculture)

# Cox model
  Cox.3 <- coxph(Surv(time = age.entry,
                       time2 = age.exit,
                       event = event) ~ UI.N  + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic +
                                        IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS, data = INMO.SC)

# Spatially distributed model
  Mod.3.ran <- coxme(Surv(time = age.entry,
                           time2 = age.exit,
                           event = event) ~ UI.N  + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic +
                                            IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + (1|SC), data = INMO.SC)
  
  
    
            # ### Model 3 - Urbanicity effects (UI.N) + Environment Effects at the same time
            # 
            #   Mod.3 <- coxph(Surv(time = age.entry,
            #                       time2 = age.exit,
            #                       event = event) ~ UI.N + DI.N,
            #                  data = INMO.SC)
            #   summary(Mod.3)  
            #    
            #  AIC(Mod.2)-AIC(Mod.3) # Model 3 fits the data only slightly than Model 2 (difference 16.51)
            #   
            #   
            #   Mod.3.ran <- coxme(Surv(time = age.entry,
            #                           time2 = age.exit,
            #                           event = event) ~ UI.N + DI.N + (1|SC),
            #                      data = INMO.SC)
            #   
            #   AIC(Mod.3)-AIC(Mod.3.ran) # Model with random effects is more likely to minimize the information loss (difference 333.2714)
            #   
### Model 4 - Model 3 + Individual Level Effects
  
  Cox.4 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ UI.N  + sexo + dep + ecivil + fnac + estudios4 + tenen + vehic + 
                                       IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + PCT_UNEMPL + PCT_SOLTEROS,
                 data = INMO.SC)
  summary(Mod.4)  
  
  AIC(Mod.3)-AIC(Mod.4) # Model 4 fits the data much better than Model 3 as the individual variables enter (difference 8379.086)
  
  
  Mod.4.ran <- coxme(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ UI.N + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic +
                                           IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + PCT_UNEMPL + 
                                           PCT_SOLTEROS + IP_DELINC + (1|SC),
                          data = INMO.SC)
  
  AIC(Mod.4) - AIC(Mod.4.ran) # Model with random effects is more likely to minimize the information loss (154.0891 difference )
  

  
  
  #### Further significance tests
  
  ### the most common way to do this is to use a likelihood ratio test, i.e. fit the full and reduced models 
  ### (the reduced model is the model with the focal variance(s) set to zero).

  library(lme4)
  
  
  m2 <- lmer(felev ~ DI.N + (1|SC)+(0+DI.N|SC),data = INMO.SC, REML=FALSE)
  m1 <- update(m2,.~DI.N + (1|SC))
  m0 <- lm(felev ~ DI.N,data = INMO.SC)
  anova(m2,m1,m0) 
  # two sequential tests - highly significant results
  # -------------------------------------------------
  #      Df      AIC      BIC    logLik   deviance   Chisq Chi    Df  Pr(>Chisq)    
  #   m0  3 -1598907 -1598874    799456   -1598913                              
  #   m1  4 -9547151 -9547108   4773580   -9547159    7948246      1  < 2.2e-16 ***
  #   m2  5 -9831402 -9831348   4915706   -9831412     284253      1  < 2.2e-16 ***
  
  # With recent versions of lme4, goodness-of-fit (deviance) can be compared between (g)lmer and (g)lm models, 
  # although anova() must be called with the mixed ((g)lmer) model listed first. Keep in mind that LRT-based null hypothesis tests
  # are conservative when the null value (such as σ2=0) is on the boundary of the feasible space; in the simplest case 
  # (single random effect variance), the p-value is approximately twice as large as it should be (Pinheiro and Bates 2000).
  
  ### The model including random effects significantly improves the model fit
  
  ###############################################################################################################
  
  
  ### For the latex table output
  
  stargazer(Mod.1, Mod.2, Mod.3, Mod.4, title ="Cox PH Model",no.space=F, 
            ci=F, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Hazard Ratios"),
            covariate.labels=c("Degree Urbanicity", "Development Indicator","Male","Physically Dependend", 
                               "Single","Widowed","Divorced/Separated", "Birth Cohort", "No or Incomplete Educ.",
                               "Primary/Secondary Educ.", "Does not Own House","Does not Own a Car"),
            single.row=F, apply.coef = exp)


  
  ###############################################################################################################
  ###############################################################################################################
  ###############################################################################################################
  ###############################################################################################################
  
  
  ### Test the home stay variable
  
  Mod.5 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ UI.N + DI.N + sexo + dep + ecivil + fnac + estudios4 + tenen + vehic + athome,
                 data = INMO.SC)
  
  summary(Mod.5)
  
  AIC(Mod.5)-AIC(Mod.4) # difference 381.3571 + working has an protective effect
  
  
  
  
  ### --------------------------- ###  
  ### Graphs for the presentation:
  ### --------------------------- ###
  
  library(forestplot)
  library(survminer)
  

  
  ## Test - ahh not really beautiful
  ggforest(fit.3)
  
  m2 <- lmer(felev ~ UI.N + (1|SC)+(0+UI.N|SC),data = INMO.SC, REML=FALSE)
  m1 <- update(m2,.~UI.N + (1|SC))
  m0 <- lm(felev ~ UI.N,data = INMO.SC)
  anova(m2,m1,m0) 
  # two sequential tests - highly significant results
  # -------------------------------------------------
  #      Df      AIC      BIC    logLik   deviance   Chisq Chi    Df  Pr(>Chisq)    
  #   m0  3 -1598907 -1598874    799456   -1598913                              
  #   m1  4 -9547151 -9547108   4773580   -9547159    7948246      1  < 2.2e-16 ***
  #   m2  5 -9831402 -9831348   4915706   -9831412     284253      1  < 2.2e-16 ***
  

  # With recent versions of lme4, goodness-of-fit (deviance) can be compared between (g)lmer and (g)lm models, 
  # although anova() must be called with the mixed ((g)lmer) model listed first. Keep in mind that LRT-based null hypothesis tests
  # are conservative when the null value (such as σ2=0) is on the boundary of the feasible space; in the simplest case 
  # (single random effect variance), the p-value is approximately twice as large as it should be (Pinheiro and Bates 2000).


  
  ###############################################################################################################
  
  
  ### For the latex table output
  
  stargazer(Mod.1, Mod.2, Mod.3, Mod.4, title ="Cox PH Model",no.space=F, 
            ci=F, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Hazard Ratios"),
            covariate.labels=c("Degree Urbanicity", "Perceived Cleaness", "Perceived Pollution", "Perceived Noise",
                               "Delinquency", "% Employed", "% Single HH","Male","Physically Dependend", 
                               "Single","Widowed","Divorced/Separated", "Birth Cohort", "No or Incomplete Educ.",
                               "Primary/Secondary Educ.", "Does not Own House","Does not Own a Car"),
            single.row=F, apply.coef = exp)

  
  
  
  
  ### %%%%%%%%%%%%% Models with Indicator components -  UI
  

  ### Simple model without random effects (only "Urbanicity" as covariate)
  fit.comp <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ POPDEN.I.SD + ARTSURF.I.SD + ROADDEN.I.SD + SERAREA.I.SD, data=INMO.SC)

  ### forest plot for the development indicator
  ### -----------------------------------------
  
  # label.dev <- c("Develop. Ind. (Cox)", "Develop. Ind. (Mix)", "Develop. Ind. (Urban)" , "Develop. Ind. (full)")
  
  label.dev <- c("Cox", "Ind", "Inds" , "Full")
  
  hazard.dev  <- c(1.027,1.029,1.042,1.045) 
  lower.dev <- c(01.02,1.020473,1.031397,1.03474)
  upper.dev <- c(1.035,1.037815,1.052775,1.03474)
  col.dev <- c("C","MC","MC","MC")
  df.dev <- data.frame(label.dev, hazard.dev, lower.dev, upper.dev,col.dev)
  
  # reverses the factor level ordering for labels after coord_flip()
  df.dev$label.dev <- factor(df.dev$label.dev, levels=rev(df.dev$label.dev))
  
  
  fp.dev <- ggplot(data=df.dev, aes(x=label.dev, y=hazard.dev, ymin=lower.dev, ymax=upper.dev, color=col.dev)) +
    geom_pointrange() + 
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab(" ") + ylab("Hazard Ratios (95% CI)") +
    scale_color_brewer(palette="Dark2", name=" ")              +
    theme_bw()  # use a white background
  print(fp.dev)
  
  grid.arrange(fp.urb, fp.dev, ncol=2)
  
  
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
    
  
  ### Additional stuff for the presentation and later use
  
  # Indicator Components of the UI (POPDEN.I+ARTSURF.I+ROADDEN.I+SERAREA.I)
  Mod.1.I <- coxph(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ POPDEN.I.SD+ARTSURF.I.SD+ROADDEN.I.SD+SERAREA.I.SD, data = INMO.SC)
  
  Mod.1.I.ran <- coxme(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ POPDEN.I.SD+ARTSURF.I.SD+ROADDEN.I.SD+SERAREA.I.SD + (1|SC), data = INMO.SC)

  
  summary(fit.comp)
  

  ### Simple model WITH random effects (only "Urbanicity" as covariate)

  # Forest plot
  # ggforest(Mod.1.I, data = INMO.SC)
  
  # adjusted survival curves
  # ggadjustedcurves(Mod.1.I, data=INMO.SC)

  
  # Logic - intercept (effect) per Census Tract (group)
  
  fit.comp.ran <- coxme(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ POPDEN.I.SD + ARTSURF.I.SD + ROADDEN.I.SD + SERAREA.I.SD + (1|SC), data=INMO.SC)

  summary(Mod.2.I)
  
  # Forest plot  
  ggforest(Mod.2.I, data=INMO.SC)
  
  Mod.2.I.ran <- coxme(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ IP_RUIDOS + IP_CONTAM + IP_LIMPIEZA + IP_DELINC + IP_HOGMONOP + (1|SC),
                     data = INMO.SC)
  
  print(Mod.2.I.ran)

  
  
  
  ### Model 2 without random effects (and single components)
  fit.comp.2 <- coxph(Surv(time = age.entry,
                         time2 = age.exit,
                         event = event) ~ POPDEN.I.SD + ARTSURF.I.SD + ROADDEN.I.SD + SERAREA.I.SD + sexo + 
                                          dep + ecivil + fnac + estudios4 + tenen + vehic, data=INMO.SC)
  
  summary(fit.comp.2)
  
  ### Model 2 without random effects (and single components) WITH random effects
  
  fit.comp.2.ran <- coxme(Surv(time = age.entry,
                             time2 = age.exit,
                             event = event) ~ POPDEN.I.SD + ARTSURF.I.SD + ROADDEN.I.SD + SERAREA.I.SD + sexo + 
                                              dep + ecivil + fnac + estudios4 + tenen + vehic + (1|SC), data=INMO.SC)
  
  fit.comp.2.ran
  
  
  ### %%%%%%%%%%%%% Full Models with Indicator components -  UI
  ### Simple model without random effects (only "Urbanicity" as covariate)
  Mod.4.comp <- coxph(Surv(time = age.entry,
                         time2 = age.exit,
                         event = event) ~ POPDEN.I.SD + ARTSURF.I.SD + ROADDEN.I.SD + SERAREA.I.SD +
                        sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic +
                        IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + PCT_UNEMPL + 
                        PCT_SOLTEROS + IP_DELINC, data=INMO.SC)
  

  summary(Mod.4.comp)

  
  # Logic - intercept (effect) per Census Tract (group)
  
  Mod.4.RAN.comp <- coxme(Surv(time = age.entry,
                         time2 = age.exit,
                         event = event) ~ POPDEN.I.SD + ARTSURF.I.SD + ROADDEN.I.SD + SERAREA.I.SD + 
                           + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic +
                           IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + PCT_UNEMPL + 
                           PCT_SOLTEROS + IP_DELINC +(1|SC), data=INMO.SC)

  ########################################################################################################################
  ########################################################################################################################
  
  ### Cut the time in periods and compare effect over time
  
  summary(INMO.SC$time)
  
  # compare the first 6 years and the last
  
  ## Change time variable
  INMO.SC.2 <- INMO.SC %>% mutate(abaja.2 = ifelse(abaja>2008,2008,abaja)) %>% 
    # new entry as well (reoccurrences!)
    mutate(aalta.2 = ifelse(aalta>2008, 2008, aalta)) %>% 
    # create a new time variable
    mutate(time.2 = ifelse(abaja.2-aalta.2==0,0.05,abaja.2-aalta.2)) %>% 
    # change event variable
    mutate(event.2 = ifelse(abaja>2008,0,event))
  
  
  INMO.SC.2 <- INMO.SC.2 %>% dplyr::mutate(age.entry.2 = aalta.2-fnac) %>%     ## census year 2002 - the birth year
    # the second age variable is for the exit from the risk set calculated as the inital age + the time to death or censorship 
    dplyr::mutate(age.exit.2 = age.entry.2+time.2)
  
  
  Cox.4 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ UI.N + IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + IP_DELINC + PCT_OCUPADOS +
                   PCT_SOLTEROS + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic, 
                 data=INMO.SC.2)
  
  
  Cox.4.2008 <- coxph(Surv(time = age.entry.2,
                      time2 = age.exit.2,
                      event = event.2) ~ UI.N + IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + IP_DELINC + PCT_OCUPADOS +
                   PCT_SOLTEROS + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic, 
                 data=INMO.SC.2)
  
  
  ### forest plot for the single indicator variables
  ### ----------------------------------------------
  
  
  ### Alternative Labels
  label.urb.I <- c("Population Den. (Cox)", "Population Den. (Mix)", "Artif. Surface (Cox)" , "Artif. Surface (Mix)",
                 "Road Den. (Cox)", "Road Den. (Mix)", "Service Area (Cox)", "Service Area (Mix)")
  
  hazard.urb.I  <- c(0.9925,0.9943,0.9817,0.9808,1.0295,1.0285,0.9995,0.9999) 
  lower.urb.I <- c(0.9760,0.9754,0.9698,0.9671,1.0149,1.0123,0.9903,0.9894)
  upper.urb.I <- c(1.0093,1.0132,0.9937,0.9946,1.0444,1.0447,1.0087,1.0104)
  col.urb.I <- c("C","MC","C","MC","C","MC","C","MC")
  df.urb.I <- data.frame(label.urb.I, hazard.urb.I, lower.urb.I, upper.urb.I,col.urb.I)
  
  # reverses the factor level ordering for labels after coord_flip()
  df.urb.I$label.urb.I <- factor(df.urb.I$label.urb.I, levels=rev(df.urb.I$label.urb.I))
  
  
  fp.urb.I <- ggplot(data=df.urb.I, aes(x=label.urb.I, y=hazard.urb.I, ymin=lower.urb.I, ymax=upper.urb.I, color=col.urb.I)) +
    geom_pointrange() + 
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab(" ") + ylab("Hazard Ratios (95% CI)") +
    scale_color_brewer(palette="Dark2", name=" ")              +
    theme_bw()   + # use a white background
    theme(legend.position="none")
  print(fp.urb.I)
  
  
  # theme(text = element_text(size=20),
  #        axis.text.x = element_text(angle=90, hjust=1)) 
  
  
  ### forest plot for the development indicator
  ### -----------------------------------------
  
  label.dev.I <- c("Noise (Cox)", "Noise (Mix)", "Pollution (Cox)", "Pollution (Mix)", "Cleanness (Cox)", "Cleanness (Mix)",
                 "Delinquency (Cox)", "Delinquency (Mix)", "Homogeneity (Cox)", "Homogeneity (Mix)")
  
  hazard.dev.I  <- c(0.9976,0.9976,1.0023,1.0023,1.0003,1.0003,1.0008,1.0008,1.0066,1.0069) 
  lower.dev.I <- c(0.9969,0.9969,1.0015,1.0014,0.9998,0.9998,1.0003,1.0002,1.0045,1.0045)
  upper.dev.I <- c(0.9982,0.9983,1.0030,1.0031,1.00008,1.0008,1.0013,1.0013,1.0087,1.0092)
  col.dev.I <- c("C","MC","C","MC","C","MC","C","MC","C","MC")
  df.dev.I <- data.frame(label.dev.I, hazard.dev.I, lower.dev.I, upper.dev.I,col.dev.I)
  
  # reverses the factor level ordering for labels after coord_flip()
  df.dev.I$label.dev.I <- factor(df.dev.I$label.dev.I, levels=rev(df.dev.I$label.dev.I))
  
  
  fp.dev.I <- ggplot(data=df.dev.I, aes(x=label.dev.I, y=hazard.dev.I, ymin=lower.dev.I, ymax=upper.dev.I, color=col.dev.I)) +
    geom_pointrange() + 
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab(" ") + ylab("Hazard Ratios (95% CI)") +
    scale_color_brewer(palette="Dark2", name=" ")              +
    theme_bw()  # use a white background
  print(fp.dev.I)
  
  # grid.arrange(fp.urb.I, fp.dev.I, ncol=2)
  
  
  ##########################################################################################################################
  ##########################################################################################################################
  
  ##########################################################################################################################
  ##########################################################################################################################  
  ### Model 4
  
  #                                         exp(coef) exp(-coef) lower .95 upper .95
  # POPDEN.I.SD                               0.9936     1.0064    0.9769    1.0106
  # ARTSURF.I.SD                              0.9890     1.0112    0.9768    1.0013
  # ROADDEN.I.SD                              1.0151     0.9851    1.0005    1.0299
  # SERAREA.I.SD                              0.9935     1.0066    0.9837    1.0033
  # IP_RUIDOS                                 0.9991     1.0009    0.9984    0.9998
  # IP_CONTAM                                 1.0018     0.9982    1.0010    1.0025
  # IP_LIMPIEZA                               1.0001     0.9999    0.9996    1.0006
  # IP_DELINC                                 1.0011     0.9989    1.0006    1.0016
  # IP_HOGMONOP                               1.0070     0.9931    1.0047    1.0092
  # sexomale                                  1.9027     0.5256    1.8715    1.9345
  # depdependent                              2.1725     0.4603    2.0731    2.2767
  # ecivilSingle                              1.3562     0.7373    1.3205    1.3929
  # ecivilWidowed                             1.1922     0.8388    1.1683    1.2165
  # ecivilDivorced/Sep                        1.4342     0.6972    1.3605    1.5119
  # fnac                                      0.9823     1.0180    0.9801    0.9845
  # estudios4No or Incomplete education       1.3706     0.7296    1.3203    1.4228
  # estudios4Primary/Lower Secondary Educ.    1.1301     0.8849    1.0853    1.1768
  # tenenDoes not Own House/Aptm.             1.1174     0.8949    1.0924    1.1430
  # vehicno car                               1.2047     0.8301    1.1845    1.2252
  
  # Mix Model
  
  #                                                 coef exp(coef)     se(coef)      z       p
  # POPDEN.I.SD                            -5.748144e-03 0.9942683 0.0093351014  -0.62 5.4e-01
  # ARTSURF.I.SD                           -1.199252e-02 0.9880791 0.0068088528  -1.76 7.8e-02
  # ROADDEN.I.SD                            1.435048e-02 1.0144539 0.0080007588   1.79 7.3e-02
  # SERAREA.I.SD                           -6.395995e-03 0.9936244 0.0054330979  -1.18 2.4e-01
  # IP_RUIDOS                              -9.133913e-04 0.9990870 0.0003929551  -2.32 2.0e-02
  # IP_CONTAM                               1.754974e-03 1.0017565 0.0004044607   4.34 1.4e-05
  # IP_LIMPIEZA                             6.567546e-05 1.0000657 0.0002665121   0.25 8.1e-01
  # IP_DELINC                               1.137936e-03 1.0011386 0.0002759977   4.12 3.7e-05
  # IP_HOGMONOP                             6.918135e-03 1.0069421 0.0012259096   5.64 1.7e-08
  # sexomale                                6.456597e-01 1.9072449 0.0084849856  76.09 0.0e+00
  # depdependent                            7.858456e-01 2.1942617 0.0241968016  32.48 0.0e+00
  # ecivilSingle                            3.066040e-01 1.3588028 0.0136885620  22.40 0.0e+00
  # ecivilWidowed                           1.755634e-01 1.1919176 0.0103700420  16.93 0.0e+00
  # ecivilDivorced/Sep                      3.666748e-01 1.4429286 0.0269925792  13.58 0.0e+00
  # fnac                                   -1.658722e-02 0.9835496 0.0011520391 -14.40 0.0e+00
  # estudios4No or Incomplete education     3.052609e-01 1.3569789 0.0192422479  15.86 0.0e+00
  # estudios4Primary/Lower Secondary Educ.  1.200712e-01 1.1275772 0.0207179259   5.80 6.8e-09
  # tenenDoes not Own House/Aptm.           1.114098e-01 1.1178529 0.0116916130   9.53 0.0e+00
  # vehicno car                             1.862262e-01 1.2046947 0.0086693759  21.48 0.0e+00
  
  
  ##########################################################################################################################
  ##########################################################################################################################
  
  #### !!! FULL MODEL EFFFECTS
  
  ### forest plot for the development indicator
  ### -----------------------------------------
  
  label.dev.IF <- c("Noise (Cox)", "Noise (Mix)", "Pollution (Cox)", "Pollution (Mix)", "Cleanness (Cox)", "Cleanness (Mix)",
                   "Delinquency (Cox)", "Delinquency (Mix)", "Homogeneity (Cox)", "Homogeneity (Mix)")
  
  hazard.dev.IF  <- c(0.9991,0.9991,1.0018,1.0017,1.0001,1.0001,1.0011,1.0011,1.0070,1.0069) 
  lower.dev.IF <- c(0.9984,0.9983,1.0010,1.0010,0.9996,0.9995,1.0006,1.0006,1.0047,1.0045)
  upper.dev.IF <- c(0.9998,0.9998,1.0025,1.0025,1.0006,1.0006,1.0016,1.0017,1.0092,1.0093)
  col.dev.IF <- c("C","MC","C","MC","C","MC","C","MC","C","MC")
  df.dev.IF <- data.frame(label.dev.IF, hazard.dev.IF, lower.dev.IF, upper.dev.IF,col.dev.IF)
  
  # reverses the factor level ordering for labels after coord_flip()
  df.dev.IF$label.dev.IF <- factor(df.dev.IF$label.dev.IF, levels=rev(df.dev.IF$label.dev.IF))
  
  
  fp.dev.IF <- ggplot(data=df.dev.IF, aes(x=label.dev.IF, y=hazard.dev.IF, ymin=lower.dev.IF, ymax=upper.dev.IF, color=col.dev.IF)) +
    geom_pointrange() + 
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab(" ") + ylab("Hazard Ratios (95% CI)") +
    scale_color_brewer(palette="Dark2", name=" ")              +
    theme_bw()  # use a white background
  print(fp.dev.IF)
  
  
  ### forest plot for the urban single indicator variables (Full Model)
  ### ------------------------------------------------------------
  
  
  ### Alternative Labels
  label.urb.IF <- c("Population Den. (Cox)", "Population Den. (Mix)", "Artif. Surface (Cox)" , "Artif. Surface (Mix)",
                    "Road Den. (Cox)", "Road Den. (Mix)", "Service Area (Cox)", "Service Area (Mix)")
  
  hazard.urb.IF  <- c(0.9936,0.9943,0.9890,0.9881,1.0151,1.0144,0.9935,0.9936) 
  lower.urb.IF <- c(0.9769,0.9760,0.9768,0.9747,1.0005,0.9988,0.9837,0.9830)
  upper.urb.IF <- c(1.0106, 1.0126,1.0013,1.0014,1.0299,1.0301,1.0033,1.004)
  col.urb.IF <- c("C","MC","C","MC","C","MC","C","MC")
  df.urb.IF <- data.frame(label.urb.IF, hazard.urb.IF, lower.urb.IF, upper.urb.IF,col.urb.IF)
  
  # reverses the factor level ordering for labels after coord_flip()
  df.urb.IF$label.urb.IF <- factor(df.urb.IF$label.urb.IF, levels=rev(df.urb.IF$label.urb.IF))
  
  
  fp.urb.IF <- ggplot(data=df.urb.IF, aes(x=label.urb.IF, y=hazard.urb.IF, ymin=lower.urb.IF, ymax=upper.urb.IF, color=col.urb.IF)) +
    geom_pointrange() + 
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab(" ") + ylab("Hazard Ratios (95% CI)") +
    scale_color_brewer(palette="Dark2", name=" ")              +
    theme_bw()   + # use a white background
    theme(legend.position="none")
  print(fp.urb.IF)
  
  
  # theme(text = element_text(size=20),
  #        axis.text.x = element_text(angle=90, hjust=1)) 
  
  
  ############################################################################################################################
  ############################################################################################################################
    # 
    #   ### For Dariya:
    # 
    #  FOR.MAP <- SCCON %>% dplyr::select(SC, PERSONAS, MUNI, EDAD_MEDIA, ArtSurfA, Service.area.popacc,
    #                                  POPDEN.I.SD, ARTSURF.I.SD, ROADDEN.I.SD, SERAREA.I.SD, UI.N, DI.N)
    #   table(nchar(FOR.MAP$SC))
    # # known error! - add the zero to the "SC"
    # 
    #   FOR.MAP$SC[nchar(FOR.MAP$SC)==9] <- paste0("0", FOR.MAP$SC[nchar(FOR.MAP$SC)==9])
    # 
    # ### Add SMR
    # SMRsc$SC <- as.character(as.factor(SMRsc$SC))
    # FOR.MAP$SC <- as.character(as.factor(FOR.MAP$SC))
    # FOR.MAP <- FOR.MAP %>% left_join(SMRsc, by="SC")
    # 
    # ### Save as txt
    # write.table(FOR.MAP,file = "ForMap.txt",sep = ";",row.names=FALSE)

