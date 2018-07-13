### --------------------------------------------------------------------------------------------------- ###
### This file contains the code for the final models, used in the paper and preparation code for tables ###

### To declutter the many lines of code and allow changes or if necessary to go back to earlier attempts,
### this is the code used for the "send-able" version.

### load data and packages

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
library(coxme)  

### --------------------------------------------------------------------------------------------------- ###
## 0.2 data

load("data/025_INDMOR-CT.RData")

## 0.3 transform data - lower and upper age bound + exclude census tracts with too little events

INMO.SC.2 <- INMO.SC %>% dplyr::filter(age.entry>=35 & age.entry<=80) 

# 351769 individuals between 35 and 80 
 
      # # Number of deaths by census tract (should be more than 1 for variability)
      # 
      # aggdata <-aggregate(INMO.SC.2$event==1, by=list(INMO.SC.2$SC),FUN=sum, na.rm=TRUE)
      # summary(aggdata$x)
      # 
      # table(aggdata$x>=2) # there are less than 2 deaths in 74 census tracts over the years!
      # 
      # colnames(aggdata) <- c("SC","agg.death")
      # 
      # INMO.SC.2 <- INMO.SC.2 %>% left_join(aggdata, by="SC") %>% 
      #   dplyr::filter(agg.death>=2)
      # 
      # # 348694 individuals under observation

      #   rm(aggdata)
### --------------------------------------------------------------------------------------------------- ###


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
### --------------------------------------------------------------------------------------------------- ###


## 1.0 Model only including urban indicator effects

  # Logic - intercept (effect) per Census Tract (group)

Mod.1 <- coxme(Surv(time = age.entry,
                    time2 = age.exit,
                    event = event) ~ UI.N + (1|SC), data=INMO.SC.2)
print(Mod.1)

fixef(Mod.1) # fixed effects
stem(exp(ranef(Mod.1)[[1]])) # list of random effects for UI by census tract

    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###     
    ### Try a random slope model
      #
        Mod.1.rs <- coxme(Surv(time = age.entry,
                               time2 = age.exit,
                               event = event) ~ UI.N + (1+UI.N|SC), data=INMO.SC.2)
        print(Mod.1.rs)
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###     

    # for comparing improvement in model fit (model without random effects)
    Cox.1 <- coxph(Surv(time = age.entry,
                       time2 = age.exit,
                        event = event) ~ UI.N, data=INMO.SC.2)
    Cox.1$loglik
    
    
    # As they are in theory nested models, a comparison of the Log-likelihood would be possible
    # without random effects: -541904.6 ; with random effects:  -541809.1 (Chisq=111.83); NULL model: -541905.4
    # LRT : 2*(-541809.1-(-541905.4)) => D is 191 with 1 df : p-value < 0.001
    
    # estimated standard deviation between census tracts is 0.14 =>
    #   interpretation: Random effects for a census tract with risk scores one standard deviation above the mean (0.14)
    #                   corresponds to a relative risk of 1.15 i.e. a 15 % higher risk in this census tract
    
    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###    
    # Using the cluster command
    Cox.1.b <- coxph(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ UI.N + cluster(SC), data=INMO.SC.2)
    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###    
    ### Different frailty distribution (Gamma)
    
      # see Austin 2017:  https://onlinelibrary.wiley.com/doi/epdf/10.1111/insr.12214
    
    Cox.1.c <- coxph(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ UI.N + frailty(SC,distribution="gamma"), data=INMO.SC.2)
    summary(Cox.1.c)
    Cox.1.c$loglik
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###    

    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###
    # Model 1 with additional random effect of urbanicity within Census tracts (difference within CS from global effect)
    # Mod.1.b <- coxme(Surv(time = age.entry,
    #                     time2 = age.exit,
    #                     event = event) ~ UI.N + (1|SC) + (0+UI.N|SC), data=INMO.SC.2)
    # print(Mod.1.b)      
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###


    
## 2.0 Model including environmental effects
    
    Mod.2 <- coxme(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ UI.N + IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + (1|SC), data = INMO.SC.2)
    print(Mod.2)
    
    # Contamination seems to increase mortality risks slightly
    

## 3.0 Model 2 + Social/Population-based effects
    
    Mod.3 <- coxme(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ UI.N + IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + PCT_OCUPADOS +
                                         PCT_SOLTEROS + (1|SC), data = INMO.SC.2)
    print(Mod.3)
    
    # social/population-based aspects all affect mortality risks - between the lowest and the highest score on the different
    # variables the differences can be substantial
      
        # example:
        min(INMO.SC.2$IP_DELINC) # 0
        max(INMO.SC.2$IP_DELINC) # 97.7
        # the difference in relative mortality risk between the two extreme values is almost 20% (97.7*0.0019559)
        
        
    # ! The urbanicity indicator shows significant impact, too !
        

## 4.0 Full model including individual level effects
    
    Mod.4 <- coxme(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ UI.N + IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + PCT_OCUPADOS +
                                         PCT_SOLTEROS + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic + (1|SC),
                        data = INMO.SC.2)
    
    print(Mod.4)
    
    # Check frailties
    summary(Mod.4$frail$SC)
    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###     
    
     # for comparing improvement in model fit (model without random effects)
    Cox.4 <- coxph(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ UI.N + IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + IP_DELINC + PCT_OCUPADOS +
                                          PCT_SOLTEROS + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic, 
                        data=INMO.SC.2)
    
    # Model comparison
    
    Cox.4$loglik
    
    anova(Mod.4, Cox.4)
    
    AIC(Cox.4)-AIC(Mod.4)
    
      # As they are in theory nested models, a comparison of the Log-likelihood would be possible
    # without random effects: -541905.4 ; with random effects:  -537517.3 (Chisq=8695.35); NULL model: -541905.4
    # LRT: 2*(-537517.3-(-537557.9)) => D is 81.2 with 18 df : p-value < 0.001
    
    # With the tests obtained in 030_DescriptSurvAnalysis.R we can confidently state that the inclusion of random
    # effects improves the model fit significantly
    
    # estimated standard deviation between census tracts is 0.103 =>
    #   interpretation: Random effects for a census tract with risk scores one standard deviation above the mean (0.103)
    #                   corresponds to a relative risk of 1.1085 i.e. an about 11 % higher risk in this census tract
    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###     
    # Using the cluster command
    Cox.4.b <- coxph(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ UI.N + IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + IP_DELINC + PCT_OCUPADOS +
                       PCT_SOLTEROS + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic + cluster(SC), data=INMO.SC.2)
    
    summary(Cox.4.b)
    
    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###    
    ### Different frailty distribution (Gamma)
    
    # see Austin 2017:  https://onlinelibrary.wiley.com/doi/epdf/10.1111/insr.12214
    
    Cox.4.c <- coxph(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ UI.N + IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + IP_DELINC + PCT_OCUPADOS +
                                           PCT_SOLTEROS + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic +
                        frailty(SC,distribution="gamma"),
                        data=INMO.SC.2)
    summary(Cox.4.c)

    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ### 
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ### 
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ### 
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ### 
    
    
## 5.0 Model results in table
    
    stargazer(Cox.1, Cox.1, Cox.1, Cox.4, title ="Cox PH Model",no.space=F, 
              ci=F, ci.level=0.95, omit.stat=c("max.rsq"),dep.var.labels=c("Hazard Ratios"),
              covariate.labels=c("Degree Urbanicity", "Perceived Cleaness", "Perceived Pollution", "Perceived Noise",
                                 "Delinquency", "% Employed", "% Single HH","Male","Physically Dependend", 
                                 "Single","Widowed","Divorced/Separated", "Birth Cohort", "No or Incomplete Educ.",
                                 "Primary/Secondary Educ.", "Does not Own House","Does not Own a Car"),
              single.row=F, apply.coef = exp)
    