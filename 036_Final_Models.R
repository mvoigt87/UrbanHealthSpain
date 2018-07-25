### --------------------------------------------------------------------------------------------------- ###
### This file contains the code for the final models, used in the paper and preparation code for tables ###

### To declutter the many lines of code and allow changes or if necessary to go back to earlier attempts,
### this is the code used for the "send-able" version.

### load data and packages

# 0.1 packages 
library(tidyverse)
library(data.table)
library(survival)
library(stargazer)
library(memisc)
# to extract the data 
library(eha)
library(broom)
library(readxl)
# mapping and spatial tools
library(sp)
library(rgdal)
library(RColorBrewer)
library("gridExtra")
library("lattice")
# main package for mixed effects models
library(coxme)  

### --------------------------------------------------------------------------------------------------- ###
## 0.2 data

load("data/025_INDMOR-CT.RData")

## 0.3 transform data - lower and upper age bound

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
# Change %-employed to %-unemployed
INMO.SC.2 <- INMO.SC.2 %>% mutate(PCT_UNEMPL = 100 - PCT_OCUPADOS)

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
      # #
      #   Mod.1.rs <- coxme(Surv(time = age.entry,
      #                          time2 = age.exit,
      #                          event = event) ~ UI.N + (1+UI.N|SC), data=INMO.SC.2)
      #   print(Mod.1.rs)
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
    
    anova(Cox.1,Mod.1)
    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###    
    # Using the cluster command
    # Cox.1.b <- coxph(Surv(time = age.entry,
    #                       time2 = age.exit,
    #                       event = event) ~ UI.N + cluster(SC), data=INMO.SC.2)
    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###    
    ### Different frailty distribution (Gamma-frailty model)
    
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
                        event = event) ~ UI.N + sexo + dep + ecivil +  fnac + estudios4 + tenen + 
                                         vehic + (1|SC), data = INMO.SC.2)
    print(Mod.2)
    
    # Contamination seems to increase mortality risks slightly
    

## 3.0 Model 2 + Social/Population-based effects
    
    Mod.3 <- coxme(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ UI.N + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic + IP_LIMPIEZA +
                                         IP_CONTAM + IP_RUIDOS + (1|SC), data = INMO.SC.2)
    print(Mod.3)
    
    # ! The urbanicity indicator shows significant impact, too !
        

## 4.0 Full model including individual level effects (normally distributed random effect)
    
    Mod.4 <- coxme(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ UI.N + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic + 
                                         IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + PCT_UNEMPL +
                                         PCT_SOLTEROS + (1|SC),
                        data = INMO.SC.2)
    
    print(Mod.4)

    # ----------------
    # Check frailties
    # ----------------
    
    summary(Mod.1$frail$SC)
    summary(Mod.2$frail$SC)
    summary(Mod.3$frail$SC)
    summary(Mod.4$frail$SC)

    
          # Extract regional frailties and save the effects (exp)
          library(stringr)
          
          frail.1 <- data.frame(frail=exp(unlist(Mod.1$frail)), 
                          region=c(as.numeric(str_extract(names(unlist(Mod.1$frail)), "[0-9]+"))))
          frail.2 <- data.frame(frail=exp(unlist(Mod.2$frail)), 
                          region=c(as.numeric(str_extract(names(unlist(Mod.2$frail)), "[0-9]+"))))
          frail.3 <- data.frame(frail=exp(unlist(Mod.3$frail)), 
                          region=c(as.numeric(str_extract(names(unlist(Mod.3$frail)), "[0-9]+"))))
          frail.4 <- data.frame(frail=exp(unlist(Mod.4$frail)), 
                                region=c(as.numeric(str_extract(names(unlist(Mod.4$frail)), "[0-9]+"))))
          
          
          frail.1 <- frail.1 %>% mutate(Model = "Model 1")
          frail.2 <- frail.2 %>% mutate(Model = "Model 2")
          frail.3 <- frail.3 %>% mutate(Model = "Model 3")
          frail.4 <- frail.4 %>% mutate(Model = "Model 4")
          
          frail <- union(frail.1,frail.2) %>% union(frail.3) %>% union(frail.4)
            
            
          ### Possible to map the frailties
          
          # write.table(frail, file = "frail.txt", sep = ";", col.names = T, row.names = F)
    
          
    # ----------------
    # Variances fixed
    # ----------------
      
    VarCorr(Mod.4)$SC 
    
    # -----------------------------------------
    # random effects coefficients and variance
    # -----------------------------------------
      
    stem(exp(ranef(Mod.4)[[1]]))
    
    # census tract effects range from 0.84 to 1.14 times the average risk
    
    var(ranef(Mod.4)$SC)     
    # 0.001321238
    
    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###     
    
    # for comparing improvement in model fit (model without random effects)
    Cox.4 <- coxph(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ UI.N + IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + IP_DELINC + PCT_UNEMPL +
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
    
        # ## Get profile likelihood for variance values to examine
        s <- seq(0.01, 0.4, length.out = 200)^2
        res <- sapply(s, function(i) {
          mod <- coxme(Surv(time = age.entry,
                                 time2 = age.exit,
                                 event = event) ~ UI.N + IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + PCT_UNEMPL +
                              PCT_SOLTEROS + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic + (1|SC),
                            data = INMO.SC.2, vfixed = i)
          2 * diff(mod$loglik)[1]
          })

        ## profile likelihood, horizontal line is 95% CI obviously lower bound
        ## includes 0, upper bound looks a little under .8

        plot(sqrt(s), res, type = "l", xlab = expression(SD[i]), ylab = "2 * LL")
        abline(h = 2 * diff(Cox.4$loglik)[1] - qchisq(0.95, 1), lty = 2)

    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###    
        
   
    # Using the cluster command
    Cox.4.b <- coxph(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ UI.N + IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + IP_DELINC + PCT_UNEMPL +
                       PCT_SOLTEROS + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic + cluster(SC), data=INMO.SC.2)
    
    summary(Cox.4.b)
    
    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###    
    ### Different frailty distribution (gamma distributed frailty distribution)
    
    # see Austin 2017:  https://onlinelibrary.wiley.com/doi/epdf/10.1111/insr.12214
    
    Cox.4.c <- coxph(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ UI.N + IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + IP_DELINC + PCT_UNEMPL +
                                           PCT_SOLTEROS + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic +
                        frailty(SC,distribution="gamma"),
                        data=INMO.SC.2)
    summary(Cox.4.c)

    
    
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ### 
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ### 
    ### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ### 
    
    ### For extracting the census tract specific hazard function
    
    Cox.4.c <- coxph(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ UI.N + IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + IP_DELINC + PCT_UNEMPL +
                       PCT_SOLTEROS + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic +
                       frailty(SC,distribution="gaussian"),
                     data=INMO.SC.2)
    summary(Cox.4.c)
    
    
        
        ## Extract hazards for a standard person
        str(survfit(Cox.4.c))
        cumhazard <- survfit(Cox.4.c)$cumhaz
        time.haz <- survfit(Cox.4.c)$time
        surv.haz <- survfit(Cox.4.c)$surv
        std.haz <- survfit(Cox.4.c)$std.err
        low.haz <- survfit(Cox.4.c)$lower
        high.haz <- survfit(Cox.4.c)$upper
        
        data.plot <- as.data.frame(cbind(cumhazard,time.haz,surv.haz,std.haz,low.haz,high.haz))
        
        ## play
        
        surv.sc <- data.plot %>% ggplot() +
          geom_step(aes(x=time.haz,y=surv.haz)) +
          theme_bw()
      
      ### Cumulated hazard  
        # Now get baseline hazard curve estimated from the cox model
        baseline <- basehaz(Cox.4.c)
        
        # Draw baseline hazard (that's male)
        plot(baseline$time, baseline$hazard, type='l',main="Hazard rates") 

        
        
        
        # estimated survival function from the cox - shared frailty model
        
        plot(survfit(Cox.4.c), xlab="Age", ylab="Surviving")
        
        # extract the mean
        x.minority <- colMeans(Cox.4.c$x)  
        x.minority["majority_government"] <- 0
        x.majority <- x.minority
        x.majority["majority_government"] <- 1
        hyp.data <- as.data.frame(rbind(x.minority,x.majority))
        
        plot(survfit(coxph.survival, newdata=hyp.data), xlab="Months", ylab="Governments Surviving", conf.int=T, lty=c(1,2))
        legend("topright", legend=c("minority govt","majority govt"), lty=c(1,2))
        
        # interpreting in terms of expected time to failure is hard because we do not estimate a baseline
        # instead look at changes in the hazard rate
        
        hazard.pct.change <- ((exp(x.minority %*% t(betas)) - exp(x.majority %*% t(betas)))/exp(x.minority %*% t(betas)))*100
        hazard.mean <- mean(hazard.pct.change)
        hazard.sd <- apply(hazard.pct.change,1,sd)
        cbind(hazard.mean, hazard.sd)  # minority gov't 47% more likely to fail
        
        
        
        
        
        
        
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
    