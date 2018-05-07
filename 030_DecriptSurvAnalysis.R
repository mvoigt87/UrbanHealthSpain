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

  # sex
  INMO.SC <- within(INMO.SC, sexo <- relevel(sexo, ref = "female"))  
  # Civil status
  INMO.SC <- within(INMO.SC, ecivil <- relevel(ecivil, ref = "Married"))
  # dependency variable
  INMO.SC <- INMO.SC %>% mutate(dep = as.factor(ifelse(dependiente=="No","not dependent", "dependent")))
  INMO.SC <- within(INMO.SC, dep <- relevel(dep, ref = "not dependent"))
  # Housing ownership
  INMO.SC <- within(INMO.SC, tenen <- relevel(tenen, ref = "Owns House/Apartment"))
  
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
                                            ####################
  
  

  
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
  


  
  
  
  
  
  

### Model 2 - Urbanicity effects (UI.N) + Environment effects (cleanness, air pollution, Noise)
  
# Cox model
  Mod.2 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ UI.N + sexo + dep + ecivil + fnac + estudios4 + tenen + vehic, data = INMO.SC)
  
# spatially distributed model
  Mod.2.ran <- coxme(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ UI.N + sexo + dep + ecivil +  fnac + estudios4 + tenen + 
                                           vehic + (1|SC), data = INMO.SC)
  

  
### Model 3 - Model 2 + Socio-cultural effects (delinquency, percentage_employed, Pct_Agriculture)

# Cox model
  Mod.3 <- coxph(Surv(time = age.entry,
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
  
  Mod.4 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ UI.N  + sexo + dep + ecivil + fnac + estudios4 + tenen + vehic + 
                                       IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + PCT_UNEMPL + PCT_SOLTEROS + IP_DELINC,
                 data = INMO.SC)
  summary(Mod.4)  
  
  # 
  
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
  
  ### The model including random effects significantly improves the model fit
  
  
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
  
  ### Simple model WITH random effects (only "Urbanicity" as covariate)
  
  # Logic - intercept (effect) per Census Tract (group)
  
  fit.comp.ran <- coxme(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ POPDEN.I.SD + ARTSURF.I.SD + ROADDEN.I.SD + SERAREA.I.SD + (1|SC), data=INMO.SC)

  
  ### %%%%%%%%%%%%% Full Models with Indicator components -  UI
  ### Simple model without random effects (only "Urbanicity" as covariate)
  Mod.4.comp <- coxph(Surv(time = age.entry,
                         time2 = age.exit,
                         event = event) ~ POPDEN.I.SD + ARTSURF.I.SD + ROADDEN.I.SD + SERAREA.I.SD +
                        sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic +
                        IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + PCT_UNEMPL + 
                        PCT_SOLTEROS + IP_DELINC, data=INMO.SC)
  
  summary(Mod.4.comp)
  ### Simple model WITH random effects (only "Urbanicity" as covariate)
  
  # Logic - intercept (effect) per Census Tract (group)
  
  Mod.4.RAN.comp <- coxme(Surv(time = age.entry,
                         time2 = age.exit,
                         event = event) ~ POPDEN.I.SD + ARTSURF.I.SD + ROADDEN.I.SD + SERAREA.I.SD + 
                           + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic +
                           IP_LIMPIEZA + IP_CONTAM + IP_RUIDOS + PCT_UNEMPL + 
                           PCT_SOLTEROS + IP_DELINC +(1|SC), data=INMO.SC)