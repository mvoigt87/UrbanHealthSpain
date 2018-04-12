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

ANDALUS.SC <- readOGR(dsn="C:/Users/y4956294S/Documents/LONGPOP/Subproject 3 - Urban Environment and Health/RCode/UrbanHealthSpain/data/mapas.capas/SecCensales2001",
                      layer = "andalusia.census.tract.2001")

# ANDALUS.SC <- readOGR(dsn="C:/Users/y4956294S/Documents/Workshops and Conferences/Cambridge/Data/actual_Urb_Index/spatDat",
#                      layer = "da08_seccion_censal")

  # # Population count by province
  # pop.prov <- by(ANDALUS.SC$POBLACION, ANDALUS.SC$PROVINCIA, sum)
  # 
  # # Extreme Values Provincial Level
  # pop.prov[which.max(pop.prov)]  # Sevilla - 1.939.775
  # pop.prov[which.min(pop.prov)]  # Huelva  - 519.596 
  # 
  # # Extreme Values Municipality Level
  # pop.mun <- by(ANDALUS.SC$POBLACION, ANDALUS.SC$MUNICIPIO, sum)
  # pop.mun[which.max(pop.mun)]  # Sevilla -  690.566

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

        SPAT.SC <- SCCON %>% dplyr::select(SC, PERSONAS, MUNI, PCT_OCUPADOS, EDAD_MEDIA, PCT_AGRIC, ArtSurfA, Service.area.popacc, pop.den,
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
      
sum(d$codigo01 %in% unique(SPAT.SC$SC))
# all 5381

# join what you get
d <- d %>% mutate(SC = codigo01) %>% 
              left_join(SPAT.SC, by="SC")

### Now the data goes back to the spatial object
ANDALUS.SC@data <- d

# Map by SMR
spplot(ANDALUS.SC,"SMRe") ### Be careful with the missings
# Map by UI
spplot(ANDALUS.SC,"UI.N") ### Be careful with the missings
# Map by DI
spplot(ANDALUS.SC,"DI.N") ### Be careful with the missings ----------------------- !!!

      # ###### %%%% For now listwise deletion of the census tracts we don´t have information for
      # 
      # ANDALUS.SC <- subset(ANDALUS.SC, !is.na(UI))

require(maptools)
require(spdep)

writePolyShape(ANDALUS.SC, "data/ANDALUS_SC2001")
### ---------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------- ###

  # Check degree of urbanization by zooming into Seville

  # Municipality - Sevilla
  # postal codes 41001-41080   # 41000-41020, 41040, 41071, 41080, 41086, 41087, 41089, 41701, 41702, 
                               # 41703, 41710, 41719, 41727- 41729
  table(ANDALUS.SC$codmun)
  SEV.mun.all <- c("41001","41002","41003","41004","41005", "41006", "41007", "41008", "41009", "41010", "41011",
               "41012", "41013", "41014", "41015", "41016", "41017", "41018", "41019", "41020", "41021",
               "41022", "41023", "41024", "41025", "41026", "41027", "41028", "41029", "41030", "41031",
               "41032", "41033", "41034", "41035", "41036", "41037", "41038", "41039", "41040", "41041", 
               "41042", "41043", "41044", "41045", "41046", "41047", "41048", "41049", "41050", "41051",
               "41052", "41053", "41054", "41055", "41056", "41057", "41058", "41059", "41060", "41061",
               "41062", "41063", "41064", "41065", "41066", "41067", "41068", "41069", "41070", "41071",
               "41072", "41073", "41074", "41075", "41076", "41077", "41078", "41079", "41080", "41081",
               "41082", "41083", "41084", "41085", "41086", "41087", "41088", "41089", "41090", "41091",
               "41092", "41093", "41094", "41095", "41096", "41097", "41098", "41099", "41100", "41101",
               "41102", "41901", "41902", "41903")
  SEV.city <- c("41091")
  
  # Municipality
  
  SEV <- subset(ANDALUS.SC, codmun %in% SEV.city)
  
  ### Map Plot
  # 37.3891° N, 5.9845° W
  

  myCols <- adjustcolor(colorRampPalette(brewer.pal(n=9, 'Blues'))(100), .85)
  
  
  SEV.UI.plot <- spplot(SEV, "UI.N",col.regions=myCols, scales=list(draw = TRUE), colorkey=TRUE)
  


  SEV.DI.plot <- spplot(SEV,"DI.N",col.regions=myCols, scales=list(draw = TRUE), colorkey=TRUE)
  
  # Provincia
  SEV.P <- subset(ANDALUS.SC, codmun %in% SEV.mun)
  P.SEV.UI.plot <- spplot(SEV.P, "UI.N")
  P.SEV.DI.plot <- spplot(SEV.P,"DI.N")

  ### Put both in one grid 
  grid.arrange(SEV.UI.plot, SEV.DI.plot, ncol=2, nrow=1)
  
  
  ### ---------------------------------------------------------------------------------------- ###
  ### ---------------------------------------------------------------------------------------- ###
  
  
  ######################################################### UI - seems to measure urbanicity quite well
  
  
### Test for Spatial Autocorrelation

## a) Adjacency Matrix from the shape file

and.nb <- poly2nb(ANDALUS.SC)

# row-standardised weight matrix - increased the weights of links from observation with few neighbours
  # --- in this case the row sums of the weights are unity
and.lw <- nb2listw(and.nb, style = "W")    

# add ", zero.policy=TRUE" in case of missing neighbours

#################
### Moran's I ###
#################

## Spatial autocorrelation test by single variable


  # Population
  moran.test(ANDALUS.SC$PERSONAS, and.lw) ### highly spatially correlated: Morans I statistic = 0.30544
  # SMR                                           
  ANDALUS.SC$SMRe <- as.numeric(ANDALUS.SC$SMRe)
  moran.test(ANDALUS.SC$SMRe, and.lw)                       ### highly spatially correlated: Moran I statistic = 0.380
  # Urbanicity indicator
  moran.test(ANDALUS.SC$UI.N, and.lw)                       ### highest spatial correllation: Moran I statistic = 0.784
  # Deprivation indicator
  moran.test(ANDALUS.SC$DI.N, and.lw)                       ### highly spatially correlated: Moran I statistic = 0.685


### --------------------------------------------------- ###    
###  Lee’s statistic for comparison of two variables:
### --------------------------------------------------- ###
  
  n <- nrow(ANDALUS.SC)
  
  # Extract L-values for the two indicators
  lee(ANDALUS.SC$UI, ANDALUS.SC$SMRe, and.lw, n)$L        # 0.05001896
  lee(ANDALUS.SC$DI, ANDALUS.SC$SMRe, and.lw, n)$L        # 0.1725797
  
  # Urban - Mortality Spatial
  lee.test(ANDALUS.SC$UI.N, ANDALUS.SC$SMRe, and.lw, alt = "two.sided")
  
    # Lee's L statistic       p-value = 9.27e-10  
    # 5.001896e-02
  
  # Deprivation - Mortality Spatial
  lee.test(ANDALUS.SC$DI.N, ANDALUS.SC$SMRe, and.lw, alt = "two.sided")
  
    # Lee's L statistic       p-value < 2.2e-16
    # 1.725797e-01 
  
  ## Monte Carlo test
  lee.mc(ANDALUS.SC$UI.N, ANDALUS.SC$SMRe, and.lw, alt = "less", nsim = 999)
      #	Monte-Carlo simulation of Lee's L
      # data:  ANDALUS.SC$UI.N ,  ANDALUS.SC$SMRe 
      # weights: and.lw  
      # number of simulations + 1: 101 
      # 
      # statistic = 0.050019, observed rank = 101, p-value = 0.9901
      # alternative hypothesis: less
  
  lee.mc(ANDALUS.SC$DI.N, ANDALUS.SC$SMRe, and.lw, alt = "less", nsim = 999)
  
      # Monte-Carlo simulation of Lee's L
      # 
      # data:  ANDALUS.SC$DI.N ,  ANDALUS.SC$SMRe 
      # weights: and.lw  
      # number of simulations + 1: 100 
      # 
      # statistic = 0.17258, observed rank = 100, p-value = 0.99
      # alternative hypothesis: less
  
  # LM
  summary(lm (SMRe ~ UI.N + DI.N, data =ANDALUS.SC))
  


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
### %%%%%%%%%%%%% Models with Indicator effects only -  DI


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

  
######################
### Testing models ###
######################

  
### Model 1 - Urban Indicator
  Mod.1 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ UI.N, data = INMO.SC)
  
  Mod.1.ran <- coxme(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ UI.N + (1|SC), data = INMO.SC)
  
  ## Compare Model Fit
  AIC(Mod.1.ran)-AIC(Mod.1)
  
### Model 2 - Environment Effects
  
  Mod.2 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ DI.N,
                      data = INMO.SC)
  summary(Mod.2)
  
  AIC(Mod.2)-AIC(Mod.1) # Model 2 fits the data much better than Model 1 (difference 1370.697)
  
  Mod.2.ran <- coxme(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ DI.N + (1|SC),
                 data = INMO.SC)
  
  AIC(Mod.2.ran)-AIC(Mod.2) # Model with random effects is more likely to minimize the information loss
  
  
### Model 3 - Urbanicity effects (UI.N) + Environment Effects at the same time

  Mod.3 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ UI.N + DI.N,
                 data = INMO.SC)
  summary(Mod.3)  
  
  AIC(Mod.3)-AIC(Mod.2) # Model 3 fits the data much better than Model 2 (difference 879.484)
  
  
  Mod.3.ran <- coxme(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ UI.N + DI.N + (1|SC),
                     data = INMO.SC)
  
  AIC(Mod.3.ran)-AIC(Mod.3) # Model with random effects is more likely to minimize the information loss (difference 888.4247)
  
### Model 4 - Model 3 + Individual Level Effects
  
  Mod.4 <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ UI.N + DI.N + sexo + dep + ecivil + fnac + estudios4 + tenen + vehic,
                 data = INMO.SC)
  summary(Mod.4)  
  
  AIC(Mod.4)-AIC(Mod.3) # Model 4 fits the data much better than Model 3 (difference 75.80463)
  
  
  Mod.4.ran <- coxme(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ UI.N + DI.N + sexo + dep + ecivil +  fnac + estudios4 + tenen + vehic + (1|SC),
                     data = INMO.SC)
  
  AIC(Mod.4.ran)-AIC(Mod.4) # Model with random effects is more likely to minimize the information loss (difference 865.8751)
  
  
  AIC(Mod.4.ran) - AIC(Mod.3.ran) # just 53 difference
  
  #### Further significance tests
  
  library(lme4)
  m2 <- lmer(felev ~ DI.N + (1|SC)+(0+DI.N|SC),data = INMO.SC, REML=FALSE)
  m1 <- update(m2,.~DI.N + (1|SC))
  m0 <- lm(felev ~ DI.N,data = INMO.SC)
  anova(m2,m1,m0) ## two sequential tests
  
  ###############################################################################################################
  
  
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
  
  ### Graphs for the presentation:
  
  library(broom)
  library(forestplot)
  library(survminer)
  
  ## Test
  ggforest(fit.3)
  
  
  
  ##### NEW CODIGO
  
  ### forest plot for the indicator variables (both indicators in one model)
  
  label <- c("Urban Ind.(Cox)", "Urban Ind.(Mix)", "Urban Ind. (Devel.)" , "Urban Ind. (full)",
             "Develop. Ind.(Cox)", "Develop. Ind.(Mix)", "Develop. Ind. (Urban)", "Develop. Ind. (full)")
  hazard  <- c(1.002,1.003,0.9794,0.9975,1.027,1.029,1.042,1.045) 
  lower <- c(0.9949,0.9944,0.968925,0.9873881,1.02,1.020473,1.031397,1.03474)
  upper <- c(1.01,1.011,0.9898532,1.007701,1.035,1.037815,1.052775,1.03474)
  
  df <- data.frame(label, hazard, lower, upper)
  
  # reverses the factor level ordering for labels after coord_flip()
  df$label <- factor(df$label, levels=rev(df$label))
  
  # fade out insignificant values
  alpha_vector = rep(0.25, nrow(df))
  alpha_vector[c(3,5,6,7,8)] = 1
  
  df$alpha = alpha_vector

  fp <- ggplot(data=df, aes(x=label, y=hazard, ymin=lower, ymax=upper, alpha=alpha)) +
    geom_pointrange(show.legend=FALSE) + 
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab(" ") + ylab("Hazard Ratios (95% CI)") +
    theme_bw()  # use a white background
  print(fp)
  
  
  ### forest plot for the urban indicator
  ### -----------------------------------
  
  # label.urb <- c("Urban Ind.(Cox)", "Urban Ind.(Mix)", "Urban Ind. (Devel.)" , "Urban Ind. (full)")
  
  ### Alternative Labels
  label.urb <- c("Cox (Only Ind.)", "Mixed Model (Only Ind.)", "Mixed Model (Two Ind.)" , "Full Model")
  
  hazard.urb  <- c(1.002,1.003,0.9794,0.9975) 
  lower.urb <- c(0.9949,0.9944,0.968925,0.9873881)
  upper.urb <- c(1.01,1.011,0.9898532,1.007701)
  col.urb <- c("C","MC","MC","MC")
  df.urb <- data.frame(label.urb, hazard.urb, lower.urb, upper.urb,col.urb)
  
  # reverses the factor level ordering for labels after coord_flip()
  df.urb$label.urb <- factor(df.urb$label.urb, levels=rev(df.urb$label.urb))
  
  
  fp.urb <- ggplot(data=df.urb, aes(x=label.urb, y=hazard.urb, ymin=lower.urb, ymax=upper.urb, color=col.urb)) +
    geom_pointrange() + 
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab(" ") + ylab("Hazard Ratios (95% CI)") +
    scale_color_brewer(palette="Dark2", name=" ")              +
    theme_bw()   + # use a white background
    theme(legend.position="none")
  print(fp.urb)
  
  
 # theme(text = element_text(size=20),
 #        axis.text.x = element_text(angle=90, hjust=1)) 
  
  
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
  
  ### Additional stuff 
  
  # Indicator Components of the UI (POPDEN.I+ARTSURF.I+ROADDEN.I+SERAREA.I)
  Mod.1.I <- coxph(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ POPDEN.I.SD+ARTSURF.I.SD+ROADDEN.I.SD+SERAREA.I.SD, data = INMO.SC)
  
  Mod.1.I.ran <- coxme(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ POPDEN.I.SD+ARTSURF.I.SD+ROADDEN.I.SD+SERAREA.I.SD + (1|SC), data = INMO.SC)
  
  summary(Mod.1.I)
  print(Mod.1.I.ran)
  
  
  ### Model 2 - Components of the Environment Effects Indicator (IP_RUIDOS + IP_CONTAM + IP_LIMPIEZA + IP_DELINC + IP_HOGMONOP)
  
  Mod.2.I <- coxph(Surv(time = age.entry,
                      time2 = age.exit,
                      event = event) ~ IP_RUIDOS + IP_CONTAM + IP_LIMPIEZA + IP_DELINC + IP_HOGMONOP,
                 data = INMO.SC)
  summary(Mod.2.I)
  
  Mod.2.I.ran <- coxme(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ IP_RUIDOS + IP_CONTAM + IP_LIMPIEZA + IP_DELINC + IP_HOGMONOP + (1|SC),
                     data = INMO.SC)
  
  print(Mod.2.I.ran)
  
  AIC(Mod.2.I.ran)-AIC(Mod.2.I) # Model with random effects is more likely to minimize the information loss
  AIC(Mod.2.I)-AIC(Mod.1.I) # Model 2 fits the data much better than Model 1 (difference 1370.697)
  
  ### Model 3 - Components of the both indicators
  
  Mod.3.I <- coxph(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ POPDEN.I.SD+ARTSURF.I.SD+ROADDEN.I.SD+SERAREA.I.SD + 
                     IP_RUIDOS + IP_CONTAM + IP_LIMPIEZA + IP_DELINC + IP_HOGMONOP,
                   data = INMO.SC)
  summary(Mod.3.I)
  
  Mod.3.I.ran <- coxme(Surv(time = age.entry,
                            time2 = age.exit,
                            event = event) ~ POPDEN.I.SD+ARTSURF.I.SD+ROADDEN.I.SD+SERAREA.I.SD + 
                         IP_RUIDOS + IP_CONTAM + IP_LIMPIEZA + IP_DELINC + IP_HOGMONOP + (1|SC),
                       data = INMO.SC)
  
  print(Mod.3.I.ran)
  
  ### Model 4 - FULL MODEL
  
  Mod.4.I <- coxph(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ POPDEN.I.SD+ARTSURF.I.SD+ROADDEN.I.SD+SERAREA.I.SD + 
                     IP_RUIDOS + IP_CONTAM + IP_LIMPIEZA + IP_DELINC + IP_HOGMONOP +
                     sexo + dep + ecivil + fnac + estudios4 + tenen + vehic,
                   data = INMO.SC)
  summary(Mod.4.I)
  
  Mod.4.I.ran <- coxme(Surv(time = age.entry,
                            time2 = age.exit,
                            event = event) ~ POPDEN.I.SD+ARTSURF.I.SD+ROADDEN.I.SD+SERAREA.I.SD + 
                         IP_RUIDOS + IP_CONTAM + IP_LIMPIEZA + IP_DELINC + IP_HOGMONOP +
                         sexo + dep + ecivil + fnac + estudios4 + tenen + vehic+ (1|SC),
                       data = INMO.SC)
  
  print(Mod.4.I.ran)
  
  
  
  + sexo + dep + ecivil + fnac + estudios4 + tenen + vehic