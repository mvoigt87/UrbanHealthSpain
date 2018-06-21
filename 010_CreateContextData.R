##### --------------------------------------------------------------------------------------------------- #####
##### A. Creating a data set which combines geographical data with aggregated macrolevel information 
#####    for Census tracts in Andalusia
##### --------------------------------------------------------------------------------------------------- #####
##### B. Generating an urbanicity measure
##### --------------------------------------------------------------------------------------------------- #####

## 0.1 - packages in use
library(corrplot)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library("gridExtra")
library(psych)
library(fitdistrplus)

#### 1. Load original data files and combine them by census tract ####

# ------------------------------------ #
# 1. Census Context Information (Rosa)
# ------------------------------------ #

sc2001 <- read.table("data/SC2001_02.txt", header=T, sep = "\t")

# Change the census section - ID (key variable to something easier)
colnames(sc2001)[4] <- "SC"

# ----------------------------- #
# 2. Dariyas landcover data
# ----------------------------- #

sclandc <- read.csv2("data/GeographicalContext2001_recalc2.csv", header = T)

# Change the census section ID (key variable to something easier)
colnames(sclandc)[1] <- "SC"

### This file contains the important variable Artificial surface which will later
### be part of the explanatory set of urbanicity variables
### If the share of artificial surface is 0 this has to be changed to the minimum value

# and the variable name is also not perfect
cbind(names(sclandc)) # it is at the 10th position
colnames(sclandc)[10] <- "ArtSurfA"

 ## Total area needs to be specified to total.area.m2
colnames(sclandc)[12] <- "total.area.m2"

# ----------------------------- #
# 3. Dariyas Service Area data 
# ----------------------------- #

#    = share of total surface in a census tract from where one can reach a hospital within 30 min (tomtom)
scservice <- read.csv2("data/Service Areas.csv", header=T)
colnames(scservice)[1] <- "SC"
 
 ## change some of the long variable names
 colnames(scservice)[3] <- "Service.area.popacc"
 colnames(scservice)[6] <- "Portion.popacc"
 ## Total area to "total.area.km2" -  specifing that the information is in km^2
 colnames(scservice)[4] <- "total.area.km2"

# ---- only 4205 cases with shares of service area (need to be set to zero for the rest of the SCs) - after joining!


#### 2. Combine data sets and generate a data set called SCCON (Section Censal Context information) ####

SCCON <- sc2001 %>% left_join(sclandc,by="SC") %>% left_join(scservice, by="SC")


#### 3. Properties + Data Management for future analysis ####
 
 head(SCCON)
 str(SCCON)    # classes (most integer and numeric)
 
 ## set data for land cover and service area to zero where necessary
 
 # artificial surface - missing values are imputed with the minimum value
   # - to prevent infinite values = or is that too much of an assumption
 
 length(SCCON$ArtSurfA[SCCON$ArtSurfA==0])         ### 402 cases with zeros
 SCCON$ArtSurfA <- ifelse(SCCON$ArtSurfA==0,min(SCCON$ArtSurfA[SCCON$ArtSurfA>0]),SCCON$ArtSurfA)

 # imputing the the missing land coverage values
  # - values are zero but in the read-in process transformed to NAs
 
 SCCON$Service.area.overall <- ifelse(is.na(SCCON$Service.area.overall),0,SCCON$Service.area.overall)
 SCCON$Service.area.popacc <- ifelse(is.na(SCCON$Service.area.popacc),0,SCCON$Service.area.popacc)
 SCCON$Portion.overall <- ifelse(is.na(SCCON$Portion.overall),0,SCCON$Portion.overall)
 SCCON$Portion.popacc <- ifelse(is.na(SCCON$Portion.popacc),0,SCCON$Portion.popacc)
 ## total area in km^2 is missing as well for the cases without service area (conversion 1km^2 = 1000000m^2)
 SCCON$total.area.km2 <- ifelse(is.na(SCCON$total.area.km2),SCCON$total.area.m2/1000000,SCCON$total.area.km2)


#### 4. Save new cobined context data set for later use ####

# saveRDS(SCCON,file="010_SCCONTEXT.RData")

 # rm(sc2001,sclandc,scservice)

 
  
##### -------------------------------------------------------------------------------------------------- #####
##### B.) Preparations of a urbanicity measures
##### -------------------------------------------------------------------------------------------------- ##### 
 
#### 1. Create extra context variables - Population Density + Road Density / km^2
 
 SCCON <- SCCON %>% 
          # Population density
          mutate(pop.den = PERSONAS/total.area.km2) %>%
          # Population density by artificial surface !!!
          mutate(artf.area.km2 = (ArtSurfA*total.area.km2)/100) %>% 
          mutate(pop.den.art = PERSONAS/artf.area.km2) %>%
          # Road density
          mutate(road.den = Road.density/total.area.km2)
 
 
 
 
 #### 2. Check Correlations and identify clusters (latent concept = urban-hood)
 ####    Source: http://rpubs.com/melike/corrplot
 
 cor.df <- cor(SCCON)
 
 ## Identify correlations
 corrplot(cor.df, order = "hclust", tl.col='black', tl.cex=.75)  
 
 ### even if a little bit difficult to see - the "natural" variable related to urbanicty is correlated to a set
 ### of variables, including: contamination, delinquency, artificial surface area, road density, single households
 ### These variables quite probable describe an urban environment. However, they also indicate a particular degree
 ### of deprivation. Thus, for an analysis of mortality these variables might have a strong mediating effect.
 ### The question remains how we can measure a latent concept, when the contributing parts might have a strong impact
 ### on the mortality themselves. Idea for now: Use geographic measures to measure urbanicity and control for more
 ### socio-economic indicators in a multi-level setting.
 
 ### Filtering variables to avoid double measurement (i.e. Occupation and Unemployment Rate)
 ### and at the same time increase visualization
 
 SCCON.cor <- SCCON %>% dplyr::select(SC,PERSONAS,PERS_VIV,PCT_OCUPADOS,PCT_CASADOS,PCT_TENENPROP, PCT_2VIV, PCT_2VEHIC, 
                               PCT_EDIFBUEN, PCT_TNUC1, MED_HIJOS, PCT_JOVENES, EDAD_MEDIA, PCT_AGRIC, IP_DELINC,
                               IP_ASEO, IP_RUIDOS, IP_CONTAM, IP_HOGMONOP, NEDIFIC, Bus.stops, Health.center,
                               Universities, Supermarkets, ArtSurfA, road.den, Portion.popacc, pop.den, pop.den.art)
 
 cor.sccon <- cor(SCCON.cor)
 corrplot(cor.sccon, order = "hclust", tl.cex = .75)
 corrplot(cor.sccon, type="upper", order = "FPC", tl.col = 'black', tl.cex = .75)
 
 # rm(SCCON.cor)
 
 ### Potential urbanicity index contributers (objective variables with no direct connection to mortality)
 
 # A. Population density
  hist(SCCON$pop.den, breaks = 30)
 summary(SCCON$pop.den)
 ### The histogram and the summary statistics are indicating a skewed distribution and a high number of densily
 ### populated aeas (as expected). Need to test for the extreme cases
 
 # Extremos <- SCCON %>% filter(SCCON$pop.den>=50000) %>% select(PROV, MUNI, PERSONAS, PCT_AGRIC, pop.den, total.area.km2)
 # They tend to be extremely small urban areas in Malaga, Granada and Seville (proof!)
 
 # B. Artificial surface area
  hist(SCCON$ArtSurfA, breaks = 30)
 summary(SCCON$ArtSurfA)          # median 87.52
 # concentration at around 0 and 100 (large rural secciones have mainly natural surface, whereas small urban 
 # census tract have a very high share of artificial surface = speaks for a binary classification (?))
 
 hist(SCCON$Portion.popacc, breaks = 30)
 summary(SCCON$Portion.popacc)
 
 # C. Road density
 summary(SCCON$road.den)
 #      Min.  1st Qu.   Median     Mean  3rd Qu.      Max. 
 #      0.00    17.81   188.00   444.40   547.50  25590.00 
 
 # D. Number of buildings - is not necessarily related to population density
 summary(SCCON$NEDIFIC)
 hist(SCCON$NEDIFIC, breaks = 30)
 # similar distribution like the population density
 
 # But correlation test shows a significant and negative correlation
 cor.test(SCCON$NEDIFIC, SCCON$pop.den, method=c("pearson"))
 
 
 #### 3. Select correlated objective urban space variables
 #### -  population density, artificial surface, road density, prop service area
 #### Generating an indicator for urbanicity
      ### steps: 
      ### a) make variables compareable - percentage share (differentiate in classes - rural, predominantly rural, peri-urban, urban)
      ### b) standardize variables
      ### c) run maximum likelihood factor analysis to obtain the weights for the indicator
      ### d) combine the variables with weights in an indicator
      ### e) test the index

  ### a) pipe line for creating the index parts and the standardized factor
SCCON <- SCCON %>% 
          mutate(POPDEN.I = ntile(pop.den,4)) %>% 
          mutate(ARTSURF.I = ntile(ArtSurfA,4)) %>% 
          mutate(ROADDEN.I = ntile(road.den,4)) %>% 
          mutate(SERAREA.I = ntile(Portion.popacc,4)) %>% 
  ### b) in a second step we standardize the indicators
          mutate(POPDEN.I.SD = POPDEN.I - mean(POPDEN.I) / sd(POPDEN.I)) %>% 
          mutate(ARTSURF.I.SD = ARTSURF.I - mean(ARTSURF.I) / sd(ARTSURF.I)) %>% 
          mutate(ROADDEN.I.SD = ROADDEN.I - mean(ROADDEN.I) / sd(ROADDEN.I)) %>%
          mutate(SERAREA.I.SD = SERAREA.I - mean(SERAREA.I) / sd(SERAREA.I))
  
  ### c) Factoranalysis
  ## find latent factors within the selected indicators

  ## soruce: http://www.statmethods.net/advstats/factor.html

  SCCON.fac <- SCCON %>% dplyr::select(POPDEN.I.SD, ARTSURF.I.SD, ROADDEN.I.SD, SERAREA.I.SD)
  
  ## 2.Identify correlations
  CS <- cor(SCCON.fac)
  
  ### Crohnbachs alpha as reliability coefficient (=91%)
  alpha(CS)
  
  
  ## 3. apply explanatory factor analysis
  fit <- factanal(SCCON.fac, 1 , rotation="varimax")
  
  print(fit, digits=2, cutoff=.3, sort=TRUE)
  
  ## Factor explains 71% of the overall variance of the 4 variables
  
  ##               POPDEN.I.SD  ARTSURF.I.SD  ROADDEN.I.SD  SERAREA.I.SD 
  ## Uniquenesses:        0.05          0.33          0.19          0.58 
  ## Loadings:            0.97          0.82          0.90          0.65
  
  
  ### d) Create Urban - Indicator
  
  SCCON <- SCCON %>% mutate(UI = (POPDEN.I*0.97+ARTSURF.I*0.82+ROADDEN.I*0.90+SERAREA.I*0.65)/4)
  
  # and a categorical urbanicity variable - for easier interpretation - by quartiles (for now)
  
  SCCON$UI.cat <- 0
  SCCON$UI.cat[SCCON$UI<=1.24] <- 1
  SCCON$UI.cat[SCCON$UI>1.24 & SCCON$UI<=2.12] <- 2
  SCCON$UI.cat[SCCON$UI>2.12 & SCCON$UI<=2.872] <- 3
  SCCON$UI.cat[SCCON$UI>2.872] <- 4
  SCCON$UI.cat <- as.factor(SCCON$UI.cat)
  
  SCCON$UI.cat <- revalue(SCCON$UI.cat, c("1"="Rural","2"="Predom. Rural", "3"="Peri-Urban", "4"="Urban"))
  
  
  
  ### e) test the urban indicator
  summary(SCCON$UI)
  
  #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #   0.835   1.240   2.120   2.087   2.872   3.340 

  # Normalize the values
  
  # Standard Deviation & Mean
  SD <- sd(SCCON$UI)  # 0.8314849
  M <- mean(SCCON$UI) # 2.0872672
  
  SCCON <- SCCON %>% mutate(UI.N = (UI-M)/SD)
  
  
  hist(SCCON$UI, breaks = 30)
  hist(SCCON$UI.N, breaks = 30)
  
  # cumulative distribution
  P = ecdf(SCCON$UI.N) 
  plot(P)
  
  
  
     ### Some transformations - playing around
  
      SCCON <- SCCON %>% mutate(UI.p = UI / max(UI)) %>% 
      # logit transformation
      mutate(UI.logit = log(UI.p)) %>% 
      mutate(UI.exp = exp(UI))
  
      hist(SCCON$UI.logit)
      hist(SCCON$UI.exp)
  
  ## See how the data is distributed! (source: https://goo.gl/zi7lwv )
  x <- SCCON$UI.N
 # x <- SCCON$UI.log
 # x <- SCCON$UI.logit
  descdist(x, discrete = FALSE) ### as observed earlier, the Index is uniformily distributed
  fit.norm <- fitdist(x, "unif")      ## has a the best fit
  plot(fit.norm)
  

  
  
  
  ##### --------------------------------------------------------------------------------------------------- #####
  ##### --------------------------------------------------------------------------------------------------- #####
  ##### --------------------------------------------------------------------------------------------------- #####
  ##### --------------------------------------------------------------------------------------------------- #####
  
  ### Deprivation Index
  ### steps: 
  ### a) select variables (scaled indicators for Delinquency, Noise, Contamination, Cleaness, Unemployment)
  ### b) use standardized variables
  ### c) run maximum likelihood factor analysis to obtain the weights for the indicator
  ### d) combine the variables with weights in an indicator
  ### e) test the index
  
  SCCON.cor.depr <- SCCON %>% dplyr::select(SC,PERS_VIV,PCT_CASADOS, PCT_TENENPROP, PCT_2VIV, PCT_2VEHIC, 
                                       PCT_EDIFMAL, PCT_TNUC1, MED_HIJOS, EDAD_MEDIA, PCT_AGRIC, IP_DELINC,
                                       IP_ASEO, IP_RUIDOS, IP_CONTAM ,IP_LIMPIEZA ,IP_HOGMONOP, IP_EDBAJO_TOTAL,
                                       IP_TRABMAN, NEDIFIC, ArtSurfA, pop.den)
  
  cor.sccon <- cor(SCCON.cor.depr)
  corrplot(cor.sccon, type="upper", order = "FPC", tl.col = 'black', tl.cex = .75)
  
  ### c) Factoranalysis
  
  ## find latent factors within the selected indicators
  
  ## soruce: http://www.statmethods.net/advstats/factor.html
  
  SCCON.fac.2 <- SCCON %>% dplyr::select(PCT_TENENPROP,IP_RUIDOS, IP_CONTAM, IP_LIMPIEZA, IP_DELINC,IP_HOGMONOP) %>% 
    # Tienen prop change for direction
    mutate(PCT_NOPROP = 100-PCT_TENENPROP) %>% dplyr::select(-PCT_TENENPROP) %>% dplyr::select(-PCT_NOPROP)
    
  
  ## 2.Identify correlations
  CS.2 <- cor(SCCON.fac.2)
  ### Crohnbachs alpha as reliability coefficient (=91%)
  alpha(CS.2, check.keys = TRUE)
  
  
  ## 3. apply explanatory factor analysis
  fit <- factanal(SCCON.fac.2, 1 , rotation="varimax")
  
  print(fit, digits=2, cutoff=.3, sort=TRUE)
  
  ### d) Create Deprivation Indicator
  
  SCCON <- SCCON %>% mutate(DI = (IP_RUIDOS*0.85 + IP_CONTAM*0.73 + IP_LIMPIEZA*0.63 + IP_DELINC*0.77 + IP_HOGMONOP*0.58)/5)
  
  ### e) test the urban indicator
  summary(SCCON$DI)
  
  #    Min. 1st Qu.  Median     Mean  3rd Qu.     Max. 
  #  0.406  11.030   17.650   18.090   24.430   59.980  
  
  # Normalize the values
  
  # Standard Deviation & Mean
  SD <- sd(SCCON$DI)  # 9.6884
  M <- mean(SCCON$DI) # 18.0926
  
  SCCON <- SCCON %>% mutate(DI.N = (DI-M)/SD)

  hist(SCCON$DI.N)
  ## See how the data is distributed! (source: https://goo.gl/zi7lwv )
  x <- SCCON$DI.N
  descdist(x, discrete = FALSE)       ## as observed earlier, the Deprivation Index is normaly distributed
  fit.norm <- fitdist(x, "norm")      ## has a the best fit
  plot(fit.norm)
  par(mfrow=c(1,1))
  
###############################################################
###############################################################
##### save data set with UI indicator and transformed variables
###############################################################  

save(SCCON,file='data/015_ContextAndUI.Rdata')
  