#### -------------------------------------------------------------------------------------------- ####
#### A.) Data cleaning and management regarding the individual follow up
#### -------------------------------------------------------------------------------------------- ####

## The special contribution to the field will be that we do not only have the area deprivation/urbanicity but
## that we can control for the individual social postion and other variables of individuals in the areas which we are 
## looking at (Nationality, Sex, Highest Degree of education 2001, Occupation in 2001).

## The problem is that we have no time varying variables
# Option: Reduce analysis to adult mortality (= individuals 50-90) --- With some background data we could show that
#   a) people generally migrate before 35 and settle after that, 
#   b) mortality occurs at in general late in life (80-90%), 
#   c) the educational degree does not change very often

## Second data problem regards the oldest part of the population:
#  People older than 85 live to a large extend in nursing homes or other care facilities (maybe with their kids)
#  For these individuals we would not measure the impact of their long-term residence environment but probably something else

### ---------------------------------------------------------------------------------------- ###
### Plan
###
### 1. Download and clean individual data (including collapsing and changing categories)
### 2. Discriptive Statistics and correlations -  arranging the covariates
### 3. Generating the time to event variables
### 4. Connect individual level data to the respective census tract information
### ---------------------------------------------------------------------------------------- ###

## 0.1 - packages in use
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)
library(stargazer)
library(memisc)


##### 1. Download and clean data

### load data set - individual episodes (smp) and the context information from the census 
load('data/020_mp_smp_data.table.RData')

### 1.1 ------- Create census tract variable form other area codes (old code from Fran)
str(mp)
head(mp)
str(smp)

codpro <- c(4,11,14,18,21,23,29,41)
mp$CensusTract <- codpro[as.factor(mp$provincia)]*10^8 + mp$municipio*10^5 + mp$distrito*10^3  + mp$seccion
head(mp[,c('CensusTract','provincia','municipio','distrito','seccion')]  )

## Check for re-entry cases

sum(mp$id %in% unique(smp$id)) # 723234 of 737319 (1.9 % return migration cases)
setdiff(unique(smp$id),mp$id)

#### 1.2. ------ Create a new data set joining the follow up to the census information

INMO <- smp %>% inner_join(mp, by="id")

tbl_df(INMO) 

#rm(mp, smp)

## Event/Censor variables

table(INMO$tipob)
# Emigraci?n fuera de Andaluc?a     Fallecimiento     Fin de estudio 
# 43672                             69613             624034 

##### 1.3. ---- Change variable names and categories for following analysis


## Census Tract Variable name for later link with the context variables
cbind(names(INMO))
colnames(INMO)[64] <- "SC"


###### 2. Discriptive Statistics and correlations - arranging the covariates
### ------------------------------------------------------------------------------------------ ###

## --- ##
## sex ##
## --- ##

INMO$sexo <- revalue(INMO$sexo, c("Hombre"="male","Mujer"="female"))

## ---------------------------- ##
## Estado civil - civil status  ##
## ---------------------------- ##

INMO$ecivil <- revalue(INMO$ecivil, c("Soltero/a"="single", "Casado/a"="married", "Viudo/a"="widowed", 
                                                "Separado/a o divorciado/a"="divorced/sep"))

## -------------------------- ##
## Education - highest degree ##
## -------------------------- ##

INMO$estudios4 <- revalue(INMO$estudios4, c("No sabe leer o escribir"="Illiterate", "Incompletos y sin estudios"="Incomplete education", 
                                                      "Primer y segundo grado"="Primary or secondary", "Superiores, tercer grado"="Higher secondary or tertiary"))

### see the gender distribution by education - Andalusia 2001

ed.sex.tbl <- table(INMO$estudios4,INMO$sexo)
sex.crtbl <- round(100*prop.table(ed.sex.tbl,2), digits=2)
sex.crtbl
 # data shows more illitate females (9.48 to 11.64%); shares of highly educated and unfinished education (! 43% !)
 # are the same for both sexes; men have relatively more often a primary or secondary degree (34.57 to 32.39%)

## ---------------------------- ##
## Housing regime - rent or buy ##
## ---------------------------- ##

table(INMO$tenen)

INMO$tenen <- revalue(INMO$tenen, c("En propiedad"="Own house/apartment", "En alquiler"="Rents house/apartment", 
                                              "Otras formas"="Other housing regime"))

INMO <- within(INMO, tenen <- relevel(tenen, ref = "Own house/apartment"))


## -------------------------------------- ##
## vehic - number of owned motor vehicles ## 
## -------------------------------------- ##

table(INMO$vehic)

INMO$vehic <- revalue(INMO$vehic, c("Uno"="One car", "Dos"="Two cars", 
                                              "Tres o más"="Three or more cars", "Ninguno"="no car"))

#### %%%%%%%%%% #### Test Area #### %%%%%%%%%%%% ####

## -------------------------------------- ##
## Social position mix of the three SES   ## 
## -------------------------------------- ##

# car education cross table
car.ed.tbl <- table(INMO$vehic,INMO$estudios4)
car.ed.ct <- round(100*prop.table(car.ed.tbl,2),digits=2)  # column percentage
car.ed.ct

# housing education crosstable
hou.ed.tbl <- table(INMO$tenen,INMO$estudios4)
hou.ed.ct <- round(100*prop.table(hou.ed.tbl,2),digits = 2) # column percentage
hou.ed.ct

### high SES is visible crossing the variables - even if owning the own house is very common throughout all categories



# apartment size education crosstable
siz.ed.tbl <- table(INMO$sut,INMO$estudios4)
siz.ed.ct <- round(100*prop.table(siz.ed.tbl,2),digits = 2) # column percentage
siz.ed.ct
# heating education crosstable
hea.ed.tbl <- table(INMO$calef,INMO$estudios4)
hea.ed.ct <- round(100*prop.table(hea.ed.tbl,2), digits = 2)
hea.ed.ct

## SES variable

# create groups 

# - high socioeconomic status = middle or high education + owns house + owns 2 or more cars
# has more than 100m2 + con Calefacion central
# - middle SES = Incomplete or Primary or middle education + owns or rents house + has more than 50 m2 + 

INMO$SES <- "Low"


INMO <- INMO %>% mutate()



#### %%%%%%%%% #### End Test Area #### %%%%%%%%% ####


#### 3. Generating the time to event data 

## --------------------------- ##
## a) Create an event variable ##
INMO$event <-  ifelse(INMO$tipob=="Fallecimiento",1,0)
table(INMO$event)                                         # no event= 667706   event= 69613

## ----------------------------- ##
## a) Generate the time variable ##


## --------------------------------- ##
## Adjust the time-to-event variable ##   - for the survival object

