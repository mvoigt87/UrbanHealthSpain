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
### 2. Discriptive Statistics and correlations
### 3. Connect individual level data to the respective census tract information
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
# Emigración fuera de Andalucía     Fallecimiento     Fin de estudio 
# 43672                             69613             624034 

##### 1.3. ---- Change variable names and categories for following analysis


## Census Tract Variable name for later link with the context variables
cbind(names(INMO))
colnames(INMO)[64] <- "SC"


## ------------------------ ##
## Create an event variable ##
INMO$event <-  ifelse(INMO$tipob=="Fallecimiento",1,0)
table(INMO$event)                                         # no event= 667706   event= 69613

## --------------------------------- ##
## Adjust the time-to-event variable ##   - for the survival object

