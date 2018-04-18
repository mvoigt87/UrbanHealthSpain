#### PAA - Urban Environment and Mortality Differentials in South Spain ####
#### ------------------------------------------------------------------ ####

### ---------------------------------------------------------------------------------------- ###
### Plan
###
### 1. Download and adjust individual data (including collapsing and changing categories)
### 2. Discriptive Statistics and correlations
### 3. Connect individual level data to the respective census tract information
### 4. Connect with spatial information
### ---------------------------------------------------------------------------------------- ###

## 0.1 - packages in use
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)
library(stargazer)
library(memisc)


##### 1. Download and transform data

## 1.1. Load data set - individual episodes (smp)
load('data/020_mp_smp_data.table.RData')

  ## ------- Create census tract variable form other area codes
  str(mp)
  head(mp)
  str(smp)

codpro <- c(4,11,14,18,21,23,29,41)
mp$CensusTract <- codpro[as.factor(mp$provincia)]*10^8 + mp$municipio*10^5 + mp$distrito*10^3  + mp$seccion
head(mp[,c('CensusTract','provincia','municipio','distrito','seccion')]  )

## Check for re-entries (probably return migrants - padron based (?))
sum(mp$id %in% unique(smp$id)) # 723234 of 737319 (1.9 % return migration cases)
setdiff(unique(smp$id),mp$id)

## 1.2. Create a new data set joining the follow up to the census information

INMO <- smp %>% inner_join(mp, by="id")

tbl_df(INMO) 

## delete original
rm(mp, smp)
## Test Event/Censor
table(INMO$tipob)
prop.table(table(INMO$tipob))
    # Emigraci?n fuera de Andaluc?a     Fallecimiento     Fin de estudio 
    # 43672                             69613             624034 
    # relative frequencies  
    # 0.06                              0.09              0.84

##### --------------------------------------------------------------------------------------------------- #####
##### 1.3. ---- Change variable names and collapse categories


## Census Tract Variable name for linking with the context file (SCCON)
cbind(names(INMO))
colnames(INMO)[64] <- "SC"


## ------------------------ ##
## Create an event variable ##
INMO$event <-  ifelse(INMO$tipob=="Fallecimiento",1,0)
table(INMO$event)                                         # no event= 667706   event= 69613
prop.table(table(INMO$event))

## ---------------------------------------------- ##
## a) Create time variable - for the survival object

# year at entering from the population 
summary(INMO$aalta)    ## pretty much everybody in 2002
# year at exit from the population 
summary(INMO$abaja)    ## most exits 2014

# time is exit - entry
INMO <- INMO %>% mutate(time = ifelse(abaja-aalta==0,0.05,abaja-aalta))

# Avoid problem that entry and exit year/time are the same by adding a small amount of time to the early exiters
# Just a problem for those who left/died within the first month - 1171 individuals

# overview and histogram of exit times (if death occurred)
summary(INMO$time[INMO$event==1])
hist(INMO$time[INMO$event==1])
# deaths are relatively equally distributed between the years 
#(there seem to occur more in the first and less in the last year)

## ---------------------------------------------- ##
# b) Everything by age

hist(INMO$fnac)   # histogram of birth years

INMO <- INMO %>% dplyr::mutate(age.entry = aalta-fnac) %>%     ## census year 2002 - the birth year
  # the second age variable is for the exit from the risk set calculated as the inital age + the time to death or censorship 
  dplyr::mutate(age.exit = age.entry+time)          
  # distribution of deaths
  INMO %>% dplyr::filter(event==1) %>% ggplot(aes(x=age.exit)) + 
    geom_histogram(binwidth = 1, color="black", fill="white") + theme_bw() # centered around 85 - looks plausible

### ----------- Changing categories for explanatory variables

## --- ##
## sex ##
## --- ##

INMO$sexo <- revalue(INMO$sexo, c("Hombre"="male","Mujer"="female"))

## ---------------------------- ##
## Estado civil - civil status  ##
## ---------------------------- ##

INMO$ecivil <- revalue(INMO$ecivil, c("Soltero/a"="Single", "Casado/a"="Married", "Viudo/a"="Widowed", 
                                      "Separado/a o divorciado/a"="Divorced/Sep"))

## -------------------------- ##
## Education - highest degree ##
## -------------------------- ##

INMO$estudios4 <- revalue(INMO$estudios4, c("No sabe leer o escribir"="No or Incomplete education", "Incompletos y sin estudios"="No or Incomplete education", 
                                            "Primer y segundo grado"="Primary/Lower Secondary Educ.", "Superiores, tercer grado"="Higher Secondary or Tertiary Educ."))
INMO <- within(INMO, estudios4 <- relevel(estudios4, ref = "Higher Secondary or Tertiary Educ."))

## ---------------------------- ##
## Housing regime - rent or buy ##
## ---------------------------- ##

INMO$tenen <- revalue(INMO$tenen, c("En propiedad"="Owns House/Apartment", "En alquiler"="Does not Own House/Aptm.", 
                                    "Otras formas"="Does not Own House/Aptm."))

INMO <- within(INMO, tenen <- relevel(tenen, ref = "Does not Own House/Aptm."))


## -------------------------------------- ##
## vehic - number of owned motor vehicles ## 
## -------------------------------------- ##

INMO$vehic <- revalue(INMO$vehic, c("Uno"="Owns car(s)", "Dos"="Owns car(s)", 
                                    "Tres o más"="Owns car(s)", "Ninguno"="no car"))


## --------------------------------------------------------------- ##
## sut - categorical variable on sqm area of the primary household ## 
## --------------------------------------------------------------- ##
table(INMO$sut)

INMO <- INMO %>% mutate(hhsize = sut) 

INMO$hhsize <- revalue(INMO$hhsize, c("<  25 m^2"="< 75 sqm", "De 25 y 50 m^2"="< 75 sqm", "De 51 y 75 m^2"="< 75 sqm", 
                                    "De 76 y 100 m^2"="76-100 sqm", "De 101 y 125 m^2"="> 100 sqm", 
                                    "De 126 y 150 m^2"="> 100 sqm", ">  de 150 m^2"="> 100 sqm"))

## ----------------------------------------------------- ##
## calef - categorical variable for existence of heating ## 
## ----------------------------------------------------- ##
table(INMO$calef)

## --------------------------------------------------------- ##
## mdesp -  main mean of transportation (lifestyle variable) ##
## --------------------------------------------------------- ##

table(INMO$mdesp)

INMO$mdesp <- revalue(INMO$mdesp, c("En automóvil privado"="By private car", "En moto"="By motorbike", 
                                              "En transporte colectivo y otros medios"="By public transport or others",
                                              "Andando o en bicicleta"="Walking or by bike"))

## --------------------------------------------------------- ##
## gnacionalidad -  nationality by continents                ##
## --------------------------------------------------------- ##

table(INMO$gnacionalidad)

INMO$NATIONAL <- 0
INMO$NATIONAL[INMO$gnacionalidad=="UE-15 (sin España), vigente en la fecha censal"] <- 1
INMO$NATIONAL[INMO$gnacionalidad=="UE-27 "] <- 1
INMO$NATIONAL[INMO$gnacionalidad=="Resto de Europa"] <- 1
INMO$NATIONAL[INMO$gnacionalidad=="África"] <- 2
INMO$NATIONAL[INMO$gnacionalidad=="Centro y Sudamérica"] <- 2
INMO$NATIONAL[INMO$gnacionalidad=="Norteamérica y Oceanía"] <- 2
INMO$NATIONAL[INMO$gnacionalidad=="Asia y apátridas"] <- 2


INMO$NATIONAL <- as.factor(INMO$NATIONAL)
INMO$NATIONAL <- revalue(INMO$NATIONAL, c("0"="Spanish", "1"="Europe without Spain", 
                                                    "2"="Rest of the World"))

table(INMO$NATIONAL)

## -------------------------------- ##  
## Occupation - collapse and rename ##
## -------------------------------- ##

table(INMO$ocupacion)

INMO$OCCUP <- "Not in occupation"
INMO$OCCUP[INMO$ocupacion== "Técnicos y profesionales. Técnicos e intelectuales"] <- "Technical & Interlectual Work" 
INMO$OCCUP[INMO$ocupacion== "Técnicos y profesionales de apoyo"] <- "Technical & Interlectual Work"
INMO$OCCUP[INMO$ocupacion== "Dirección de empresas y de las administraciones públicas"] <- "High Service and Directors"
INMO$OCCUP[INMO$ocupacion== "Empleados de tipo administrativo"] <- "Administrative Work"
INMO$OCCUP[INMO$ocupacion== "Trabajadores servicios de restauración, personales, protección y vendedores"] <- "Service Sector Work"
INMO$OCCUP[INMO$ocupacion== "Trabajadores cualificados agricultura y pesca"] <- "Work in Agriculture Sector"
INMO$OCCUP[INMO$ocupacion== "Artesanos trabajadores cualificados de las industrias manufactureras construcción y la minería"] <- "Mechanical and Industrial Work"
INMO$OCCUP[INMO$ocupacion== "Operadores de instalaciones y maquinaria y montadores"] <- "Mechanical and Industrial Work"
INMO$OCCUP[INMO$ocupacion== "Trabajadores no cualificados"] <- "Unqualified Work"

INMO$OCCUP <- as.factor(INMO$OCCUP)

table(INMO$OCCUP)
prop.table(table(INMO$OCCUP))

#### Change reference category for later analysis (ref=Mechanical and industrial work)
INMO <- within(INMO, OCCUP <- relevel(OCCUP, ref = "Mechanical and Industrial Work"))

### A second variable with less categories
INMO$OCCUP2 <- 0
INMO$OCCUP2[INMO$OCCUP=="Administrative Work"] <- 1
INMO$OCCUP2[INMO$OCCUP=="Service Sector Work"] <- 1
INMO$OCCUP2[INMO$OCCUP=="Technical & Interlectual Work"] <- 2
INMO$OCCUP2[INMO$OCCUP=="High service and Directors"] <- 2
INMO$OCCUP2[INMO$OCCUP=="Mechanical and Industrial Work"] <- 3
INMO$OCCUP2[INMO$OCCUP=="Work in Agriculture Sector"] <- 3
INMO$OCCUP2[INMO$OCCUP=="Unqualified Work"] <- 4
INMO$OCCUP2[INMO$jubilado=="Sí"] <- 5

table(INMO$OCCUP2) ### relatively good frequency distribution
INMO$OCCUP2 <- as.factor(INMO$OCCUP2)

INMO$OCCUP2 <- revalue(INMO$OCCUP2, c("0"="Without Occupation", "1"="No Manual Work", "2"="High Qualified Work", 
                                                "3"="Manual Work", "4"="Unqualified Work", "5"="Retired"))

round(prop.table(table(INMO$OCCUP2)),3)

#### Change reference category for later analysis (ref=Mechanical and industrial work)
INMO <- within(INMO, OCCUP2 <- relevel(OCCUP2, ref = "Manual Work"))


## ------------------------------------------------------------------------- ##  
## At home - For exposure to residential environment (unemployed or retired) ##
## ------------------------------------------------------------------------- ##

INMO <- INMO %>% mutate(athome = as.factor(ifelse(OCCUP2=="Without Occupation" | OCCUP2=="Retired","at home", "working")))

##### --------------------------------------------------------------------------------------------------- #####
##### --------------------------------------------------------------------------------------------------- #####
##### --------------------------------------------------------------------------------------------------- #####
##### --------------------------------------------------------------------------------------------------- #####


#### ---------- Upload and connect the Spatial Urban Index Data ---------------- ####
load("data/015_ContextAndUI.RData")

#### Testing which census tracts in the individual data set are covered urban indicators (Frans solution)
sum(unique(INMO$SC) %in% SCCON$SC)   # 5381
sum(SCCON$SC %in% unique(INMO$SC))   # 5381

#########################b##############################
# Connect census tract information to the individuum  #
#######################################################

INMO.SC <- INMO %>% left_join(SCCON, by="SC")

#### Create final working data set
  ## Selection of variables
  cbind(colnames(INMO.SC))

  INMO.SC <- INMO.SC %>% dplyr::select(id,aalta,abaja,felev,provincia,municipio,zona,distrito,seccion,
                                       fnac, sexo, lugnac, ecivil, trabajando, jubilado, dependiente,
                                       viudedad, estudios4, mdesp, tenen, pconta, pverde, pdelin, vehic,
                                       agua, conparhij, nmiem, SC, event, time, age.entry, age.exit,
                                       hhsize, NATIONAL, OCCUP, OCCUP2, PERSONAS, VIVIENDAS, PCT_OCUPADOS,
                                       PCT_SOLTEROS, PCT_TENENPROP, PCT_EDIFMAL, PCT_MAYORES, EDAD_MEDIA,
                                       PCT_AGRIC, IP_DELINC, IP_CONTAM, IP_HOGMONOP, NEDIFIC,
                                       Hospitals, Universities, Supermarkets, Open.Markets, ArtSurfA,
                                       Road.density, total.area.m2, total.area.km2, Service.area.popacc,
                                       pop.den, road.den, POPDEN.I.SD, ARTSURF.I.SD, ROADDEN.I.SD, SERAREA.I.SD,
                                       UI, UI.cat, UI.N, UI.logit, DI, DI.N, IP_RUIDOS, IP_CONTAM, IP_LIMPIEZA,
                                       IP_DELINC, IP_HOGMONOP, athome, IP_EDBAJO_TOTAL, PCT_TENENPROP)

#### Create final working data set
  
  save(INMO.SC, file = 'data/025_INDMOR-CT.Rdata')
  