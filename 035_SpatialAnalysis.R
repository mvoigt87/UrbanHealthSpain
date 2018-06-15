### ----------------------------------------------------------------------------------------- ###
###                        Check for Spatial Autocorrelation                                  ###
### ----------------------------------------------------------------------------------------- ###
library(readxl)
library(sp)
library(rgdal)
library(RColorBrewer)
library("gridExtra")
library("lattice")


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
                                   POPDEN.I.SD, ARTSURF.I.SD, ROADDEN.I.SD, SERAREA.I.SD, UI, UI.cat, UI.N, IP_DESEMPLEO, IP_CONTAM,
                                   IP_RUIDOS, PCT_SOLTEROS, IP_DELINC)

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
# Noise
moran.test(ANDALUS.SC$IP_RUIDOS, and.lw)                  ### high spatial correllation: Moran I statistic = 0.570***
# Pollution
moran.test(ANDALUS.SC$IP_CONTAM, and.lw)                  ### high spatial correllation: Moran I statistic = 0.444***
# Single Households
moran.test(ANDALUS.SC$PCT_SOLTEROS, and.lw)               ### high spatial correllation: Moran I statistic = 0.568***
# Unemployment
moran.test(ANDALUS.SC$IP_DESEMPLEO, and.lw)               ### high spatial correllation: Moran I statistic = 0.5004***
# Delinquency
moran.test(ANDALUS.SC$IP_DELINC, and.lw)                  ### high spatial correllation: Moran I statistic = 0.780***


    # Deprivation indicator
    # moran.test(ANDALUS.SC$DI.N, and.lw)                       ### highly spatially correlated: Moran I statistic = 0.685


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

