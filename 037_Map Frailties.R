### ----------------------------------------------------------------------------------------- ###
###                       Map Frailties / Random Effects                                      ###
### ----------------------------------------------------------------------------------------- ###

library(dplyr)
library(readxl)
library(sp)
library(rgdal)
library(RColorBrewer)
library("gridExtra")
library("lattice")

### Load spatial data - shape file

ANDALUS.SC <- readOGR(dsn="C:/Users/y4956294S/Documents/LONGPOP/Subproject 3 - Urban Environment and Health/RCode/UrbanHealthSpain/data/mapas.capas/SecCensales2001",
                      layer = "andalusia.census.tract.2001")

# access the dataframe of the spatial data frame

d <- ANDALUS.SC@data

# load frailty data and change census tract to factor

frail <- read.table("frailties.txt", header = T, sep = ";")

frail$region <- as.factor(as.numeric(frail$region))

frail <- frail %>% mutate(SC=region) %>% select(-region)

    # check for the census tracts in west Andalusia with the extra zero
    frail$SC <- as.character(as.factor(frail$SC))
    table(nchar(frail$SC))
    # here they are - add zero
    frail$SC[nchar(frail$SC)==9] <- paste0("0", frail$SC[nchar(frail$SC)==9])
    
# link information with spatial data frame

d <- d %>% mutate(SC=codigo01) %>% left_join(frail, by="SC")

### Now the data goes back to the spatial object
ANDALUS.SC@data <- d

#### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #####

### Mapping frailties

# color specifications for continous measures
display.brewer.all()

# continuous color palatte
# myCols <- adjustcolor(colorRampPalette(brewer.pal(n=9, 'Oranges'))(100), .85)

# centered quasi discrete
myCols <- adjustcolor(colorRampPalette(brewer.pal(n=9, 'RdBu'))(100), .85)

## Andalusia
AND.frail <- spplot(ANDALUS.SC,"frail",col.regions=myCols, scales=list(draw = TRUE), colorkey=TRUE)


## Sevilla

SEV.city <- c("41091")
SEV <- subset(ANDALUS.SC, codmun %in% SEV.city)

SEV.frail.plot <- spplot(SEV, "frail",col.regions=myCols, scales=list(draw = TRUE), colorkey=TRUE)
