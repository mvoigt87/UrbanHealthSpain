### ---------------------------------- ###  
### Graphs for urbanicity presentation
### ---------------------------------- ###

#### Only possible when "030_DecriptSurvAnalysis.R" was run

library(forestplot)
library(survminer)


## Test - ahh not really beautiful
ggforest(Mod.3)


### Self-made plots
### ---------------
### forest plot for the urban indicator
### -----------------------------------

# label.urb <- c("Urban Ind.(Cox)", "Urban Ind.(Mix)", "Urban Ind. (Devel.)" , "Urban Ind. (full)")

### Alternative Labels 
label.urb <- c("Model 1 (Only Indicator (UI))   ", "Model 2 (+ Individual Var.)  ", "Model 3 (+ Environment Var.)" , "Model 4 (+ Pop.-Based Var.) ")

hazard.urb  <- c(1.0061,1.0308,1.0176,1.0027) 
lower.urb <- c(0.9963,1.0212,1.0058,0.9904)
upper.urb <- c(1.0158,1.0404,1.0295,1.015)
col.urb <- c("MC1","MC1","MC1","MC2")
df.urb <- data.frame(label.urb, hazard.urb, lower.urb, upper.urb,col.urb)

# reverses the factor level ordering for labels after coord_flip()
df.urb$label.urb <- factor(df.urb$label.urb, levels=rev(df.urb$label.urb))


fp.urb <- ggplot(data=df.urb, aes(x=label.urb, y=hazard.urb, ymin=lower.urb, ymax=upper.urb, color=col.urb)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(" ") + ylab("Hazard Ratios (95% CI)") +
  scale_color_manual(values=c("#FF6934", "#33FFD5"), name=" ") +
  theme_bw()   + # use a white background
  theme(legend.position="none")
print(fp.urb)

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




### forest plot for Indicator Components

###################################################################################################################
###################################################################################################################
### Prepare the input data
label <- c("Population Density (S)", "Population Density (F)", "Artificial Surface (S)" , "Artificial Surface (F)",
           "Health Service (S)", "Health Service (F)", "Road Density (S)", "Road Density (F)")
hazard  <- c(0.9949,0.9962,0.97533,0.9857,1.0023,0.9942,1.0346,1.0156)
lower <- c(0.9733,0.9755,0.9597,0.9706,0.9904,0.9821,1.0161,0.9979)
upper <- c(1.0165,1.0168,0.9909,1.0008,1.0143,1.0062,1.0531,1.0333)
# color full and simple model differently
col.dev <- c("S","F","S","F","S","F","S","F")

df <- data.frame(label, hazard, lower, upper, col.dev)
# reverses the factor level ordering for labels after coord_flip()
df$label <- factor(df$label, levels=rev(df$label))
###################################################################################################################
###################################################################################################################
    # fade out insignificant values
    # alpha_vector = rep(0.25, nrow(df))
    # alpha_vector[c(3,5,6,7,8)] = 1

    # df$alpha = alpha_vector

fp.comp <- ggplot(data=df, aes(x=label, y=hazard, ymin=lower, ymax=upper,color=col.dev)) +
  geom_pointrange(show.legend=FALSE) +
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(" ") + ylab("Hazard Ratios (95% CI)") +
  scale_color_manual(values=c("#FF6934", "#33FFD5"), name=" ") +
  theme_bw()  # use a white background
print(fp.comp)

###################################################################################################################
###################################################################################################################

### different line types for black and white plot - for publication

fp.comp.2 <- ggplot(data=df, aes(x=label, y=hazard, ymin=lower, ymax=upper,linetype=col.dev, shape=col.dev, color=col.dev)) +
  geom_pointrange(show.legend=FALSE) +
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(" ") + ylab("Hazard Ratios (95% CI)") +
  scale_color_manual(values=c("#000000", "#A9A9A9"), name=" ") +
  theme_bw()  # use a white background
fp.comp.2 <- fp.comp.2 + theme(legend.position = c(0.15, 0.15))
print(fp.comp.2)

###################################################################################################################
###################################################################################################################
### Another attempt with groups - for publication
#   (https://datascienceplus.com/lattice-like-forest-plot-using-ggplot2-in-r/)

# Changing the labels
label <- c("Population Density", "Population Density", "Artificial Surface" , "Artificial Surface",
           "Health Service", "Health Service", "Road Density", "Road Density")
df <- data.frame(label, hazard, lower, upper, col.dev)

### ----------------------------------------------------------------------------------------------------- ###
fp = ggplot(data=df,
           aes(x = col.dev,y = hazard, ymin = lower, ymax = upper))+
  geom_pointrange(aes(col=col.dev, shape=col.dev))+
  geom_hline(aes(fill=col.dev), yintercept =1, linetype=2)+
  xlab(' ')+ ylab("Hazard Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=col.dev),width=0.25,cex=0.5)+ 
  scale_color_manual(values=c("#000000", "#A9A9A9"), labels=c("Full Model", "Model 1"), name=" ") +
  scale_fill_discrete(name = " ") +
  facet_wrap(~label,strip.position="left",nrow=9,scales = "free_y") +
  theme_bw() +
   # theme(plot.title=element_text(size=12),
  #       axis.text.y=element_blank(),
  #       axis.ticks.y=element_blank(),
  #       axis.text.x=element_text(face="bold"),
  #       axis.title=element_text(size=12),
  #       strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))+
  coord_flip()
fp <- fp + theme(legend.position = c(0.85, 0.9), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  scale_shape_discrete(guide=FALSE)
fp

### -------------
### With colors!
### -------------
fp.col = ggplot(data=df,
            aes(x = col.dev,y = hazard, ymin = lower, ymax = upper))+
  geom_pointrange(aes(col=col.dev, shape=col.dev))+
  geom_hline(aes(fill=col.dev), yintercept =1, linetype=2)+
  xlab(' ')+ ylab("Hazard Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=col.dev),width=0.25,cex=0.5)+ 
  scale_color_manual(values=c("#FFF24C", "#3400B2"), labels=c("Full Model", "Model 1"), name=" ") +
  scale_fill_discrete(name = " ") +
  facet_wrap(~label,strip.position="left",nrow=9,scales = "free_y") +
  theme_bw() +
  # theme(plot.title=element_text(size=12),
  #       axis.text.y=element_blank(),
  #       axis.ticks.y=element_blank(),
  #       axis.text.x=element_text(face="bold"),
  #       axis.title=element_text(size=12),
  #       strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))+
  coord_flip()
fp.col <- fp.col + theme(legend.position = c(0.85, 0.9), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  scale_shape_discrete(guide=FALSE)
fp.col

###################################################################################################################
###################################################################################################################




### KMEs Poster

# Sex
KM.female <- survfit(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ 1, data = subset(INMO.SC,sexo=="female"), type="kaplan-meier")

KM.male <- survfit(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ 1, data = subset(INMO.SC,sexo=="male"), type="kaplan-meier")  

KM.FEM <- tidy(KM.female) %>% dplyr::select(time,estimate) %>% mutate(sex = "female")
KM.MAL <- tidy(KM.male) %>% dplyr::select(time, estimate) %>% mutate(sex = "male")

KM.SEX <- union(KM.FEM,KM.MAL)

SEX.plot <- KM.SEX %>% ggplot(aes(x=time,y=estimate,color=sex)) +
  geom_step() +
  scale_colour_manual(values = c("#FF6934", "#33FFD5"), name="")     +
  scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Estimated Survival Probability") +
  theme_bw()

SEX.plot <- SEX.plot + theme(legend.position = c(0.15, 0.15))
# ---------------------------------------------------------------------------- 

# Education
KM.lower.ED <- survfit(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ 1, data = subset(INMO.SC,estudios4=="No or Incomplete education"), type="kaplan-meier")

KM.med.ED <- survfit(Surv(time = age.entry,
                        time2 = age.exit,
                        event = event) ~ 1, data = subset(INMO.SC,estudios4=="Primary/Lower Secondary Educ."), type="kaplan-meier")  

KM.high.ED <- survfit(Surv(time = age.entry,
                          time2 = age.exit,
                          event = event) ~ 1, data = subset(INMO.SC,estudios4=="Higher Secondary or Tertiary Educ."), type="kaplan-meier") 

KM.LOW <- tidy(KM.lower.ED) %>% dplyr::select(time,estimate) %>% mutate(edu = "No or incomplete Ed.")
KM.MED <- tidy(KM.med.ED) %>% dplyr::select(time, estimate) %>% mutate(edu = "Max. Lower Second. Ed.")
KM.HIG <- tidy(KM.high.ED) %>% dplyr::select(time, estimate) %>% mutate(edu = "Higher Ed.")

KM.EDU <- union(KM.LOW,KM.MED) %>% union(KM.HIG)

EDU.plot <- KM.EDU %>% ggplot(aes(x=time,y=estimate,color=edu)) +
  geom_step() +
  scale_colour_manual(values = c("#FF6934", "#26B7FF","#26FF57"), name="")     +
  scale_x_continuous(name = "Age") +
  scale_y_continuous(name = " ") +
  theme_bw()
EDU.plot <- EDU.plot + theme(legend.position = c(0.2, 0.2))

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

### forest plot for the single indicator variables
### ----------------------------------------------


### Alternative Labels
label.urb.I <- c("Population Den. (Cox)", "Population Den. (Mix)", "Artif. Surface (Cox)" , "Artif. Surface (Mix)",
                 "Road Den. (Cox)", "Road Den. (Mix)", "Service Area (Cox)", "Service Area (Mix)")

hazard.urb.I  <- c(0.9925,0.9943,0.9817,0.9808,1.0295,1.0285,0.9995,0.9999) 
lower.urb.I <- c(0.9760,0.9754,0.9698,0.9671,1.0149,1.0123,0.9903,0.9894)
upper.urb.I <- c(1.0093,1.0132,0.9937,0.9946,1.0444,1.0447,1.0087,1.0104)
col.urb.I <- c("C","MC","C","MC","C","MC","C","MC")
df.urb.I <- data.frame(label.urb.I, hazard.urb.I, lower.urb.I, upper.urb.I,col.urb.I)

# reverses the factor level ordering for labels after coord_flip()
df.urb.I$label.urb.I <- factor(df.urb.I$label.urb.I, levels=rev(df.urb.I$label.urb.I))


fp.urb.I <- ggplot(data=df.urb.I, aes(x=label.urb.I, y=hazard.urb.I, ymin=lower.urb.I, ymax=upper.urb.I, color=col.urb.I)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(" ") + ylab("Hazard Ratios (95% CI)") +
  scale_color_brewer(palette="Dark2", name=" ")              +
  theme_bw()   + # use a white background
  theme(legend.position="none")
print(fp.urb.I)


# theme(text = element_text(size=20),
#        axis.text.x = element_text(angle=90, hjust=1)) 


### forest plot for the development indicator
### -----------------------------------------

label.dev.I <- c("Noise (Cox)", "Noise (Mix)", "Pollution (Cox)", "Pollution (Mix)", "Cleanness (Cox)", "Cleanness (Mix)",
                 "Delinquency (Cox)", "Delinquency (Mix)", "Homogeneity (Cox)", "Homogeneity (Mix)")

hazard.dev.I  <- c(0.9976,0.9976,1.0023,1.0023,1.0003,1.0003,1.0008,1.0008,1.0066,1.0069) 
lower.dev.I <- c(0.9969,0.9969,1.0015,1.0014,0.9998,0.9998,1.0003,1.0002,1.0045,1.0045)
upper.dev.I <- c(0.9982,0.9983,1.0030,1.0031,1.00008,1.0008,1.0013,1.0013,1.0087,1.0092)
col.dev.I <- c("C","MC","C","MC","C","MC","C","MC","C","MC")
df.dev.I <- data.frame(label.dev.I, hazard.dev.I, lower.dev.I, upper.dev.I,col.dev.I)

# reverses the factor level ordering for labels after coord_flip()
df.dev.I$label.dev.I <- factor(df.dev.I$label.dev.I, levels=rev(df.dev.I$label.dev.I))


fp.dev.I <- ggplot(data=df.dev.I, aes(x=label.dev.I, y=hazard.dev.I, ymin=lower.dev.I, ymax=upper.dev.I, color=col.dev.I)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(" ") + ylab("Hazard Ratios (95% CI)") +
  scale_color_brewer(palette="Dark2", name=" ")              +
  theme_bw()  # use a white background
print(fp.dev.I)

# grid.arrange(fp.urb.I, fp.dev.I, ncol=2)


##########################################################################################################################
##########################################################################################################################

##########################################################################################################################
##########################################################################################################################  
### Model 4

#                                         exp(coef) exp(-coef) lower .95 upper .95
# POPDEN.I.SD                               0.9936     1.0064    0.9769    1.0106
# ARTSURF.I.SD                              0.9890     1.0112    0.9768    1.0013
# ROADDEN.I.SD                              1.0151     0.9851    1.0005    1.0299
# SERAREA.I.SD                              0.9935     1.0066    0.9837    1.0033
# IP_RUIDOS                                 0.9991     1.0009    0.9984    0.9998
# IP_CONTAM                                 1.0018     0.9982    1.0010    1.0025
# IP_LIMPIEZA                               1.0001     0.9999    0.9996    1.0006
# IP_DELINC                                 1.0011     0.9989    1.0006    1.0016
# IP_HOGMONOP                               1.0070     0.9931    1.0047    1.0092
# sexomale                                  1.9027     0.5256    1.8715    1.9345
# depdependent                              2.1725     0.4603    2.0731    2.2767
# ecivilSingle                              1.3562     0.7373    1.3205    1.3929
# ecivilWidowed                             1.1922     0.8388    1.1683    1.2165
# ecivilDivorced/Sep                        1.4342     0.6972    1.3605    1.5119
# fnac                                      0.9823     1.0180    0.9801    0.9845
# estudios4No or Incomplete education       1.3706     0.7296    1.3203    1.4228
# estudios4Primary/Lower Secondary Educ.    1.1301     0.8849    1.0853    1.1768
# tenenDoes not Own House/Aptm.             1.1174     0.8949    1.0924    1.1430
# vehicno car                               1.2047     0.8301    1.1845    1.2252

# Mix Model

#                                                 coef exp(coef)     se(coef)      z       p
# POPDEN.I.SD                            -5.748144e-03 0.9942683 0.0093351014  -0.62 5.4e-01
# ARTSURF.I.SD                           -1.199252e-02 0.9880791 0.0068088528  -1.76 7.8e-02
# ROADDEN.I.SD                            1.435048e-02 1.0144539 0.0080007588   1.79 7.3e-02
# SERAREA.I.SD                           -6.395995e-03 0.9936244 0.0054330979  -1.18 2.4e-01
# IP_RUIDOS                              -9.133913e-04 0.9990870 0.0003929551  -2.32 2.0e-02
# IP_CONTAM                               1.754974e-03 1.0017565 0.0004044607   4.34 1.4e-05
# IP_LIMPIEZA                             6.567546e-05 1.0000657 0.0002665121   0.25 8.1e-01
# IP_DELINC                               1.137936e-03 1.0011386 0.0002759977   4.12 3.7e-05
# IP_HOGMONOP                             6.918135e-03 1.0069421 0.0012259096   5.64 1.7e-08
# sexomale                                6.456597e-01 1.9072449 0.0084849856  76.09 0.0e+00
# depdependent                            7.858456e-01 2.1942617 0.0241968016  32.48 0.0e+00
# ecivilSingle                            3.066040e-01 1.3588028 0.0136885620  22.40 0.0e+00
# ecivilWidowed                           1.755634e-01 1.1919176 0.0103700420  16.93 0.0e+00
# ecivilDivorced/Sep                      3.666748e-01 1.4429286 0.0269925792  13.58 0.0e+00
# fnac                                   -1.658722e-02 0.9835496 0.0011520391 -14.40 0.0e+00
# estudios4No or Incomplete education     3.052609e-01 1.3569789 0.0192422479  15.86 0.0e+00
# estudios4Primary/Lower Secondary Educ.  1.200712e-01 1.1275772 0.0207179259   5.80 6.8e-09
# tenenDoes not Own House/Aptm.           1.114098e-01 1.1178529 0.0116916130   9.53 0.0e+00
# vehicno car                             1.862262e-01 1.2046947 0.0086693759  21.48 0.0e+00


##########################################################################################################################
##########################################################################################################################

#### !!! FULL MODEL EFFFECTS

### forest plot for the development indicator
### -----------------------------------------

label.dev.IF <- c("Noise (Cox)", "Noise (Mix)", "Pollution (Cox)", "Pollution (Mix)", "Cleanness (Cox)", "Cleanness (Mix)",
                  "Delinquency (Cox)", "Delinquency (Mix)", "Homogeneity (Cox)", "Homogeneity (Mix)")

hazard.dev.IF  <- c(0.9991,0.9991,1.0018,1.0017,1.0001,1.0001,1.0011,1.0011,1.0070,1.0069) 
lower.dev.IF <- c(0.9984,0.9983,1.0010,1.0010,0.9996,0.9995,1.0006,1.0006,1.0047,1.0045)
upper.dev.IF <- c(0.9998,0.9998,1.0025,1.0025,1.0006,1.0006,1.0016,1.0017,1.0092,1.0093)
col.dev.IF <- c("C","MC","C","MC","C","MC","C","MC","C","MC")
df.dev.IF <- data.frame(label.dev.IF, hazard.dev.IF, lower.dev.IF, upper.dev.IF,col.dev.IF)

# reverses the factor level ordering for labels after coord_flip()
df.dev.IF$label.dev.IF <- factor(df.dev.IF$label.dev.IF, levels=rev(df.dev.IF$label.dev.IF))


fp.dev.IF <- ggplot(data=df.dev.IF, aes(x=label.dev.IF, y=hazard.dev.IF, ymin=lower.dev.IF, ymax=upper.dev.IF, color=col.dev.IF)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(" ") + ylab("Hazard Ratios (95% CI)") +
  scale_color_brewer(palette="Dark2", name=" ")              +
  theme_bw()  # use a white background
print(fp.dev.IF)


### forest plot for the urban single indicator variables (Full Model)
### ------------------------------------------------------------


### Alternative Labels
label.urb.IF <- c("Population Den. (Cox)", "Population Den. (Mix)", "Artif. Surface (Cox)" , "Artif. Surface (Mix)",
                  "Road Den. (Cox)", "Road Den. (Mix)", "Service Area (Cox)", "Service Area (Mix)")

hazard.urb.IF  <- c(0.9936,0.9943,0.9890,0.9881,1.0151,1.0144,0.9935,0.9936) 
lower.urb.IF <- c(0.9769,0.9760,0.9768,0.9747,1.0005,0.9988,0.9837,0.9830)
upper.urb.IF <- c(1.0106, 1.0126,1.0013,1.0014,1.0299,1.0301,1.0033,1.004)
col.urb.IF <- c("C","MC","C","MC","C","MC","C","MC")
df.urb.IF <- data.frame(label.urb.IF, hazard.urb.IF, lower.urb.IF, upper.urb.IF,col.urb.IF)

# reverses the factor level ordering for labels after coord_flip()
df.urb.IF$label.urb.IF <- factor(df.urb.IF$label.urb.IF, levels=rev(df.urb.IF$label.urb.IF))


fp.urb.IF <- ggplot(data=df.urb.IF, aes(x=label.urb.IF, y=hazard.urb.IF, ymin=lower.urb.IF, ymax=upper.urb.IF, color=col.urb.IF)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab(" ") + ylab("Hazard Ratios (95% CI)") +
  scale_color_brewer(palette="Dark2", name=" ")              +
  theme_bw()   + # use a white background
  theme(legend.position="none")
print(fp.urb.IF)


# theme(text = element_text(size=20),
#        axis.text.x = element_text(angle=90, hjust=1)) 


############################################################################################################################
############################################################################################################################
# 
#   ### For Dariya:
# 
#  FOR.MAP <- SCCON %>% dplyr::select(SC, PERSONAS, MUNI, EDAD_MEDIA, ArtSurfA, Service.area.popacc,
#                                  POPDEN.I.SD, ARTSURF.I.SD, ROADDEN.I.SD, SERAREA.I.SD, UI.N, DI.N)
#   table(nchar(FOR.MAP$SC))
# # known error! - add the zero to the "SC"
# 
#   FOR.MAP$SC[nchar(FOR.MAP$SC)==9] <- paste0("0", FOR.MAP$SC[nchar(FOR.MAP$SC)==9])
# 
# ### Add SMR
# SMRsc$SC <- as.character(as.factor(SMRsc$SC))
# FOR.MAP$SC <- as.character(as.factor(FOR.MAP$SC))
# FOR.MAP <- FOR.MAP %>% left_join(SMRsc, by="SC")
# 
# ### Save as txt
# write.table(FOR.MAP,file = "ForMap.txt",sep = ";",row.names=FALSE)
