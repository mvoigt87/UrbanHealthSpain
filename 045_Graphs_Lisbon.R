### Changes to the plot for Lisbon presentation

### Labels 
label.urb <- c("Urbanicity Indicator", "Urbanicity Indicator", "Urbanicity Indicator", "Urbanicity Indicator")

### Hazard Ratios for Urban indicator in different models and Environmental/Social variables in Model 3-4
hazard.urb  <- c(1.0061,1.0308,1.0176,1.0027) 
lower.urb <- c(0.9963,1.0212,1.0058,0.9904)
upper.urb <- c(1.0158,1.0404,1.0295,1.015)
# color for different models
col.dev <- c("Model 1","Model 2","Model 3","Model 4")
df.urb <- data.frame(label.urb, hazard.urb, lower.urb, upper.urb,col.dev)

# # reverses the factor level ordering for labels after coord_flip()
df.urb$label.urb <- factor(df.urb$label.urb, levels=rev(df.urb$label.urb))

### Forest Plot
####----------------------------------------------
fp.urb.col = ggplot(data=df.urb,
                    aes(x = col.dev,y = hazard.urb, ymin = lower.urb, ymax = upper.urb))+
  geom_pointrange(aes(col=col.dev, shape=col.dev))+
  geom_hline(aes(fill=col.dev), yintercept =1, linetype=2)+
  xlab(' ')+ ylab("Hazard Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower.urb, ymax=upper.urb,col=col.dev),width=0.25,cex=0.5)+ 
  scale_color_manual(values=c("#D813FF", "#0A45FF", "#CC144C", "#15FF5E"), name=" ") +
  scale_fill_discrete(name = " ") +
  facet_wrap(~label.urb,strip.position="left",nrow=9,scales = "free_y") +
  theme_bw() +
  # theme(plot.title=element_text(size=12),
  #       axis.text.y=element_blank(),
  #       axis.ticks.y=element_blank(),
  #       axis.text.x=element_text(face="bold"),
  #       axis.title=element_text(size=12),
  #       strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))+
  coord_flip()
fp.urb.col <- fp.urb.col + theme(legend.position = c(0.85, 0.85), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  scale_shape_discrete(guide=FALSE)
fp.urb.col


################### Environmental factors

hazard <- c(1.0007, 1.0005, 1.002, 1.002, 0.9999, 0.9995, 1.0025, 1.0094)
lower <- c(1.0001, 0.9999, 1.0011, 1.0012, 0.9991, 0.9987, 1.0013, 1.007)
upper <- c(1.0012, 1.0011, 1.0029, 1.0029, 1.0008, 1.0004, 1.0036, 1.0119)
# Changing the labels
label <- c("Cleanness", "Cleanness", "Pollution" , "Pollution",
           "Noise", "Noise", "% Unemployed", "% Single HH")
col.dev <- c("Model 3", "Model 4", "Model 3", "Model 4", "Model 3", "Model 4", "Model 4", "Model 4")
df <- data.frame(label, hazard, lower, upper, col.dev)

### ----------------------------------------------------------------------------------------------------- ###
fp = ggplot(data=df,
            aes(x = col.dev,y = hazard, ymin = lower, ymax = upper))+
  geom_pointrange(aes(col=col.dev, shape=col.dev))+
  geom_hline(aes(fill=col.dev), yintercept =1, linetype=2)+
  xlab(' ')+ ylab("Hazard Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=col.dev),width=0.25,cex=0.5)+ 
  scale_color_manual(values=c("#FFF24C", "#3400B2"), labels=c("Model 3", "Model 4"), name=" ") +
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
fp <- fp + theme(legend.position = c(0.85, 0.1), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  scale_shape_discrete(guide=FALSE)
fp


### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ### 
### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ### 
### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ### 

### Variation in the hazard over time for different frailties (different sd of random effect)

