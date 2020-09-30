## Model generated from laboratory-derived estimates of productivity

setwd("/Users/PikesStuff/Desktop/Manuscript/Figures_&_Code")
library(ggplot2)
library(gdata)
library(plyr)
library(plotly)
library(ggpubr)
library(reshape2)

mydata<-read.xls("Model_actual.xls", sheet = 5)
head(mydata)


mydata$Site <- factor(mydata$Site, levels = c("Stillwater Cove", "Point Loma", "Campo Kennedy"))
mydata$Date <- factor(mydata$Date, levels = c("Spring 2017", "Summer 2017", "Winter 2018", "Spring 2018", "Summer 2018"))
#mydata$Treatment <-factor(mydata$Treatment, levels = c("GCP", "CR", "NCP"))

mean_NBP <- aggregate(NBP ~ Site:Date, data=mydata, mean) 
mean_NBP

mean_BR <- aggregate(BR ~ Site:Date, data=mydata, mean) 
mean_BR


BR <-  ggplot(data=mydata, aes(x = Date, y = BR, 
                               fill = Site, color = Site, shape = Site))+
  geom_boxplot(alpha=0.1, color="black", outlier.shape = NA) +
  stat_summary(fun = "mean", colour="darkred", geom="point",shape=18, size=2, 
               show_guide = FALSE, position = position_dodge(width = .75)) +
  theme_classic(base_size = 14)+
  facet_wrap(~Site) +
  theme(strip.text.x = element_blank()) +
  ylab(expression('BR (mg / m'^{2}*"/ day)"))+
  theme(legend.position='none') +
  coord_cartesian(ylim = c(-1600,0)) + 
  scale_fill_manual(values = c("Stillwater Cove"= "grey0", "Point Loma" = "grey64", "Campo Kennedy" = "gray88"))

NBP <-  ggplot(data=mydata, aes(x = Date, y = NBP, fill = Site, color = Site, shape = Site))+
  geom_boxplot(alpha=0.1, color="black", outlier.shape = NA) +
  stat_summary(fun = mean, colour="darkred", geom="point",shape=18, size=2, 
               show_guide = FALSE, position = position_dodge(width = .75)) +
  theme_classic(base_size = 14)+
  facet_wrap(~Site) +
  #theme(strip.text.x = element_blank()) +
  ylab(expression('NBP (mg / m'^{2}*"/ day)"))+
  theme(legend.position='none') +
  coord_cartesian(ylim = c(10,3000)) + 
  scale_fill_manual(values = c("Stillwater Cove"= "grey0", "Point Loma" = "grey64", "Campo Kennedy" = "gray88"))


ggarrange(#GCP + rremove("x.text") + rremove("xlab"),
          NBP  + rremove("x.text") + rremove("xlab"),
          BR, 
          ncol = 1, nrow = 2, legend = "none")

# Gross production: not relevent for this study, but may be helpful for future work -----------------------------------------------

#GCP <- ggplot(data=mydata, aes(x = Date, y = GCP, fill = Site, color = Site, shape = Site))+
geom_boxplot(alpha=0.1, color="black") +
  stat_summary(fun.y = mean, colour="darkred", geom="point",shape=18, size=3, 
               show_guide = FALSE, position = position_dodge(width = .75)) +
  theme_classic(base_size = 14)+
  facet_wrap(~Site) +
  ylab(expression('GBP (mg / m'^{2}*"/ day)"))+
  theme(legend.position='none') +
  ylim(-0,1000) +
  scale_fill_manual(values = c("Stillwater Cove"= "grey0", "Point Loma" = "grey64", "Campo Kennedy" = "gray88"))


