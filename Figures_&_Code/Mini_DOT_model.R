## Model of DO from MiniDots in forested and adjacent deforested habitats

setwd("/Users/PikesStuff/Desktop/Manuscript/Figures_&_Code/")
library(gdata)
library(forcats)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(readxl)
mydata<-read.xls("DO_All.xls", sheet = 1)
head(mydata)

mydata$Site <- factor(mydata$Site, levels=c("Stillwater Cove", "Point Loma", "Campo Kennedy"))
mydata$Treatment <- factor(mydata$Treatment, levels = c("Forested", "Deforested"))

#GEP <- ggplot(data=mydata, aes(x = Treatment, y = Daily.GCP, fill = Treatment, color = Treatment, shape = Treatment))+
  geom_boxplot(alpha=0.1, color="black") +
  stat_summary(fun.y = mean, colour="darkred", geom="point",shape=18, size=3, show_guide = FALSE) +
  theme_classic(base_size = 14)+
  facet_grid(~Site) +
  ylab ("Daily GEP (change in DO mg/l/hr)") +
  theme(legend.position='none') +
  scale_fill_manual(values = c("Forested"= "grey0", "Deforested" = "grey88")) 

NR <- ggplot(data=mydata, aes(x = Treatment, y = Daily.Re, fill = Treatment, color = Treatment, shape = Treatment))+
  geom_boxplot(alpha=0.5, color="black") +
  stat_summary(fun.y = mean, colour="darkred", geom="point",shape=18, size=3, show_guide = FALSE) +
  theme_classic(base_size = 14)+
  facet_grid(~Site) +
  ylab ("Nighttime Respiration (change in DO mg/l/hr)") +
  theme(legend.position='none') +
  scale_fill_manual(values = c("Forested"= "grey44", "Deforested" = "grey88"))     

DP <- ggplot(data=mydata, aes(x = Treatment, y = Daily.NCP, fill = Treatment, color = Treatment, shape = Treatment))+
  geom_boxplot(alpha=0.5, color="black") +
  stat_summary(fun.y = mean, colour="darkred", geom="point",shape=18, size=3, show_guide = FALSE) +
  theme_classic(base_size = 14)+
  facet_grid(~Site) +
  ylab ("Net Daytime Production (change in DO mg/l/hr)") +
  theme(legend.position='none') +
  scale_fill_manual(values = c("Forested"= "grey44", "Deforested" = "grey88")) 


ggarrange(DP + rremove("x.text") +rremove("xlab"), 
          NR + rremove("x.text")+rremove("xlab"), 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 1, common.legend = FALSE, legend= "bottom")




Daily.GEP<-aov(mydata$Daily.GCP~mydata$Site+mydata$Treatment+mydata$Site:mydata$Treatment)
anova(Daily.GEP)

Daily.ER<-aov(mydata$Daily.Re~mydata$Site+mydata$Treatment+mydata$Site:mydata$Treatment)
anova(Daily.ER)

Daily.NEP<-aov(mydata$Daily.NCP~mydata$Site+mydata$Treatment+mydata$Site:mydata$Treatment)
anova(Daily.NEP)
omega_sq(Daily.GEP)
omega_sq(Daily.ER)
omega_sq(Daily.NEP)