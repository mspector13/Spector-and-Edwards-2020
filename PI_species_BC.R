# Bar chart of biomass of species used in P vs I experiments 

library(gdata)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(forcats)
setwd("/Users/PikesStuff/Desktop/Manuscript/Figures_&_Code/")
mydata<-read.xls("PI_species.xls")
head(mydata)


mydata$Time
levels(mydata$Time)
mydata$Time <- factor(mydata$Time, 
                      levels=c("Spring 2017" , "Summer 2017", "Winter 2018" , 
                                            "Spring 2018" , "Summer 2018"))

mydata$Site <- factor(mydata$Site, 
                      levels=c("Stillwater Cove", "Point Loma", "Campo Kennedy"))
mydata$Species
levels(mydata$Species)
#mydata$Species <- factor(mydata$Species, 
                         levels=c("B. pseudodichotoma",
                                  "C. ruprechtiana",
                                  "D. californicum",
                                  "E. arborea",
                                  "Gelidium spp.",
                                  "L. farlowii",
                                  "M. pyrifera (juv)",
                                  "P. californica",
                                  "P. linearis",
                                  "S. horneri",
                                  "S. osmundacea",
                                  "Z. farlowii"))

mydata$Species <- reorder(mydata$Species, mydata$Biomass)
#mydata$Species <- factor(mydata$Species, levels=rev(levels(mydata$Species)))


ggplot() +
  geom_bar(aes(x= Time, y=Biomass, fill=Species), 
           data = mydata, stat = "identity", position = "stack")+
  scale_y_continuous(limits = c(0,4000))+ 
  xlab("Date Sampled") + ylab(expression('Biomass (g ww 0.5m'^{2}*")"))+
  theme_classic(base_size = 14)+
  theme(legend.text = element_text(face = "italic"))+
  #theme(legend.position='right') +
  guides(fill=guide_legend(ncol=1))+
  facet_wrap(vars(Site), scales = "free", ncol = 3, nrow = 1) +
  theme(plot.title = element_text(hjust=0.5, size = 14)) +
  theme(axis.title=element_text(size=14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_grey(start = .9, end = 0)

#lol:
  #scale_fill_manual(
    values = c("B. pseudodichotoma" = "gray1",
               "C. ruprechtiana" = "gray2",
               "D. californicum" = "grey80",
               "E. arborea" = "gray6",
               "Gelidium spp." = "grey6",
               "L. farlowii" = "grey40",
               "M. pyrifera (juv)" = "grey27",
               "P. californica" = "grey52",
               "P. linearis" = "gray0",
               "S. horneri" = "grey5",
               "S. osmundacea" = "grey",
               "Z. farlowii" = "grey0"))
