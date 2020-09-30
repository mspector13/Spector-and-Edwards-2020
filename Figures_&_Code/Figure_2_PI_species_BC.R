## Bar chart of biomass of species used in P vs I experiments 

library(gdata)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(forcats)
library(RColorBrewer)

setwd("/Users/PikesStuff/github/SDSU_Thesis/Figures_&_Code/")

mydata<-read.xls("PI_species.xls")
head(mydata)


mydata$Time
levels(mydata$Time)
mydata$Time <- factor(mydata$Time, 
                      levels=c("Spring 2017" , "Summer 2017", "Winter 2018" , 
                                            "Spring 2018" , "Summer 2018"))

mydata$Site <- factor(mydata$Site, 
                      levels=c("Stillwater Cove", "Point Loma", "Campo Kennedy"))
#mydata$Species
#levels(mydata$Species)
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
                                  "Z. farlowii")

mydata$Species <- reorder(mydata$Species, mydata$Biomass)
#mydata$Species <- factor(mydata$Species, levels=rev(levels(mydata$Species)))

pal <- colorRampPalette(c("darkorange4", "lightyellow", "turquoise4"))

Fig2 <- ggplot() +
  geom_bar(aes(x= Time, y=Biomass, fill=Species), 
           data = mydata, stat = "identity", position = "stack")+
  scale_y_continuous(label = comma, limits = c(0,4000)) + 
  xlab("Date sampled") + ylab(expression('Biomass (g ww 0.5 m'^{2}*")"))+
  theme_classic(base_size = 16) +
  theme(legend.text = element_text(face = "italic")) +
  guides(fill=guide_legend(ncol=1))+
  facet_wrap(vars(Site), scales = "free", ncol = 3, nrow = 1) +
  theme(plot.title = element_text(hjust=0.5, size = 16)) +
  theme(axis.title=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = pal(12))
  
ggsave("Fig2.pdf", height=10, width=15, units='in')
ggsave("Fig2.png", height=10, width=15, units='in')
ggsave("Fig2.tiff", height=10, width=15, units='in', dpi=600)


  
  #scale_fill_manual(
    values = c("Botryocladia pseudodichotoma" = "darksalmon",
               "Cryptopleura ruprechtiana" = "burlywood2",
               "Dictyonerum californicum" = "tan",
               "Ecklonia arborea" = "blue4",
               "Gelidium spp." = "sienna3",
               "Laminaria farlowii" = "steelblue2",
               "Macrocystis pyrifera (juv)" = "cyan1",
               "Pterygophora californica" = "darkorange1",
               "Prionitis linearis" = "grey",
               "Sargassum horneri" = "grey5",
               "Stephanocystis osmundacea" = "brown2",
               "Zonaria farlowii" = "chocolate"))


  #scale_fill_grey(start = .9, end = 0)
  



