# Luke Help ---------------------------------------------------------------

############################## - Dr. Luke Miller's adapted Webb Model
library(gdata)
# Each document has multiple species; one on each sheet
setwd("/Users/PikesStuff/Desktop/Never Science/What Ryhmes with Thesis?/PI_Model/")
mydata=read.xls("PI_PLS_3_18.xls", sheet = 3 )

#mydata=read.xls("Ptery_SWC.xls", sheet = 8 )

#Testing differences across times for same species (test=Ptery_SWC)
#setwd("/Users/PikesStuff/Desktop/")
#mydata=read.xls("Ptery_SWC.xls", sheet = 9)

mydata

# Alternate version where the O2 data are first scaled based on the smallest
# O2 value, which is negative due to respiration. This will shift all O2 
# values up above zero, allowing the fit of the power curve to all of the data
# We'll also add on a small additional amount (0.000001) to all values so that
# the smallest value is also slightly above zero, and thus can be log-transformed
mydata$O2scaled = mydata$O2 + abs(min(mydata$O2)) + 0.000001
range(mydata$O2scaled)
# The PAR = 0 values will also cause a problem in the log transform, so all PAR 
# values should have a small amount added to them 
mydata$PARscaled = mydata$PAR + 0.001

fit3 = lm(log(O2scaled)~log(PARscaled), data = mydata) # fit linearized (log-transformed) data
fit3 # show coefficients. These are based on a linear fit through the log-transformed
# PAR & O2 data

a = coef(fit3)[1] # extract coefficient a  (Intercept of the linear fit)
b = coef(fit3)[2] # extract coefficient b  (Slope of the linear fit)
# Since the coefficients a and b are from the log-transformed version of the
# power function, we need to backtransform coeff a so that it's usable in the
# original power function equation. R estimated 'a' based on the linearized 
# equation where we were fitting log(a) as the intercept,
# and therefore we need the exp() of a for use in the (original) power function
# equation.
# power function equation: O2 = a*(PAR)^b 
myx = seq(0.001,120,by=0.001) # generate a set of x values (PAR) to draw the fit line
# Generate a set of y values (O2) using the power function equation
backfitO2scaled = exp(a) * (myx^(b)) 
# Now undo the scaling that was applied to the data set
backfitO2unscaled = backfitO2scaled - abs(min(mydata$O2)) - 0.000001
backfitPARunscaled = myx - 0.001
# Plot the raw data points
plot(O2~PAR, data = mydata, cex = 1.5, col = 'red', pch = 20, las = 1, 
     main="INSERT ALGA HERE", ylab = "Dissolved Oxygen (mg/L)", xlab="PAR", 
     mgp=c(2.8,0.45,0), cex.lab = 1.5, cex.axis = 0.85)
#grid(lwd = 1.5, col = 'grey50') 
# Add the power function line to the plot
lines(x = backfitPARunscaled, y = backfitO2unscaled, lty = 100) 
#text(locator(1),"y=ax") 

exp(a)
b

mydata=read.xls("Ptery_SWC.xls", sheet = 10)
mydata
library(ggpubr)

with(mydata, shapiro.test(a))
with(mydata, shapiro.test(b))
res <- t.test(a, b, var.equal = TRUE)
res


