# This script loads Sensitivity.csv and standardizes using the z-score method.
# Then RDA is performed using the vegan package.
# Authors: Elizabeth Herdter
# For: Ocean Conservancy 
# Date Created: 7/20/16

#Load working directory
setwd("~/Desktop/Amberjack")

#Load pertinent packages
library(dplyr)
library(vegan)

#data(varespec)

#Load data and perform z-score standardization ( (x-u)/sd ) so that it has a mean of 0 and a standard dev of 1d

data <- data.frame(read.csv("Sensitivity.csv", header=TRUE) %>% apply(2,scale))
datasub<- data[-c(6,7,12,13),]

#Make predictor (X) matrix (Steepness, Rel.Mortality, M)
#Make response (Y) matrix

#Remove rows 6,7 and 12, 13 because we arent going to consider variation in Rel. mortality
# Also remove column 2 which is the Rel. Mortality column
cols<- c(1,3)
rows <- c(1:5, 8:11)
pred <- data[rows,cols] #Steepness,  M if you want to just plot with these
rownames(pred) <- c("run1", "run2", "run3", "run4", "run5", "run6", "run7", "run8", "run9")

#pred <- data[,c(1:2,4:14)] #Steepness, Rel.Mortality, age spec M
resp <- data[rows ,15:30] #R0,B0, Bcurrent, SSB0, SSBcurrent, Fcurr,  Fref_spr, SSBref_spr, Fratio_spr, SSBratio_spr, MSST, SSBref_MSST etc...
rownames(resp) <- c("run1", "run2", "run3", "run4", "run5", "run6", "run7", "run8", "run9")


#Model building- constrained ordination
data.rda <- rda(resp~ Steepness + M, data= pred)#Plotting example from below:
#http://www.inside-r.org/packages/cran/vegan/docs/ade2vegancca
  #data.rda <- rda(resp ~ Steepness + Rel.Mort + M0+ M1+ M2 +M3+ M4+ M5+ M6+ M7+ M8+ M9 +M10, data=pred)
plot.new()
plot(data.rda, type="n", xlab="", ylab="", scale="symmetric") #when type="n" no points. it just sets the frame of the plot
#remove the generic plot labels and then add them at the end- see below
text(data.rda, dis="cn") #dis is short for display- when dis="sp" it will plot response variables(species); when its ="si" it will plot the years(sites); when its = "cn" it will plot the predictor vectors
#points(data.rda, pch=21, col="red", bg="yellow", cex=1.2) - this is if we want the years(sites) plotted as unidentified points
text(data.rda, "species",  col="blue", cex=0.8) #plots the response data (species)
text(data.rda, "sites", col="red", cex=0.8)
title(xlab="RDA 1 (57.0%)", ylab= "RDA 2 (22.9%)")  #labeled axes with %variance explained. Unfortunately it doesnt look like this is a default in the vegan package so I had to hand calculcate below. 

#Determine % variance of each axis to put on axis labels
#http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf
# page 19-20
#total constrained variance is 26.3605. Inertia is correlations (or variance)
# So proportion of variance from first axis is = 15.065/26.3605

data.rda
per.var1 <- 11.628/20.38  #Inertia of axis 1/total
per.var2 <- 4.675/20.38 #Inertia of axis 2/total

# NOTES:
#1. I tested out plotting with the age-specific M values and they plot right on top of each other.
#   I left the code in place for you to take a look if you want to. It's commented out but just run the extra pred matrix and the rda command. 
#2. The runs are labeled by run number. We can change however we want by adjusting rowname command. There are 13 runs. one base run and 4 additional runs per parameter. 
#3. Vectors need to be added to the species(response) variables but I haven't figured out how to do that. 


############################
# Make multiplot of relationship between predictors and response
####################

# M first

par(mfrow=c(5,3))
par(mar=c(4,4,1,1))  
plot(M ~ Fcurr, data=datasub)
plot(M ~ R0, data=datasub)
plot(M ~ B0, data=datasub)
plot(M ~ SSB0, data=datasub)
plot(M ~ SSBcurrent, data=datasub)
plot(M ~ Fcurr, data=datasub)
plot(M ~ Fref_spr, data=datasub)
plot(M ~ SSBref_spr, data=datasub)
plot(M ~ MSST, data=datasub)
plot(M ~ SSBref_MSST, data=datasub)
plot(M ~ Fref_msy, data=datasub)
plot(M ~ SSBref_msy, data=datasub)
plot(M ~ Fratio_msy, data=datasub)
plot(M ~ SSBratio_msy, data=datasub)

# Steepness
par(mfrow=c(5,3))
par(mar=c(4,4,1,1))  
plot(Steepness ~ Fcurr, data=datasub)
plot(Steepness ~ R0, data=datasub)
plot(Steepness ~ B0, data=datasub)
plot(Steepness ~ SSB0, data=datasub)
plot(Steepness ~ SSBcurrent, data=datasub)
plot(Steepness ~ Fcurr, data=datasub)
plot(Steepness ~ Fref_spr, data=datasub)
plot(Steepness ~ SSBref_spr, data=datasub)
plot(Steepness ~ MSST, data=datasub)
plot(Steepness ~ SSBref_MSST, data=datasub)
plot(Steepness ~ Fref_msy, data=datasub)
plot(Steepness ~ SSBref_msy, data=datasub)
plot(Steepness ~ Fratio_msy, data=datasub)
plot(Steepness ~ SSBratio_msy, data=datasub)

