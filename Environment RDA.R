# This script loads Environment.csv and standardizes using the z-score method. Creates RDA ordination plot. 
# Then RDA is performed using the vegan package.
# Authors: Elizabeth Herdter
# For: Ocean Conservancy 
# Date Created: 7/21/16
# Date Completed: 7/27/16
# See NOTES at bottom. 

# 10/27/16
# Total mortality into Z0, Z1, Z2, Z3 and Zadult
# Remove Menhaden Effort and Landings

#Load working directory
setwd("~/Desktop/Amberjack")

#Load pertinent packages
library(dplyr)
library(vegan)

#Load data and perform z-score standardization ( (x-u)/sd ) so that it has a mean of 0 and a standard dev of 1
data <- data.frame(read.csv("Environment.csv", header=TRUE, row.names=1) %>% apply(2,scale))   #%>% mutate(Njuv= N1+N2+N3, Nadult= N4+N5+N6+N7+N8+N9+N10)

#Assign row names
name <- c(1980:2011)
rownames(data) <- name

#Make predictor (X) matrix (Catch, effort and discard data)
#Make response (Y) matrix
pred <- data[,1:28]
#select to 1:28 once we have data for prey abundance

resp <- data[,c(29:50, 58)] #rows 1990-2011, response columns, N1, N2, N3, Z0, Z1, Z2, Z3,Z4, Z5, Z6, Z7, Z8, Z9, Nadult
#missing values in last row for Z and N values

#Model building- constrained ordination from tutorial
#removed a few predictor variables in this list although they remain in the predictor dataframe
data.rda <- rda(resp ~ AWP.DOY +  SST.MAX + HURR.ACT + FERT.USE + PRECIP + STR.FLOW+NO3.LOAD +AREA.HYP +SED.CONC+ AMO.MEAN+ ZOO.SPR+ LAND.FIS +LAND.INV + LAND.REC+ REC.TRIP+ REC.DAYS+ N.BN.SHARK + N.BT.SHARK, data=pred, scale=TRUE)
# 10/27/16- removed lnd.men, eff.men (menhaden landings and effort), awp.mean (atlantic warm pool mean), sst.mean (mean offshore sea surface temperature) 




#Plotting example from below:
#http://www.inside-r.org/packages/cran/vegan/docs/ade2vegancca
plot.new()
plot(data.rda, type="n", xlab="", ylab="", scaling="sites") #when type="n" no points. it just sets the frame of the plot
#remove the generic plot labels and then add them at the end- see below
text(data.rda, dis="cn") #dis is short for display- when dis="sp" it will plot response variables(species); when its ="si" it will plot the years(sites); when its = "cn" it will plot the predictor vectors
#points(data.rda, pch=21, col="red", bg="yellow", cex=1.2) - this is if we want the years(sites) plotted as unidentified points
text(data.rda, "species",  col="blue", cex=0.8) #plots the response data (species)
text(data.rda, "sites", col="red", cex=0.8)
title(xlab="RDA 1 (38.4%)", ylab= "RDA 2 (21.5%)")  #labeled axes with %variance explained. Unfortunately it doesnt look like this is a default in the vegan package so I had to hand calculcate below. 

#Determine % variance of each axis to put on axis labels
#http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf
# page 19-20
#total constrained variance is 26.3605. Inertia is correlations (or variance)
# So proportion of variance from first axis is = 15.065/26.3605

data.rda
per.var1 <- 8.845/23.0 #Inertia of axis 1/total
per.var2 <- 4.96/23  #Inertia of axis 2/total


#NOTES: Same as those for Time Varying RDA
#1. Still not able to add arrows to the species scores (response data). Must figure out how to do that. 
#2. Might be worth combining numbers at age into total numbers... It seems very crowded and its obvious there is little difference in influence among all age specific numbers. 
#3. Recruits and N0 seem identical.. makes sense. I eliminated N0 to visually organize. 
#4. Total mortality at age also seems very crowded. It's not valid to just add them (at least I don't think) so maybe we can pick some benchmark ages and plot those or just chose 1 age. 
#5. In regards to #4, if we condense total mortality at age then we will be able to see SPR_ratio which may be more important to view. 
#6. Bratio and SPB are the same values once they are scaled so I removed Bratio
#7. ********* Seems that SPB and B0 were also same exact values so I removed B0



######################
# Plot with deviation in the response variables
# Upper
#######################

resp <- data[,c(29:50, 58)] #rows 1990-2011, response columns, N1, N2, N3, Z0, Z1, Z2, Z3,Z4, Z5, Z6, Z7, Z8, Z9, Nadult


