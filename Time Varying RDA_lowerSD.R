# ABOUT ####
# This script loads TimeVarying86_withvariance.csv and standardizes using the z-score method. Creates RDA ordination plot. 
# Makes plots with lower standard deviation of response variables that have associated SD.
# Uses the multiplot function to make a multiplot of TimeVarying Lower and Upper. 
# Then RDA is performed using the vegan package.
# Authors: Elizabeth Herdter
# For: Ocean Conservancy 
# Date Created: 12/14/16


#Load working directory####
setwd("~/Desktop/Amberjack")

#Load pertinent packages####
library(dplyr)
library(vegan)


#Load data and perform z-score standardization ( (x-u)/sd ) so that it has a mean of 0 and a standard dev of 1####
data <- data.frame(read.csv("TimeVarying1986_withvariance.csv", header=TRUE, row.names=1) %>% apply(2,scale)) # %>% mutate(Njuv= N1+N2+N3, Nadult= N4+N5+N6+N7+N8+N9+N10)
# I added Nadult into the .csv because I realized that I shouldnt add scaled numbers as I had done above - EH 10/27/16 

#Make predictor (X) matrix (Catch, effort and discard data)
#Make response (Y) matrix
pred <- data[5:26,1:10] #rows 1990-2011
resp_lowerSD <- data[5:26,c(14,17,21,25,29,31:48,56)] #rows 1990-2011
resp_lowerSD <- rename(resp_lowerSD, SPB=SPBLSD, SPR=SPRratioLSD, Recruits=RecruitsLSD, TotalF=FLSD, Bratio=BratioLSD)
#
#this way it removes the years that had to be interpolated- starts at 1990 instead
#Mike, once I chose the starting year of 1990 the horseshoe pattern seemed to dissamble a bit. 
#I think it looked that way because the first four yers of the 1986 data set were estimated by just averaging. 
# Starting in 1990 is more valid, I think, because I belive it more accurately depicts the patterns in the data.

#Assign non-generic row names to years for plotting purposes
name <- c(1990:2011)
rownames(pred) <- name
rownames(resp_lowerSD) <- name


#this has data from 1986+. It contains some interpolated data which I believe causes the unidirectional nature seen in the ordination plot below. 
#pred <- data[1:26,1:10]
#resp <- data[1:26,11:42] #missing values in last row for Z and N values


#Model building- constrained ordination from tutorial ####
data.rda_lower <- rda(resp_lowerSD ~ C_COMHL+C_COMLL+C_REC+C_HB+CPUE_COMLL+CPUE_COMHL+CPUE_MRFSS+CPUE_HB+D_REC+D_HB, data=pred, scale=TRUE)

## Variable selection ####
#https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/vegan-package
mod0 <- rda(resp_lowerSD ~ 1, pred) #No constraints:like a PCA
plot(mod0)
mod1 <- rda(resp_lowerSD ~., pred)
mod1
plot(mod1)
mod <- ordistep(mod0, scope=formula(mod1))
anova(mod)
anova(mod, by="margin")

#Step: resp ~ C_COMHL + C_REC + CPUE_COMLL + C_HB + CPUE_COMHL + D_HB +  CPUE_MRFSS 

#Step: resp ~ C_COMHL + C_REC + CPUE_COMLL + C_HB + CPUE_COMHL + D_HB +  CPUE_MRFSS 

pred <- pred %>% select(C_COMHL, C_REC, CPUE_COMLL, C_HB, CPUE_COMHL, D_HB, CPUE_MRFSS)
data.rda_lower <- rda(resp_lowerSD ~., pred)



#Plotting example from below:
#http://www.inside-r.org/packages/cran/vegan/docs/ade2vegancca
plot.new()
plot(data.rda_lower, type="n", xlab="", ylab="") 
ordipointlabel(data.rda_lower, display="species", scaling="sites", add=TRUE, col="blue")
#ordipointlabel(data.rda_lower, display="sites", scaling="symm", add=TRUE)
text(data.rda_lower, dis="cn") 
#points(data.rda_lower, pch=21, col="red", bg="yellow", cex=1.2) - this is if we want the years(sites) plotted as unidentified points
#text(data.rda_lower, "species",  col="blue", cex=0.8) #plots the response data (species)
text(data.rda_lower, "sites", col="red", cex=0.8)
title(xlab="RDA 1 (54.7%)", ylab= "RDA 2 (17.4%)")  #labeled axes with %variance explained. Unfortunately it doesnt look like this is a default in the vegan package so I had to hand calculcate below. 

#when type="n" no points. it just sets the frame of the plot
#remove the generic plot labels and then add them at the end- see below
#dis is short for display- when dis="sp" it will plot response variables(species); when its ="si" it will plot the years(sites); when its = "cn" it will plot the predictor vectors

#MUST RE_DO THIS STEP IF REMOVING DATA
#Determine % variance of each axis to put on axis labels
#http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf
# page 19-20
#total constrained variance is 26.3605. Inertia is correlations (or variance)
# So proportion of variance from first axis is = 15.065/26.3605

data.rda_lower

per.var1 <- 10.273/18.7650 #Inertia of axis 1/total
per.var2 <- 3.276/18.7650   #Inertia of axis 2/total


#Stack LOWER and UPPER in same plot####
# MUST have run Time Varying RDA_upperSD.R first 

par(mfrow=c(2,1), mar=c(4,5,2,2))
plot(data.rda_upper, type="n", xlab="", ylab="") 
ordipointlabel(data.rda_upper, display="species", scaling="sites", add=TRUE, col="blue")
#ordipointlabel(data.rda_upper, display="sites", scaling="symm", add=TRUE)
text(data.rda_upper, dis="cn") 
#points(data.rda_upper, pch=21, col="red", bg="yellow", cex=1.2) - this is if we want the years(sites) plotted as unidentified points
#text(data.rda_upper, "species",  col="blue", cex=0.8) #plots the response data (species)
text(data.rda_upper, "sites", col="red", cex=0.8)
title(xlab="RDA 1 (56.7%)", ylab= "RDA 2 (17.0%)")  #labeled axes with %variance explained. Unfortunately it doesnt look like this is a default in the vegan package so I had to hand calculcate below. 
text(-2.75, 1.5, labels="A")

plot(data.rda_lower, type="n", xlab="", ylab="") 
ordipointlabel(data.rda_lower, display="species", scaling="sites", add=TRUE, col="blue")
#ordipointlabel(data.rda_lower, display="sites", scaling="symm", add=TRUE)
text(data.rda_lower, dis="cn") 
#points(data.rda_lower, pch=21, col="red", bg="yellow", cex=1.2) - this is if we want the years(sites) plotted as unidentified points
#text(data.rda_lower, "species",  col="blue", cex=0.8) #plots the response data (species)
text(data.rda_lower, "sites", col="red", cex=0.8)
title(xlab="RDA 1 (54.7%)", ylab= "RDA 2 (17.4%)")  #labeled axes with %variance explained. Unfortunately it doesnt look like this is a default in the vegan package so I had to hand calculcate below. 
text(-2.75, 1.5, labels="B")


#NOTES ####
#1. Still not able to add arrows to the species scores (response data). Must figure out how to do that. 
#2. Might be worth combining numbers at age into total numbers... It seems very crowded and its obvious there is little difference in influence among all age specific numbers. 
#3. Recruits and N0 seem identical.. makes sense. I eliminated N0 to visually organize. 
#4. Total mortality at age also seems very crowded. It's not valid to just add them (at least I don't think) so maybe we can pick some benchmark ages and plot those or just chose 1 age. 
#5. In regards to #4, if we condense total mortality at age then we will be able to see SPR_ratio which may be more important to view. 
#6. Bratio and SPB are the same values once they are scaled so I removed Bratio
#7. Seems that SPB and B0 were also same exact values so I removed B0

# Weighted average for Z for adults

respZ <- resp[,14:19] #z for adults
respN <- data[5:26, 33:38]

ZN <- cbind(respZ, respN)

