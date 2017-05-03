# ABOUT ####
# This script loads TimeVarying86.csv and standardizes using the z-score method. Creates RDA ordination plot. 
# Then RDA is performed using the vegan package.
# Authors: Elizabeth Herdter
# For: Ocean Conservancy 
# Date Created: 7/21/16
# See NOTES at bottom. 
# 10/20/16- Combined Numbers at age into juvenile and adult -EH
# 10/27/16- Z0, Z1, Z2, Z3 and Zadult by weighted mean of Z4-Z9
# Also, total numbers were created in the csv file rather than within here because I was accidentally adding scaled numbers in the 10/20 version.


#Load working directory####
setwd("~/Desktop/Amberjack")

#Load pertinent packages####
library(dplyr)
library(vegan)


#Load data and perform z-score standardization ( (x-u)/sd ) so that it has a mean of 0 and a standard dev of 1
data <- data.frame(read.csv("TimeVarying1986.csv", header=TRUE, row.names=1) %>% apply(2,scale)) # %>% mutate(Njuv= N1+N2+N3, Nadult= N4+N5+N6+N7+N8+N9+N10)
# I added Nadult into the .csv because I realized that I shouldnt add scaled numbers as I had done above - EH 10/27/16 


#Make predictor (X) matrix (Catch, effort and discard data)
#Make response (Y) matrix
pred <- data[5:26,1:10] #rows 1990-2011
resp <- data[5:26,c(11:32, 40)] #rows 1990-2011, response columns, N1, N2, N3, Z0, Z1, Z2, Z3, Nadult,Zadult
#missing values in last row for Z and N values
#this way it removes the years that had to be interpolated- starts at 1990 instead
#Mike, once I chose the starting year of 1990 the horseshoe pattern seemed to dissamble a bit. 
#I think it looked that way because the first four yers of the 1986 data set were estimated by just averaging. 
# Starting in 1990 is more valid, I think, because I belive it more accurately depicts the patterns in the data. 

#Assign non-generic row names to years for plotting purposes
name <- c(1990:2011)
rownames(pred) <- name
rownames(resp) <- name


#this has data from 1986+. It contains some interpolated data which I believe causes the unidirectional nature seen in the ordination plot below. 
#pred <- data[1:26,1:10]
#resp <- data[1:26,11:42] #missing values in last row for Z and N values


#Model building- constrained ordination from tutorial ####
data.rda <- rda(resp ~ C_COMHL+C_COMLL+C_REC+C_HB+CPUE_COMLL+CPUE_COMHL+CPUE_MRFSS+CPUE_HB+D_REC+D_HB, data=pred, scale=TRUE)

## Variable selection ####
#https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/vegan-package
mod0 <- rda(resp ~ 1, pred) #No constraints:like a PCA
plot(mod0)
mod1 <- rda(resp ~., pred)
mod1
plot(mod1)
mod <- ordistep(mod0, scope=formula(mod1))
anova(mod)
anova(mod, by="margin")
#Step: resp ~ C_COMHL + C_REC + CPUE_COMLL + D_HB + CPUE_COMHL + C_HB + CPUE_MRFSS  # this seems like the final model 

pred <- pred %>% select(C_COMHL, C_REC, CPUE_COMLL, D_HB, CPUE_COMHL, C_HB, CPUE_MRFSS)
data.rda <- rda(resp ~., pred)

#Plotting example from below:
#http://www.inside-r.org/packages/cran/vegan/docs/ade2vegancca
plot.new()
plot(data.rda, type="n", xlab="", ylab="") 
ordipointlabel(data.rda, display="species", scaling="sites", add=TRUE, col="blue")
#ordipointlabel(data.rda, display="sites", scaling="symm", add=TRUE)
text(data.rda, dis="cn") 
#points(data.rda, pch=21, col="red", bg="yellow", cex=1.2) - this is if we want the years(sites) plotted as unidentified points
#text(data.rda, "species",  col="blue", cex=0.8) #plots the response data (species)
text(data.rda, "sites", col="red", cex=0.8)
title(xlab="RDA 1 (56.6%)", ylab= "RDA 2 (15.9%)")  #labeled axes with %variance explained. Unfortunately it doesnt look like this is a default in the vegan package so I had to hand calculcate below. 

#when type="n" no points. it just sets the frame of the plot
#remove the generic plot labels and then add them at the end- see below
#dis is short for display- when dis="sp" it will plot response variables(species); when its ="si" it will plot the years(sites); when its = "cn" it will plot the predictor vectors


#MUST RE_DO THIS STEP IF REMOVING DATA
#Determine % variance of each axis to put on axis labels
#http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf
# page 19-20
#total constrained variance is 26.3605. Inertia is correlations (or variance)
# So proportion of variance from first axis is = 15.065/26.3605

data.rda
per.var1 <- 10.102/17.8416 #Inertia of axis 1/total
per.var2 <- 2.841/17.8416  #Inertia of axis 2/total


#NOTES: 
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

