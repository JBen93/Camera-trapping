# Niche Partitioning ---------------------------------------------

remove(list=ls())
library(tidyverse)
library(vegan)
library(multcomp)
library(ecolTest)
library(dplyr)
library(Matrix)
library(ggpubr)
library(nicheROVER)

#Import mergedctdata datasets
ctdata<-read.csv("cameraWestern.csv")

names(ctdata)

#Remove unwanted columns
ctdata2=subset(ctdata,select = -c(site,start,end,season,date, time,latitude,longitude,season))
ctdata2   
unique(ctdata2$species)

#Select key species include cattle as a measure of illegal grazing

ctdata3 <- ctdata2 %>%
  filter(species %in% c("Cattle", "Zebra", "Buffalo","Topi"))

names(ctdata3)

#sum replicates together for each site
ct_sum <- ctdata3 %>%
  group_by(species) %>%
  na.omit()

ct_sum


# 2-d projections of 4 niche species- bayesian model
nsamples <- 500
herb.par <- tapply(1:nrow(ct_sum), ct_sum$species,
                   function(ii) niw.post(nsamples = nsamples, X = ct_sum[ii,3]))
#assign colors to each species
clrs <- c("black", "red", "blue","orange") # colors for each species

over.stat <- overlap(herb.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot

overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Western Parks Overlap Probability (%) -- Niche Region Size: 95%")


#########################################################################

# analysis for fish data

data(fish) # 4 fish, 3 isotopes
aggregate(fish[2:4], fish[1], mean) # isotope means per fish

# random draws from posterior distribution with default prior
nsamples <- 500
fish.par <- tapply(1:nrow(fish), fish$species,
                   function(ii) niw.post(nsamples = nsamples, X = fish[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("black", "red", "blue", "orange") # colors for each species


over.stat <- overlap(fish.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")

