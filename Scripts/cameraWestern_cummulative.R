
#Species Cummulative Curve-Burigi-Ibanda-Rumanyika
# Niche Partitioning ---------------------------------------------
# clear everything in memory
rm(list=ls())

# First set the working directory
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/R input")

# Restore environment (if using renv)
renv::restore()

# Load libraries
library(vegan)
library(dplyr)
library(tidyverse)
library(Matrix)

#Import pre&post fencing datasets
ctdata<-read.csv("cameraWestern.csv")

names(ctdata)

#Remove unwanted columns: keep park name, species, date and count
ctdata2=subset(ctdata,select = -c(site,latitude,longitude,start,end,season,time))
ctdata2                                 

#subset and assess each park

#burigi

#remove unwanted parks
ctdat2_burigi <-ctdata2 %>%
  filter(!park %in% c("Ibanda-Kyerwa", "Rumanyika-Karagwe"))

#Make accumulation curve for burigi
acc_burigi   <- ctdat2_burigi %>% 
  dplyr::select(species, date,count) %>% 
  group_by(species,date) %>% 
  summarize(n = sum(count))

#factorize species as it is a categorical variable
acc_burigi$species <- as.factor(acc_burigi$species) #convert to factor

#remove unused species levels
acc_burigi$species <- droplevels(acc_burigi$species) #remove unused species levels

#order by date
acc_burigi <- acc_burigi[order(as.Date(acc_burigi$date,format = "%d/%m/%Y")),] #order by date

#pivot wider
acc_burigi.w <- acc_burigi %>%  pivot_wider(names_from = "species", values_from = "n",values_fill = 0) #pivot wider

#convert to dataframe
acc_burigi.w<- as.data.frame(acc_burigi.w[,2:(ncol(acc_burigi.w))])

#calculate species accumulation curve
acc_all_burigi <- specaccum(acc_burigi.w)
acc_all_burigi

#plot species accumulation curve
plot(acc_all_burigi, col = "blue",ci.type = "poly", ci.col = "grey", ci.lty = 1, ylab = "Number of species",xlab = "Trapping days", main = "", font.sub = 40)

#Ibanda

#remove unwanted parks
ctdat2_ibanda <-ctdata2 %>%
  filter(!park %in% c("Burigi-chato", "Rumanyika-Karagwe"))

#Make accumulation curve for ibanda
acc_ibanda   <- ctdat2_ibanda %>% 
  dplyr::select(species, date,count) %>% 
  group_by(species,date) %>% 
  summarize(n = sum(count))

#factorize species as it is a categorical variable
acc_ibanda$species <- as.factor(acc_ibanda$species)

#remove unused species levels
acc_ibanda$species <- droplevels(acc_ibanda$species)

#order by date
acc_ibanda <- acc_ibanda[order(as.Date(acc_ibanda$date,format = "%d/%m/%Y")),]

#pivot wider
acc_ibanda.w <- acc_ibanda %>%  pivot_wider(names_from = "species", values_from = "n",values_fill = 0)

#convert to dataframe
acc_ibanda.w<- as.data.frame(acc_ibanda.w[,2:(ncol(acc_ibanda.w))])

#calculate species accumulation curve
acc_all_ibanda <- specaccum(acc_ibanda.w)
acc_all_ibanda

#plot species accumulation curve
plot(acc_all_ibanda, col = "blue",ci.type = "poly", ci.col = "grey", ci.lty = 1, ylab = "Number of species",xlab = "Trapping days", main = "", font.sub = 40)

#rumanyika

#remove unwanted parks
ctdat2_rumanyika <-ctdata2 %>%
  filter(!park %in% c("Burigi-chato", "Ibanda-Kyerwa"))

#Make accumulation curve for rumanyika
acc_rumanyika   <- ctdat2_rumanyika %>% 
  dplyr::select(species, date,count) %>% 
  group_by(species,date) %>% 
  summarize(n = sum(count))

#factorize species as it is a categorical variable
acc_rumanyika$species <- as.factor(acc_rumanyika$species)

#remove unused species levels
acc_rumanyika$species <- droplevels(acc_rumanyika$species)

#order by date
acc_rumanyika <- acc_rumanyika[order(as.Date(acc_rumanyika$date,format = "%d/%m/%Y")),]

#pivot wider
acc_rumanyika.w <- acc_rumanyika %>%  pivot_wider(names_from = "species", values_from = "n",values_fill = 0)

#convert to dataframe
acc_rumanyika.w<- as.data.frame(acc_rumanyika.w[,2:(ncol(acc_rumanyika.w))])

#calculate species accumulation curve
acc_all_rumanyika <- specaccum(acc_rumanyika.w)
acc_all_rumanyika

#plot species accumulation curve
plot(acc_all_rumanyika, col = "blue",ci.type = "poly", ci.col = "grey", ci.lty = 1, ylab = "Number of species",xlab = "Trapping days", main = "", font.sub = 40)

#Combine plots

plot(acc_all_burigi, col = "blue",ci.type = "poly", ci.col = "grey", ci.lty = 1, ylab = "Number of species",xlab = "Trapping days", main = "", font.sub = 40)

plot(acc_all_ibanda, add = TRUE, col = "yellow",ci.type = "poly", ci.col = "grey", ci.lty = 1, ylab = "Number of species",xlab = "Trapping days", main = "", font.sub = 4, xlim=c(0,500), ylim=c(0,50))

plot(acc_all_rumanyika, add = TRUE, col = "red",ci.type = "poly", ci.col = "grey", ci.lty = 1, ylab = "Number of species",xlab = "Trapping days", main = "", font.sub = 4, xlim=c(0,500), ylim=c(0,50))

 