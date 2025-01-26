#################################################
### Community Analyses:Richness, Relative Abundance Index, & Capture Rate ###
#################################################
#set working directory

setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/R input")

# Load libraries

# setup -------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggrepel)
library(ggpubr)
library(vegan)
library(plotly)
library(ggpmisc)
library(nicheROVER)


# Part 1 - Diversity Indices ----------------------------------------------
# Relative Abundance Index ------------------------------------------------

# load data 

MTZ18 <- read_csv("MTZ_Jan-Dec2018.csv")
MTZ19 <- read_csv("MTZ_Jan-Dec2019.csv")

# calculate weighted counts for 2018 (relative abundance for 2018=CT_18)
#richness
unique(MTZ18$species)
unique(MTZ19$species)

MTZ_18_calc <- 
  MTZ18 %>% 
  count(species, count) %>% # count the number of individuals per species
  mutate(per = count * n) %>% # calculate the weighted counts
  select(species, per) %>% # select the species and the weighted counts
  group_by(species) %>% # group by species
  tally(per) %>% # calculate the total weighted counts per species
  rename(CT_18 = n) # rename the column


#Checklist for 2018
ggplot(
  data = MTZ_18_calc,
  mapping = aes(x = CT_18, 
                y = species)) +
  geom_col()

# calculate weighted counts for 2019 (relative abundance for 2019=CT_19)
MTZ_19_calc <- 
  MTZ19 %>% 
  count(species, count) %>% 
  mutate(per = count * n) %>% 
  select(species, per) %>% 
  group_by(species) %>% 
  tally(per) %>% 
  rename(CT_19 = n)

#Checklist for 2019
ggplot(
  data =MTZ_19_calc ,
  mapping = aes(x = CT_19, 
                y = species)) +
  geom_col()

# join 2018 and 2019 datasets

CT <- full_join(MTZ_18_calc, MTZ_19_calc,
                by = 'species')


#Let's calculate capture rate for 2018 and 2019
# BUT start calculating search effort: remember definition f capture rate -------------------------------------------------

effort18 <- MTZ18 %>% 
  group_by(site) %>% 
  mutate(first = min(DateTimeOriginal),
         last = max(DateTimeOriginal),
         SE_18 = round(difftime(last, first, units="days"))) %>% 
  slice(1) %>% 
  mutate(effort_18 = as.numeric(SE_18)) %>% 
  select(site, first, last, effort_18) #SE stand for search effort

effort19 <- MTZ19 %>% 
  group_by(site) %>% 
  mutate(first = min(DateTimeOriginal),
         last = max(DateTimeOriginal),
         SE_19 = round(difftime(last, first, units="days"))) %>% 
  slice(1) %>% 
  mutate(effort_19 = as.numeric(SE_19)) %>% 
  select(site, first, last, effort_19)

SE <- rbind(effort18, effort19) 

SE18 <- sum(SE[["effort_18"]], na.rm=TRUE)
SE19 <- sum(SE[["effort_19"]], na.rm=TRUE)

# what was the total search effort for 2018? for 2019?
#for 2018 is 5359
#for 2019 is 5664

# calculate RAI/photographic capture rate

RAI <- CT %>% 
  mutate(RAI_18 = CT_18/SE18,
         RAI_19 = CT_19/SE19,
         ln_RAI_18 = log(RAI_18),
         ln_RAI_19 = log(RAI_19))
RAI

# evaluate consistency between years by graphing in ggplot

RAI_graph <- 
  ggplot(
    data = RAI,
    mapping = aes(x = ln_RAI_18,
                  y = ln_RAI_19)) +
  geom_point(size = 0.75) +
  labs(title = 'Correlation between camera trap images 
  collected in 2018 and 2019',
       x = 'Photographic capture rate 2018 (ln)',
       y = 'Photographic capture rate 2019 (ln)') +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  geom_label_repel(aes(label = species),
                   box.padding = 1,
                   max.overlaps = 15,
                   size = 2.5) +
  stat_cor(method = "pearson") 

# view

RAI_graph
