
#Species diversity

remove(list=ls())
library(tidyverse)
library(vegan)
library(multcomp)
library(ecolTest)
library(dplyr)
library(Matrix)
library(ggpubr)
library(ggpmisc)

#Import mergedctdata datasets
cdat<-read.csv("cameraWestern.csv")

names(cdat)

#Remove unwanted columns
cdat2=subset(cdat,select = -c(start,end,time,latitude,longitude,season,date,time))
cdat2   
unique(cdat2$species)

#Remove cattle to see variation of wild animals between parks
cdat2 <-cdat2 %>%
  filter(!species %in% "Cattle")

#sum replicates together for each site
all2_sum <- cdat2 %>%
  group_by(site,park, species) %>%
  summarise(sumcount = sum(count))

#format data so into wide format and community data matrix - rows = sites, columns = species
all2_sum_wide <- all2_sum %>%
  pivot_wider(names_from=species, values_from = sumcount, values_fill = 0) 

all2_sum_wide_bare <- all2_sum_wide %>%
  column_to_rownames(var="site") %>%
  dplyr::select(-c(park))

#calculate indices
div_sh <- enframe(diversity(all2_sum_wide_bare, "shannon")) %>%
  rename("site" = "name", 
         "diversity_sh" = "value")


div_si <- enframe(diversity(all2_sum_wide_bare, "simpson")) %>%
  rename("site" = "name", 
         "diversity_si" = "value")

div <- left_join(div_sh, div_si, by ="site")

ctdata2 <- left_join(all2_sum_wide %>% 
                       dplyr::select(c("site", "park")),
                       div, 
                       by="site")

#Perform ANOVA test
names(ctdata2)

ctdata2$park<-as.factor(ctdata2$park)

m<-lm(diversity_sh~park, data=ctdata2)
summary(m) #species diversity is the same for all parks
anova(m)

m2<-lm(diversity_si~park, data=ctdata2)
summary(m2) #species richness is the same for all parks
anova(m2)

div_plot <- ctdata2%>%
  group_by(park) %>%
  ggplot(aes(x=park, y=diversity_sh)) +
  geom_boxplot()+
  ylab("Shannon Diversity Index") +
  xlab("")+
  theme_minimal(base_size = 16)+
  theme(legend.position = c(0.9, 0.2),
        legend.key.size = unit(0.5, "cm"), 
        legend.text = element_text(size=9), 
        legend.title = element_text(size=10),
        axis.title = element_text(size=13), 
        axis.text=element_text(size=12))+
  ggtitle("Diversity:F(2,34) = 2.531, p = 0.095")
div_plot

rich_plot <- ctdata2%>%
  group_by(park) %>%
  ggplot(aes(x=park, y=diversity_si)) +
  geom_boxplot()+
  ylab("Simpson Index") +
  xlab("")+
  theme_minimal(base_size = 16)+
  theme(legend.position = c(0.9, 0.2),
        legend.key.size = unit(0.5, "cm"), 
        legend.text = element_text(size=9), 
        legend.title = element_text(size=10),
        axis.title = element_text(size=13), 
        axis.text=element_text(size=12))+ggtitle("Richness:F(2,34) = 1.569, p = 0.223")
rich_plot

#combine both plots

library(gridExtra)
library(grid)

grid.arrange(div_plot, rich_plot, ncol=2)
