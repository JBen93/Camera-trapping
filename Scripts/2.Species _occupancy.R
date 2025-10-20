# clear everything in memory
rm(list=ls())

# First set the working directory
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/R input")
renv::restore()
# Load libraries
library(readr)
library(unmarked)
library(tidyverse) 
library(raster)
library(RColorBrewer)
library(sf)
library(terra)
library(PerformanceAnalytics)

# Bring original files occ model 

#bring original values, not scaled, better do it here, means and sd will be needed

#setwd


covars  <- read.csv("Covariates.csv")
p_occ <- read.csv("Western_Occupancy.csv")


#Let's perform correlation test for our covariates
library(PerformanceAnalytics)
names(covars) #check the names of the columns
cortab<-covars[,c(4:13)] #select the columns to be correlated
chart.Correlation(cortab, histogram = TRUE, pch=19,base=20) #plot the correlation matrix

#covariates table with less strong correlated variables
cortab2<-covars[,c(4:11)] #select the columns to be correlated

names(covars)

covars2<-covars[,1:11] #select the columns to used in the model

# Run best model

# Create unmarkedFrame

y <- p_occ[,-1] #remove the first column

umf = unmarkedFrameOccu(y = y, 
                        siteCovs = covars2) # siteCovs
head(umf) 
tail(umf)

# model
psi_tree <-occu(~ Road+Park_km+Treecover+Settlement #Detection
                ~ BurnFreq+Rainfall+Elevation+
                  Treecover+Water_km+Settlement, umf) #Occupancy

psi_tree

psi_tree2 <-occu(~ Settlement
                ~ Rainfall, umf) 

psi_tree2 #best model

# Spatial prediction

#Now we can ring file with values to predict upon we can use a raster file, or a data #frame with grid of points with thecorresponding covariate values. with x.,y and values of #covariates (basically the covariates) siteCovs

#because of the scale of our data and purpose of this example, we are going to use the map of western parks of Tanzania

## Using a rainfall raster map to predict occupancy


# resolve namespace conflicts
select <- dplyr::select #select function from dplyr
projection <- raster::projection #projection function from raster

#data
tree.r <- raster("RainWestern.tif")
tree.r


# project
tree.r.prj3 <- projectRaster(tree.r,
                             crs = crs("+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs")) #WGS 84 / UTM zone 36S


# check NA
scale_fill_viridis_c(na.value = 'deeppink') #check the NA values

#double check here, I might need to set the proj! *****
res(tree.r.prj3) # ok
crs(tree.r.prj3)  #conserve the crs

plot(tree.r.prj3) 


# Agregate and tidy up
#we can aggregate that to have a coarser scale, maybe four times to be close to 2km so it #is similar to the grid size?


#e <- extent(150000,400000,9700000,9900000) #


tree.fin2 <- tree.r.prj3
plot(tree.fin2, axes = TRUE) #
res(tree.fin2)
crs(tree.fin2) #


# predict

class(tree.fin2) 
ef <- stack(tree.fin2)            # need to transform the scaled map into a rasterstack
names(ef) <- "Rainfall"  # the raster vars must be named,
crs(ef)#ok

#this can take some time!
E.psi <- predict(psi_tree2, type="state", newdata= ef) #rasterstack object

#need to project the predictions too?
crs(E.psi) <- tree.r.prj3 
crs(E.psi)

raster_plot <- plot(E.psi, axes= FALSE, col=rev(terrain.colors(50))) # working fine! 4 graphs (SE...)

class(raster_plot)
crs(E.psi) 
E.psi@layers ##because the map is in this format we can see the layers stacked
#[[1]] is the predicted so this is the one we need to put the reserves on top

#save it as raster
#writeRaster(E.psi, "data_out/pred_raster_proj_1.tif", overwrite = TRUE) #



# Add reserves 
library(terra)
library(sp)
library(sf)

reserves.shp = read_sf("Western.shp")#SpatialPolygonsDataFrame

reserves.shp.trans <- st_transform(reserves.shp,
                                  CRSobj = crs(
                                    tree.fin2))
plot(reserves.shp.trans)

plot(E.psi[[1]])
plot(reserves.shp.trans, add=TRUE)


# If you prefer ggplot you can use

#we need to convert to a data frame first.
E.psi_df  <- as.data.frame(E.psi, xy = TRUE) #we had 475,475 rows, without NA now we have 229,739

str(E.psi_df)

library(ggplot2)
pplot1 <- ggplot() +
  geom_histogram(data = E.psi_df, aes(Predicted))
pplot1

#raster plot of prediction

plot2a <- ggplot() +
  geom_raster(data = E.psi_df,
              aes(x = x, y = y, fill = Predicted)) +
  scale_fill_gradientn(name = "Occ Pred", colors = rev(terrain.colors(100))) +
  labs(x = "", y = "") +
  coord_equal() +theme_bw()
  
plot2a


ggsave("occ_Rainfall_TRCO_10-07-2023.tiff")

