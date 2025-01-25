
rm(list = ls()) # Clean R's brain

# First set the working directory
setwd("/Users/Joshua/Library/CloudStorage/OneDrive-UniversityofFlorida/R input")

#Install secr package to be used for this analysis

library(secr)

leopard=read.capthist("Capture.txt","Trap layout secr.txt",
                      detector = "proximity",fmt = "trapID",
                      noccasions = 4,covnames="Sex")

#creating the spatial mask, FIRST, we assign layout a name,


traps=read.traps("Trap layout secr.txt",
                 detector = "proximity")

#second step specifying the buffer width and grid spacing
#note that the home range of clouded leopard is 5.3kmsq, 
#diameter is 2600 meters, buffer is 13,000 obtained by 2600 
#times 5

#add buffer around cameras to be 500 meters
mask1=make.mask(traps,buffer = 13000,spacing=500,
                type = "trapbuffer",poly = NULL)

#third, we plot the mask as well as our traps
plot(mask1)
plot(traps,detpar = list(col="blue"),add = TRUE)

#fitting our models
secr0=secr.fit(leopard,model = g0~1,mask = mask1,CL=TRUE,
               trace = FALSE)

#What is the parameter explained by trap response
secrb=secr.fit(leopard,model = g0~b,mask = mask1,
               CL=TRUE,trace = FALSE)
secrh=secr.fit(leopard,model = g0~h2,mask = mask1,
               CL=TRUE,trace = FALSE)
secrhb=secr.fit(leopard,model = g0~h2*b,mask = mask1,
                CL=TRUE,trace = FALSE)
secrsex=secr.fit(leopard,model = g0~"Sex",mask = mask1,
                 CL=TRUE,trace = FALSE)
secrt=secr.fit(leopard,model = g0~t,mask = mask1,
               CL=TRUE,trace = FALSE)

#looking on outputs of secr0 by running secr0 and 
#derived separately
secr0
derived(secr0)
derived(secrb)
derived(secrsex)

#identify the best model in which the lower the AIC the better
AIC(secr0,secrh,secrhb,secrb,secrsex,secrt)
derived(secrsex)
#density by sex
derived(secrsex,groups = "Sex")

