# This script is for computing SPI and SPEI for both baseline and predicted climate| Damasco Rubangakene 
#_____________________________________________________________________________
# Follow the instructions below to load the data | compute spi and spei | make plots

require(SPEI)
#1. Load data
setwd("D:\\Courses\\Advanced_quantitative_2017\\4_Hands_on_R\\SPI")

#data(ax)
ax<-read.csv("ax2.csv", header=T)
require(SPEI)
attach(ax)

#2. Four  and twelve-months SPEI
# Compute Potential Evapotranspitaion using hargreaves
#if hargreaves is the interest use= 
ax$PET <- hargreaves(TMIN,TMAX,lat=13.6475)
spei1  <-  spei(ax$PRCP-ax$PET,4)
spei12 <-  spei(ax$PRCP-ax$PET,12)

# Extract information from spei  object
summary(spei4)
names(spei4)
spei1$call
spei1$fitted
spei1$coefficients

# Plot  spei  object
par(mfrow=c(2,1))
plot(spei1)
plot(spei12,'ax')

# 3.  Four and tvelwe-months SPI
dev.new()
spi_4  <-  spi(ax$PRCP,4)
spi_12 <-  spi(ax$PRCP,12)
par(mfrow=c(2,1))
plot(spi_4)
plot(spi_12)
# Define  the  properties of  the  time  series  with  ts()
plot(spei(ts(ax$PRCP-ax$PET,freq=12,start=c(1980,1)),12))
# Time series  not  starting in January
plot(spei(ts(ax$PRCP-ax$PET,freq=12,start=c(1980,6)),12))
# Using a particular reference  period
plot(spei(ts(ax$PRCP-ax$PET,freq=12,start=c(1980,1)),12, ref.start=c(1980,1),  ref.end=c(2000,1)))
# Using different  kernels
spei24<-spei(ax$PRCP-ax$PET,24)
spei24_gau<-spei(ax$PRCP-ax$PET,24,kernel=list(type='gaussian',shift=0))
par(mfrow=c(2,1))
plot(spei24,'Rectangular  kernel')
plot(spei24_gau,'Gaussian  kernel')
# Computing several  time  series  at  a time
data(balance)
names(balance)
bal_spei12  <-  spei(balance,12)
plot(bal_spei12)
# User provided  parameters
coe <-spei1$coefficients
dim(coe)
spei(ax$PRCP-ax$PET,1,params=coe)

#######################################################
######################################################
######################################################

# A rectangular kernel with a time scale of 12 and no shift
kern(12)
# A gaussian kernel with a time scale of 12 and no shift
kern(12,'gaussian')
# Comparison of the four kernels, with and without shift
kern.plot(12)
kern.plot(12,2)






#####___________________________________________________________________________
### FOR PREDICTED CLIMATE 

######SPI and SPEI written by Damasco##########
##########Loading Packege in R##################

require(SPEI)

########Set the working directory in R##########
setwd("C:\\sss\\b_SPEI and SPI\\1_My_data\\Projected\\Atsbi")

############Load data in to R###################
Pa<-read.csv("Ats_et8.5.csv", header=T)

############Check data in to R##################
summary(Pa)
head(Pa)
str(Pa)

#######Attach to recogonise the header##########
attach(Pa)

#####compute 4 months and 12 months SPI#########

(spi_4<-spi(Pa$PRCP,scale=4, start=c(1980:4):12, end=(2010:1),kernel = list(type = 'rectangular', shift = 0), distribution = 'Gamma', fit = 'ub-pwm', na.rm = TRUE, ref.start=NULL, ref.end=NULL, x=TRUE, params=NULL))

(spi_12<-spi(Pa$PRCP,scale=12, start=c(1980:12):12, end=(2010:1),kernel = list(type = 'rectangular', shift = 0), distribution = 'Gamma', fit = 'ub-pwm', na.rm = TRUE, ref.start=NULL, ref.end=NULL, x=TRUE, params=NULL))

###############Plot SPI4 and SPI12#############

par(mfrow=c(2,1))
plot(spi_4,'Rectangular kernel')
plot(spi_12,'Rectangular kernel')
 
###########################################SPEI COMPUTATION###########################################################

#########compute evapotranspiration using Hargreav's: for other methods refer the original script#######

########################Edit data and calculate the ETo######################

Pa$PET <-hargreaves(TMIN,TMAX,lat=13.880)
print(hargreaves(TMIN,TMAX,lat=13.880))

#############################SPEI4 and SPEI12################################
spei4 <- spei(Pa$PRCP-Pa$PET,4)
spei12 <-spei(Pa$PRCP-Pa$PET,12)

# ####################Extract information from spei object###################
summary(spei4)
names(spei4)
spei4$call
spei4$fitted
spei4$coefficients

summary(spei12)
names(spei12)
spei12$call
spei12$fitted
spei12$coefficients

##########Plot SPEI4 and SPEI 12###############################################

par(mfrow=c(2,1))
plot(spei4,'Rectangular kernel')
plot(spei12,'Rectangular kernel')

