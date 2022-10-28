# Data --------------------------------------------------------------------

arctic <- read.csv("N_seaice_extent_daily_v2.1.csv")
#Obtained from https://nsidc.org/data/seaice_index/archives.html

# Packages ----------------------------------------------------------------

library(lubridate)
library(ggplot2)
library(ggfortify)
library(extRemes)

# Processing data ---------------------------------------------------------

arctic$Date <- dmy(paste(arctic$Day,arctic$Month,arctic$Year,sep="-")) #converts dates into a nicer format

dev.new();dev.new() #opens a graphics device in a seperate window

ggplot(data=arctic)+geom_point(aes(x=Date,y=Extent)) #plots data over time

acf(arctic$Extent,lag.max=365) #plots autocorrelation function at different lags 

#Create a matrix with two columns: years and annual minima
range(arctic$Year) #what years do we have in the data?
arcticMin <- matrix(0,nrow=length(unique(arctic$Year)),ncol=2) #matrix for storing information
arcticMin[,1] <- c(1978:2017) #years
for(i in 1978:2017){
    arcticMin[i-1977,2] <- min(arctic$Extent[arctic$Year==i],na.rm=T) #minimum extent for each year extent
}
#Convert to data frame structure
arcticMin <- as.data.frame(arcticMin)
names(arcticMin) <- c("Year","Extent")

ggplot(data=arcticMin)+geom_point(aes(x=Year,y=Extent)) #not sensible to include first or last years - since we don't have complete data for these years

d <- nrow(arcticMin)
arcticMin <- arcticMin[-c(1,d),] #remove first and last years

ggplot(data=arcticMin)+geom_point(aes(x=Year,y=Extent)) #much better

# Fitting GEV -------------------------------------------------------------

#need to model minus the minima
arcticMin[,2] <- -arcticMin[,2]

max.fit.1 <- fevd(Extent,data=arcticMin,type="GEV")
summary(max.fit.1)
plot(max.fit.1) #diagnostics -- density plot not good

#Return levels
-return.level(max.fit.1,c(5,50,100)) #minus these values to get back to the original scale
#or
plot(max.fit.1,type="rl",rperiods=seq(2,1000,by=1))

#To plot on the 'original scale'
R <- return.level(max.fit.1,seq(2,1000,by=1),do.ci=T)

max.rl <- matrix(0,nrow=999,ncol=4)
max.rl[,1] <- seq(2,1000,by=1)
max.rl[,2] <- -R[,1] #lower confidence interval
max.rl[,3] <- -R[,2] #point estimates
max.rl[,4] <- -R[,3] #upper confidence interval

max.rl[,1] <- -log(-log(1-1/max.rl[,1]))

max.rl <- as.data.frame(max.rl)
names(max.rl) <- c("ReturnPeriod","ReturnLevel","LowerCI","UpperCI")
ggplot(data=max.rl)+geom_line(aes(x=ReturnPeriod,y=ReturnLevel))+geom_line(aes(x=ReturnPeriod,y=LowerCI))+geom_line(aes(x=ReturnPeriod,y=UpperCI))+labs(x="Return Period",y="Return level (degrees C)")+scale_x_continuous(breaks=log(c(5,10,25,50,100,250,500,1000)),labels=c("5","10","25","50","100","250","500","1000"))
#this plots both return level estimates, and their associated uncertainty

# Fitting GEV with a covariates --------------------------------------------

#Scale the covariate - this avoids issues when fitting the GEV
arcticMin$ScaledYear <- arcticMin$Year-1978

max.fit.2 <- fevd(Extent,data=arcticMin,type="GEV",location.fun=~1+ScaledYear) #linear trend in location parameter

summary(max.fit.2)
lr.test(max.fit.1,max.fit.2) #linear trend in location is significant

max.fit.3 <- fevd(Extent,data=arcticMin,type="GEV",location.fun=~1+ScaledYear,scale.fun=~1+ScaledYear)
lr.test(max.fit.2,max.fit.3) #additional linear trend in scale is not significant

#Visual diagnostics
plot(max.fit.2) #too much for one plot - but better density fit

par(mfrow=c(1,1))
plot(max.fit.2,type="rl") #can see trend in return level values 

#Need to negate to get back to the original scale
rl.2 <- erlevd(max.fit.2,period=c(2,20,100))

rl.2.neg <- -rl.2
period <- c(rep(2,38),rep(20,38),rep(100,38))
rl.final <- cbind(rep(arcticMin$Year,3),as.vector(t(rl.2.neg)),period)
rl.final <- as.data.frame(rl.final)
rl.final[,3] <- as.factor(rl.final[,3])
levels(rl.final[,3]) <- c("2yr","20yr","100yr")
names(rl.final) <- c("Year","RL","RP")

ggplot()+geom_line(data=rl.final,aes(x=Year,y=RL,color=RP))+geom_line(aes(x=Year,y=-Extent),data=arcticMin) #plotted trends in return levels, appear in good agreement with what the data suggests 
