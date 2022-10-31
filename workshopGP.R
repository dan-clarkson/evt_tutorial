# Data --------------------------------------------------------------------

summitA <- read.csv("summit_daily_max.csv")

# Packages ----------------------------------------------------------------

packages = c("lubridate","ggplot2","extRemes")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

library("lubridate")
library("ggplot2")
library("extRemes")

# Processing data ---------------------------------------------------------

extract.dates <- function(time){
    yr <- floor(time)
    day <- time-yr
    doy <- c()
    #only century year is a leap year
    doy[!is.na(yr) & yr%%4==0] <- ceiling(day[!is.na(yr) & yr%%4==0]*366)
    doy[!is.na(yr) & yr%%4!=0] <- ceiling(day[!is.na(yr) & yr%%4!=0]*365)
    return(list("year"=yr,"doy"=doy,"date"=parse_date_time(paste(yr,doy),"Yj")))
}

#Extract summit

summitA <- summitA[-which(is.na(summitA$Temperature)),]
summitA <- summitA[,-1]

summit.doy <- extract.dates(summitA$Date)
summit <- as.data.frame(cbind(as_date(summit.doy$date),summitA[,2]))
summit[,1] <- as_date(summit[,1])
names(summit) <- c("Date","Temperature")

nrow(summit)

plot(summit$Date,summit$Temperature,xlab="Date",ylab="Temperature")

ggplot(data=summit)+geom_point(aes(x=Date,y=Temperature))

acf(summit$Temperature,lag.max=365,na.action=na.pass)
pacf(summit$Temperature,lag.max=365,na.action=na.pass)

summitNoNa <- na.omit(summit)
u <- quantile(summitNoNa$Temperature,0.95)
summitEx <- summitNoNa[summitNoNa$Temperature>u,]

plot(summitEx$Date,summitEx$Temperature)

ggplot(data=summitEx)+geom_point(aes(x=Date,y=Temperature))

# Fitting GPD -------------------------------------------------------------

pot.fit.1 <- fevd(Temperature,data=summitNoNa,threshold=u,type="GP")
summary(pot.fit.1)
plot(pot.fit.1)

plot(pot.fit.1,type="rl",rperiods=seq(2,1000,by=1))

#Could we decluster to improve the fit? 