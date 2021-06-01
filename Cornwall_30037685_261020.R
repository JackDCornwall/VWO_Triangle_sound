#set up code
rm(list=ls()) #clearing crap
setwd("C://Users/Jack/Documents/University/Year One/VWO/VWO assignment 1/Assignment code/") #setting working directory

#importing data
data <- read.csv("spectrum_3.txt", sep="\t")
#data <- read.csv("spectrum_2.txt", sep="\t")

#giving sensible names from columns
colnames(data) <- list("f_hz","dB_raw")

class(data) #checking data frame 
head(data) #checking data has been imported correctly

#required constants
I_0 <- 10e-12 #declaring threshold intensity of hearing
v <- 344 #speed of sound in air at 20 degrees in m/s
rho <- 1.2250 #density of air


#interting decibel values
#data$dB_calc <- data$dB_raw*-1
data$dB_calc <- 0

for(i in (1:length(data$dB_raw))){
  data$dB_calc[i] = 144+data$dB_raw[i]
  
}

#declaring function to calculate intensity from dB
intensity_calc <- function(dB){
  return (I_0*10^(dB/10))
}

#calculatting intensity value
data$intensity_calc <- intensity_calc(data$dB_calc)

#calculating intensity manually to check calculation
#data$intensity_calc2 <- ""
#for (i in (1:length(data$f_hz))){
#  
#  data$intensity_calc2[i] <- intensity_calc(data$dB_calc[i])
#}


#calculating pressure values
data$pressure_calc <- sqrt(2*data$intensity_calc*rho*v)

#normalising factor from dBFS to dBSPL
data$pressure_calc <- data$pressure_calc/40

#calculating pressure manually to check calculation
##data$pressure_calc2 <- ""
#for (i in (1:length(data$f_hz))){
  
#  data$pressure_calc2[i] <- sqrt(2*data$intensity_calc[i]*v)
#}

#calculating period
data$period = 1/data$f_hz

#calculating omega
data$w <- (2*pi) / data$period

#calculating lambda
data$lambda <- v/data$f_hz

#calculating k
data$k <- (2*pi)/data$lambda

#working plotting demo
#t <- seq(0,2*pi,0.0001)
#f <- 10*sin(pi*t)
#f <-f + 5*sin(2*pi*t)
#f <-f + 2.5*sin(3*pi*t)
#plot(t,f)

#creating time sequence for plot
#0.22 used as it is the period of the smallest frequency
#t <- seq(0,0.022,.001)

#f <- 0*t #f will be used to store the cumulative function


####Uncomment which ever graph is needd
#t <- seq(0,2.5,.00001) # entire time 
#t <- seq(0,.5,.0001) # outer envelope frequency
#t <- seq(0,0.1,.0001) #medium detail graph
#t <- seq(0,0.023,.0001) #inner envelope period
t <- seq(0,0.0016,.000001) #high detail graph
#t <- seq(0,0.000115,.000001) #one period


#k term is not included
f <- data$pressure_calc[1] * sin(-1*data$w[1]*t)
 
#looping through and merging all frequencies
for (i in(2:length(data$f_hz))){
  
  f <- f + data$pressure_calc[i] * sin(-1*data$w[i]*t)
}

#plotting curve
plot(t,f,
     main="Triangle sound wave",
     type="l",
     col="black",
     ylab="Pressure [Pa]",
     xlab="Time [s]"
     )
  