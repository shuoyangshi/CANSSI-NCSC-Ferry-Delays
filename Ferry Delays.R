library(stringr)
library(zoo)
library(dplyr)
library(tidyr)
library(caret)
library(mltools)
library(data.table)
library(pbapply)
library(parallel)

train = read.csv("train.csv",as.is=TRUE)
test = read.csv("test.csv", as.is=TRUE)
vancouver = read.csv("vancouver.csv",as.is=TRUE,na.strings = c("","NA"))
victoria = read.csv("victoria.csv",as.is=TRUE,na.string = c("","NA"))
traffic = read.csv("traffic.csv",as.is=TRUE)
names(train)
dim(train)

#################################################################

unique(train[which(is.na(train$Trip.Duration)),]$Trip)
unique(train[which(!is.na(train$Trip.Duration)),]$Trip)
unique(train[train$Trip == "Swartz Bay to Fulford Harbour (Saltspring Is.)",]$Trip.Duration)
which(!is.na(train[train$Trip == "Swartz Bay to Fulford Harbour (Saltspring Is.)",]$Trip.Duration))

dim(train)
unique(train$Trip)

# Add Trip/Duration indicator since Tsawwasen and Swartz Bay to Southern Gulf Islands' Durations are missing completely
train$Trip.Ind[train$Trip == "Tsawwassen to Swartz Bay"] = "1"
train$Trip.Ind[train$Trip == "Swartz Bay to Tsawwassen"] = "2"
train$Trip.Ind[train$Trip == "Tsawwassen to Duke Point"] = "3"
train$Trip.Ind[train$Trip == "Duke Point to Tsawwassen"] = "4"
train$Trip.Ind[train$Trip == "Horseshoe Bay to Snug Cove (Bowen Is.)"] = "5"
train$Trip.Ind[train$Trip == "Horseshoe Bay to Langdale"] = "6"
train$Trip.Ind[train$Trip == "Langdale to Horseshoe Bay"] = "7"
train$Trip.Ind[train$Trip == "Swartz Bay to Fulford Harbour (Saltspring Is.)"] = "8"
train$Trip.Ind[train$Trip == "Departure Bay to Horseshoe Bay"] = "9"
train$Trip.Ind[train$Trip == "Horseshoe Bay to Departure Bay"] = "10"
train$Trip.Ind[train$Trip == "Swartz Bay to Southern Gulf Islands"] = "11"
train$Trip.Ind[train$Trip == "Tsawwassen to Southern Gulf Islands"] = "12"

train[train$Trip == "Swartz Bay to Southern Gulf Islands",]$Trip.Duration = "70"
train[train$Trip == "Tsawwassen to Southern Gulf Islands",]$Trip.Duration = "55"

# Converting time and date to standard form  
month_to_num <- function(x){
  x <- as.character(x)
  f1 <- function(z) {switch (z,
                             January = 1,
                             February = 2,
                             March = 3,
                             April = 4,
                             May = 5,
                             June = 6,
                             July = 7,
                             August = 8,
                             September = 9,
                             October = 10,
                             November = 11,
                             December = 12)
  }
  res = sapply(x, f1)
  return(res)
}

week_to_num <- function(x){
  x <- as.character(x)
  f1 <- function(z) {
    switch(z, 
           Sunday = 1,
           Monday = 2,
           Tuesday = 3,
           Wednesday = 4,
           Thursday = 5,
           Friday = 6,
           Saturday = 7)
  }
  res = sapply(x,f1)
  return(res)
}

time_am_to_24 <- function(x){
  n = length(x)
  x <- as.character(x)
  x <- str_split(x,'[:| ]')
  x <- matrix(unlist(x),nrow=n,ncol=3,byrow = T)
  res = as.numeric(x[,1]) + as.numeric(x[,2])/60
  res = res + (x[,3] == "PM")*12
  res[res >= 24] = res[res >= 24] - 12
  res
}

train$Dep.Time = time_am_to_24(train$Scheduled.Departure)
train$Month <- month_to_num(train$Month)
train$Date <- paste(train$Year,train$Month,train$Day.of.Month,sep = '-')
train$Day <- week_to_num(train$Day)

test$Month <- month_to_num(test$Month)
test$Date <- paste(test$Year,test$Month,test$Day.of.Month,sep = '-')
test$Dep.Time = time_am_to_24(test$Scheduled.Departure)
test$Day <- week_to_num(test$Day)

traffic$Date <- paste(traffic$Year,traffic$Month,traffic$Day,sep = '-')
traffic$Traf.Time <- paste(traffic$Hour + traffic$Minute/60 + traffic$Second/3600)
vancouver$Date <- paste(vancouver$Year, vancouver$Month, vancouver$Day,sep = '-')
victoria$Date <- paste(victoria$Year, victoria$Month, victoria$Day,sep = '-')

# Fill in Victoria
#which(is.na(victoria$Weather))
victoria[1:7,]$Weather = "Clear"
victoria = victoria %>% fill(Weather)
#victoria = victoria %>% mutate(victoria$Weather <- na.locf(victoria$Weather))
unique(victoria$Weather)
victoria <- transform(victoria,id=as.numeric(factor(victoria$Weather)))
colnames(victoria)[colnames(victoria)=="id"] <- "Weather.Ind"
victoria[1:5,]$Temperature.in.Celsius = 10
victoria[14590:14592,]$Temperature.in.Celsius = 7
victoria$Temperature.in.Celsius = na.approx(victoria$Temperature.in.Celsius)

victoria[1:5,]$Dew.Point.Temperature.in.Celsius = 11.1
victoria[14590:14592,]$Dew.Point.Temperature.in.Celsius = 4.0
victoria$Dew.Point.Temperature.in.Celsius = na.approx(victoria$Dew.Point.Temperature.in.Celsius)

victoria[1:5,]$Relative.Humidity.in.Percent = 91
victoria[14590:14592,]$Relative.Humidity.in.Percent = 71
victoria$Relative.Humidity.in.Percent = na.approx(victoria$Relative.Humidity.in.Percent)

victoria[1:5,]$Wind.Direction.in.Degrees = 330
victoria[14590:14592,]$Wind.Direction.in.Degrees = 260
victoria$Wind.Direction.in.Degrees = na.approx(victoria$Wind.Direction.in.Degrees)

victoria[1:5,]$Wind.Speed.km.per.h = 1
victoria[14590:14592,]$Wind.Speed.km.per.h = 11
victoria$Wind.Speed.km.per.h = na.approx(victoria$Wind.Speed.km.per.h)
  
victoria[1:5,]$Visibility.in.km = 24.1
victoria[14590:14592,]$Visibility.in.km = 24.1
victoria$Visibility.in.km = na.approx(victoria$Visibility.in.km)
  
victoria[1:5,]$Station.Pressure.in.kPa = 101.81
victoria[14590:14592,]$Station.Pressure.in.kPa = 101.96
victoria$Station.Pressure.in.kPa = na.approx(victoria$Station.Pressure.in.kPa)  

# Fill in Vancouver
which(is.na(vancouver$Dew.Point.Temperature.in.Celsius))

vancouver$Temperature.in.Celsius = na.approx(vancouver$Temperature.in.Celsius)  
vancouver$Dew.Point.Temperature.in.Celsius = na.approx(vancouver$Dew.Point.Temperature.in.Celsius)
vancouver$Relative.Humidity.in.Percent = na.approx(vancouver$Relative.Humidity.in.Percent)

which(!is.na(vancouver$Humidex.in.Celsius))

vancouver[1:11,]$Humidex.in.Celsius = 25
vancouver[10169:14592,]$Humidex.in.Celsius = 25
vancouver$Humidex.in.Celsius = na.approx(vancouver$Humidex.in.Celsius)

which(is.na(vancouver))

dim(vancouver)
dim(train)

train$Est.Time = round(train$Dep.Time)
train[which(train$Est.Time == 24),]$Est.Time = 0

traffic$Date <- paste(traffic$Year,traffic$Month,traffic$Day,sep = '-')
traffic$time <- traffic$Hour + traffic$Minute/60 + traffic$Second/3600

traffic[traffic$Date == train$Date[1],]
temp <- traffic[traffic$Date == train$Date[1],]
temp[which.min(temp$time - traffic$time[1]),]

for (i in 1:length(train$Date)){
  temp <- traffic[traffic$Date == train$Date[i],]
  temp[which.min(temp$Traf.Time - train$Dep.Time[i]),]
}

traffic$Date <- paste(traffic$Year,traffic$Month,traffic$Day,sep = '-')
traffic$time <- traffic$Hour + traffic$Minute/60 + traffic$Second/3600
temp <- traffic[traffic$Date == train$Date[1],]
temp[which.min(temp$time - train$Scheduled.Departure[1]),]

traffic$Date <- paste(traffic$Year,traffic$Month,traffic$Day,sep = '-')
traffic$time <- traffic$Hour + traffic$Minute/60 + traffic$Second/3600
temp <- traffic[traffic$Date == train$Date[1],]
temp1 <- temp[temp$Time > train$Scheduled.Departure[1],]
temp2 <- temp[temp$Time < train$Scheduled.Departure[1],]
temp2[which.max(temp2$Time - train$Scheduled.Departure[1]),]
temp1[which.min(temp1$time - train$Scheduled.Departure[1]),]


train$last_time_traffic <- NA
train$next_time_traffic <- NA

for (i in 1:nrow(train)) {
  last_time_traffic <- NA
  next_time_traffic <- NA 
  temp <- traffic[traffic$Date == train$Date[i],]
  temp1 <- temp[temp$Time >= train$Dep.Time[i],]
  temp2 <- temp[temp$Time <= train$Dep.Time[i],]
  temp2[which.max(temp2$Time - train$Dep.Time[i]),]
  temp1[which.min(temp1$Time - train$Dep.Time[i]),]
  last_time_traffic <- temp1$Traffic.Ordinal
  next_time_traffic <- temp2$Traffic.Ordinal
  train$last_time_traffic[i] <- last_time_traffic
  train$next_time_traffic[i] <- next_time_traffic
  }

head(train)

train$last_time_traffic <- NA
train$next_time_traffic <- NA

for (i in 1:nrow(train)) {
  last_time_traffic <- NA
  next_time_traffic <- NA 
  temp <- traffic[traffic$Date == train$Date[i],]
  temp1 <- temp[temp$Time >= train$Dep.Time[i],]
  temp2 <- temp[temp$Time <= train$Dep.Time[i],]
  temp2[which.max(temp2$Time - train$Dep.Time[i]),]
  temp1[which.min(temp1$Time - train$Dep.Time[i]),]
  if(nrow(temp1) > 0){
    last_time_traffic <- temp1$Traffic.Ordinal
    }
  if(nrow(temp2) > 0){
    next_time_traffic <- temp2$Traffic.Ordinal
  }
  train$last_time_traffic[i] <- last_time_traffic
  train$next_time_traffic[i] <- next_time_traffic
  print(i)
  }

traffic$Traf.Time = as.numeric(traffic$Traf.Time)

##################################
# Traffic to train

train$last_time_traffic <- NA
train$next_time_traffic <- NA

ff <- function(x){
  last_time_traffic <- NA
  next_time_traffic <- NA
  temp <- traffic[traffic$Date == as.character(x[14]),]
  temp1 <- temp[as.numeric(temp$Traf.Time) >= as.numeric(x[13]),]
  temp2 <- temp[as.numeric(temp$Traf.Time) <= as.numeric(x[13]),]
  
  if(nrow(temp1) > 0){
    last_time_traffic <-temp1[which.min(as.numeric(temp1$Traf.Time) - as.numeric(x[13])),]$Traffic.Ordinal
  }
  if(nrow(temp2) > 0){
    next_time_traffic <- temp2[which.max(as.numeric(temp2$Traf.Time) - as.numeric(x[13])),]$Traffic.Ordinal
  }
  return(c(last_time_traffic,next_time_traffic))
}

cl = makeCluster(detectCores())
clusterExport(cl,ls())
res = pbapply(train, 1,ff,cl=cl)
res
stopCluster(cl)

res=t(res)
train$last_time_traffic = res[,1]
train$next_time_traffic = res[,2]
##################################
# Vancouver to train
train$van_temperature_last <- NA
train$van_temperature_next <- NA
train$van_dew_point_temperature_last <- NA
train$van_dew_point_temperature_next <- NA
train$van_humidity_last <- NA
train$van_humidity_next <- NA
train$van_humidex_last <- NA
train$van_humidex_next <- NA

van <- function(x){
  van_temperature_last <- NA
  van_dew_point_temperature_last <- NA
  van_humidity_last <- NA
  van_humidex_last <- NA
  van_temperature_next <- NA
  van_dew_point_temperature_next <- NA
  van_humidity_next <- NA
  van_humidex_next <- NA
  temp <- vancouver[vancouver$Date == as.character(x[14]),]
  temp1 <- temp[as.numeric(temp$Hour) >= as.numeric(x[13]),]
  temp2 <- temp[as.numeric(temp$Hour) <= as.numeric(x[13]),]
  
  if(nrow(temp1) > 0){
    van_temperature_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[13])),]$Temperature.in.Celsius
    van_dew_point_temperature_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[13])),]$Dew.Point.Temperature.in.Celsius
    van_humidity_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[13])),]$Relative.Humidity.in.Percent
    van_humidex_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[13])),]$Humidex.in.Celsius
  }
  if(nrow(temp2) > 0){
    van_temperature_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[13])),]$Temperature.in.Celsius
    van_dew_point_temperature_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[13])),]$Dew.Point.Temperature.in.Celsius
    van_humidity_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[13])),]$Relative.Humidity.in.Percent
    van_humidex_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[13])),]$Humidex.in.Celsius
  }
  return(c(van_temperature_last,van_temperature_next,van_dew_point_temperature_last,van_dew_point_temperature_next,
           van_humidity_last,van_humidity_next,van_humidex_last,van_humidex_next))
}

cl = makeCluster(detectCores())
clusterExport(cl,ls())
res = pbapply(train, 1,van,cl=cl)
res
stopCluster(cl)
res=t(res)
train$van_temperature_last <- res[,1]
train$van_temperature_next <- res[,2]
train$van_dew_point_temperature_last <- res[,3]
train$van_dew_point_temperature_next <- res[,4]
train$van_humidity_last <- res[,5]
train$van_humidity_next <- res[,6]
train$van_humidex_last <- res[,7]
train$van_humidex_next <- res[,8]

############################
# Victoria to train
train$vic_temperature_last <- NA
train$vic_temperature_next <- NA
train$vic_dew_point_temperature_last <- NA
train$vic_dew_point_temperature_next <- NA
train$vic_humidity_last <- NA
train$vic_humidity_next <- NA
train$vic_wind_direction_last <- NA
train$vic_wind_direction_next <- NA
train$vic_wind_speed_last <- NA
train$vic_wind_speed_next <- NA
train$vic_visibility_last <- NA
train$vic_visibility_next <- NA
train$vic_pressure_last <- NA
train$vic_pressure_next <- NA
train$vic_weather_last <- NA
train$vic_weather_next <- NA

vic <- function(x){
  vic_temperature_last <- NA
  vic_temperature_next <- NA
  vic_dew_point_temperature_last <- NA
  vic_dew_point_temperature_next <- NA
  vic_humidity_last <- NA
  vic_humidity_next <- NA
  vic_wind_direction_last <- NA
  vic_wind_direction_next <- NA
  vic_wind_speed_last <- NA
  vic_wind_speed_next <- NA
  vic_visibility_last <- NA
  vic_visibility_next <- NA
  vic_pressure_last <- NA
  vic_pressure_next <- NA
  vic_weather_last <- NA
  vic_weather_next <- NA
  
  temp <- victoria[victoria$Date == as.character(x[14]),]
  temp1 <- temp[as.numeric(temp$Hour) >= as.numeric(x[13]),]
  temp2 <- temp[as.numeric(temp$Hour) <= as.numeric(x[13]),]
  
  if(nrow(temp1) > 0){
    vic_temperature_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[13])),]$Temperature.in.Celsius
    vic_dew_point_temperature_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[13])),]$Dew.Point.Temperature.in.Celsius
    vic_humidity_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[13])),]$Relative.Humidity.in.Percent
    vic_wind_direction_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[13])),]$Wind.Direction.in.Degrees
    vic_wind_speed_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[13])),]$Wind.Speed.km.per.h
    vic_visibility_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[13])),]$Visibility.in.km
    vic_pressure_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[13])),]$Station.Pressure.in.kPa
    vic_weather_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[13])),]$Weather.Ind

    
  }
  if(nrow(temp2) > 0){
    vic_temperature_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[13])),]$Temperature.in.Celsius
    vic_dew_point_temperature_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[13])),]$Dew.Point.Temperature.in.Celsius
    vic_humidity_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[13])),]$Relative.Humidity.in.Percent
    vic_wind_direction_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[13])),]$Wind.Direction.in.Degrees
    vic_wind_speed_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[13])),]$Wind.Speed.km.per.h
    vic_visibility_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[13])),]$Visibility.in.km
    vic_pressure_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[13])),]$Station.Pressure.in.kPa
    vic_weather_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[13])),]$Weather.Ind
  }
  return(c(vic_temperature_last,vic_temperature_next,vic_dew_point_temperature_last,vic_dew_point_temperature_next,
           vic_humidity_last,vic_humidity_next,vic_wind_direction_last,vic_wind_direction_next,
           vic_wind_speed_last,vic_wind_speed_next,vic_visibility_last,vic_visibility_next,
           vic_pressure_last,vic_pressure_next,vic_weather_last,vic_weather_next))
}

cl = makeCluster(detectCores())
clusterExport(cl,ls())
res = pbapply(train, 1,vic,cl=cl)
res
stopCluster(cl)
res=t(res)
train$vic_temperature_last <- res[,1]
train$vic_temperature_next <- res[,2]
train$vic_dew_point_temperature_last <- res[,3]
train$vic_dew_point_temperature_next <- res[,4]
train$vic_humidity_last <- res[,5]
train$vic_humidity_next <- res[,6]
train$vic_wind_direction_last <- res[,7]
train$vic_wind_direction_next <- res[,8]
train$vic_wind_speed_last <- res[,9]
train$vic_wind_speed_next <- res[,10]
train$vic_visibility_last <- res[,11]
train$vic_visibility_next <- res[,12]
train$vic_pressure_last <- res[,13]
train$vic_pressure_next <- res[,14]
train$vic_weather_last <- res[,15]
train$vic_weather_next <- res[,16]
############################
# Traffic to test

test$last_time_traffic <- NA
test$next_time_traffic <- NA

ff <- function(x){
  last_time_traffic <- NA
  next_time_traffic <- NA
  temp <- traffic[traffic$Date == as.character(x[10]),]
  temp1 <- temp[as.numeric(temp$Traf.Time) >= as.numeric(x[11]),]
  temp2 <- temp[as.numeric(temp$Traf.Time) <= as.numeric(x[11]),]
  
  if(nrow(temp1) > 0){
    last_time_traffic <-temp1[which.min(as.numeric(temp1$Traf.Time) - as.numeric(x[11])),]$Traffic.Ordinal
  }
  if(nrow(temp2) > 0){
    next_time_traffic <- temp2[which.max(as.numeric(temp2$Traf.Time) - as.numeric(x[11])),]$Traffic.Ordinal
  }
  return(c(last_time_traffic,next_time_traffic))
}

cl = makeCluster(detectCores())
clusterExport(cl,ls())
res = pbapply(test, 1,ff,cl=cl)
res
stopCluster(cl)

res=t(res)
test$last_time_traffic = res[,1]
test$next_time_traffic = res[,2]
##################################
# Vancouver to test
test$van_temperature_last <- NA
test$van_temperature_next <- NA
test$van_dew_point_temperature_last <- NA
test$van_dew_point_temperature_next <- NA
test$van_humidity_last <- NA
test$van_humidity_next <- NA
test$van_humidex_last <- NA
test$van_humidex_next <- NA

van <- function(x){
  van_temperature_last <- NA
  van_dew_point_temperature_last <- NA
  van_humidity_last <- NA
  van_humidex_last <- NA
  van_temperature_next <- NA
  van_dew_point_temperature_next <- NA
  van_humidity_next <- NA
  van_humidex_next <- NA
  temp <- vancouver[vancouver$Date == as.character(x[10]),]
  temp1 <- temp[as.numeric(temp$Hour) >= as.numeric(x[11]),]
  temp2 <- temp[as.numeric(temp$Hour) <= as.numeric(x[11]),]
  
  if(nrow(temp1) > 0){
    van_temperature_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[11])),]$Temperature.in.Celsius
    van_dew_point_temperature_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[11])),]$Dew.Point.Temperature.in.Celsius
    van_humidity_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[11])),]$Relative.Humidity.in.Percent
    van_humidex_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[11])),]$Humidex.in.Celsius
  }
  if(nrow(temp2) > 0){
    van_temperature_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[11])),]$Temperature.in.Celsius
    van_dew_point_temperature_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[11])),]$Dew.Point.Temperature.in.Celsius
    van_humidity_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[11])),]$Relative.Humidity.in.Percent
    van_humidex_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[11])),]$Humidex.in.Celsius
  }
  return(c(van_temperature_last,van_temperature_next,van_dew_point_temperature_last,van_dew_point_temperature_next,
           van_humidity_last,van_humidity_next,van_humidex_last,van_humidex_next))
}

cl = makeCluster(detectCores())
clusterExport(cl,ls())
res = pbapply(test, 1,van,cl=cl)
res
stopCluster(cl)
res=t(res)
test$van_temperature_last <- res[,1]
test$van_temperature_next <- res[,2]
test$van_dew_point_temperature_last <- res[,3]
test$van_dew_point_temperature_next <- res[,4]
test$van_humidity_last <- res[,5]
test$van_humidity_next <- res[,6]
test$van_humidex_last <- res[,7]
test$van_humidex_next <- res[,8]

############################
# Victoria to test
test$vic_temperature_last <- NA
test$vic_temperature_next <- NA
test$vic_dew_point_temperature_last <- NA
test$vic_dew_point_temperature_next <- NA
test$vic_humidity_last <- NA
test$vic_humidity_next <- NA
test$vic_wind_direction_last <- NA
test$vic_wind_direction_next <- NA
test$vic_wind_speed_last <- NA
test$vic_wind_speed_next <- NA
test$vic_visibility_last <- NA
test$vic_visibility_next <- NA
test$vic_pressure_last <- NA
test$vic_pressure_next <- NA
test$vic_weather_last <- NA
test$vic_weather_next <- NA

vic <- function(x){
  vic_temperature_last <- NA
  vic_temperature_next <- NA
  vic_dew_point_temperature_last <- NA
  vic_dew_point_temperature_next <- NA
  vic_humidity_last <- NA
  vic_humidity_next <- NA
  vic_wind_direction_last <- NA
  vic_wind_direction_next <- NA
  vic_wind_speed_last <- NA
  vic_wind_speed_next <- NA
  vic_visibility_last <- NA
  vic_visibility_next <- NA
  vic_pressure_last <- NA
  vic_pressure_next <- NA
  vic_weather_last <- NA
  vic_weather_next <- NA
  
  temp <- victoria[victoria$Date == as.character(x[10]),]
  temp1 <- temp[as.numeric(temp$Hour) >= as.numeric(x[11]),]
  temp2 <- temp[as.numeric(temp$Hour) <= as.numeric(x[11]),]
  
  if(nrow(temp1) > 0){
    vic_temperature_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[11])),]$Temperature.in.Celsius
    vic_dew_point_temperature_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[11])),]$Dew.Point.Temperature.in.Celsius
    vic_humidity_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[11])),]$Relative.Humidity.in.Percent
    vic_wind_direction_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[11])),]$Wind.Direction.in.Degrees
    vic_wind_speed_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[11])),]$Wind.Speed.km.per.h
    vic_visibility_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[11])),]$Visibility.in.km
    vic_pressure_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[11])),]$Station.Pressure.in.kPa
    vic_weather_last <- temp1[which.min(as.numeric(temp1$Hour) - as.numeric(x[11])),]$Weather.Ind
    
    
  }
  if(nrow(temp2) > 0){
    vic_temperature_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[11])),]$Temperature.in.Celsius
    vic_dew_point_temperature_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[11])),]$Dew.Point.Temperature.in.Celsius
    vic_humidity_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[11])),]$Relative.Humidity.in.Percent
    vic_wind_direction_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[11])),]$Wind.Direction.in.Degrees
    vic_wind_speed_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[11])),]$Wind.Speed.km.per.h
    vic_visibility_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[11])),]$Visibility.in.km
    vic_pressure_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[11])),]$Station.Pressure.in.kPa
    vic_weather_next <- temp2[which.max(as.numeric(temp2$Hour) - as.numeric(x[11])),]$Weather.Ind
  }
  return(c(vic_temperature_last,vic_temperature_next,vic_dew_point_temperature_last,vic_dew_point_temperature_next,
           vic_humidity_last,vic_humidity_next,vic_wind_direction_last,vic_wind_direction_next,
           vic_wind_speed_last,vic_wind_speed_next,vic_visibility_last,vic_visibility_next,
           vic_pressure_last,vic_pressure_next,vic_weather_last,vic_weather_next))
}

cl = makeCluster(detectCores())
clusterExport(cl,ls())
res = pbapply(test, 1,vic,cl=cl)
res
stopCluster(cl)
res=t(res)
test$vic_temperature_last <- res[,1]
test$vic_temperature_next <- res[,2]
test$vic_dew_point_temperature_last <- res[,3]
test$vic_dew_point_temperature_next <- res[,4]
test$vic_humidity_last <- res[,5]
test$vic_humidity_next <- res[,6]
test$vic_wind_direction_last <- res[,7]
test$vic_wind_direction_next <- res[,8]
test$vic_wind_speed_last <- res[,9]
test$vic_wind_speed_next <- res[,10]
test$vic_visibility_last <- res[,11]
test$vic_visibility_next <- res[,12]
test$vic_pressure_last <- res[,13]
test$vic_pressure_next <- res[,14]
test$vic_weather_last <- res[,15]
test$vic_weather_next <- res[,16]

test$Trip.Ind[test$Trip == "Tsawwassen to Swartz Bay"] = "1"
test$Trip.Ind[test$Trip == "Swartz Bay to Tsawwassen"] = "2"
test$Trip.Ind[test$Trip == "Tsawwassen to Duke Point"] = "3"
test$Trip.Ind[test$Trip == "Duke Point to Tsawwassen"] = "4"
test$Trip.Ind[test$Trip == "Horseshoe Bay to Snug Cove (Bowen Is.)"] = "5"
test$Trip.Ind[test$Trip == "Horseshoe Bay to Langdale"] = "6"
test$Trip.Ind[test$Trip == "Langdale to Horseshoe Bay"] = "7"
test$Trip.Ind[test$Trip == "Swartz Bay to Fulford Harbour (Saltspring Is.)"] = "8"
test$Trip.Ind[test$Trip == "Departure Bay to Horseshoe Bay"] = "9"
test$Trip.Ind[test$Trip == "Horseshoe Bay to Departure Bay"] = "10"
test$Trip.Ind[test$Trip == "Swartz Bay to Southern Gulf Islands"] = "11"
test$Trip.Ind[test$Trip == "Tsawwassen to Southern Gulf Islands"] = "12"

############################

library(lars)
library(selectiveInference)
library(glmnet)
library(lasso2)

train_y = train[,11]
train_x = train[,c(6:9,12:13,15:40)]
train_x$Trip.Ind = as.numeric(train_x$Trip.Ind)
str(train_x)

test_x = test[,c(5:8,11:38)]
test_x$Trip.Ind = as.numeric(test_x$Trip.Ind)
col_order = c(1:4,32,5:31)
test_x = test_x[,col_order]

# Missing Value
next_NA_index=which(is.na(train_x$next_time_traffic))
train_x$next_time_traffic[next_NA_index] = train_x$last_time_traffic[next_NA_index]
which(is.na(train_x$next_time_traffic))
which(is.na(train_x$vic_temperature_last))
which(is.na(train_x$vic_temperature_next))

train_x$van_temperature_last[which(is.na(train_x$van_temperature_last))]=train_x$van_temperature_next[which(is.na(train_x$van_temperature_last))]
train_x$van_dew_point_temperature_last[which(is.na(train_x$van_dew_point_temperature_last))]=train_x$van_dew_point_temperature_next[which(is.na(train_x$van_dew_point_temperature_last))]
train_x$van_humidity_last[which(is.na(train_x$van_humidity_last))]=train_x$van_humidity_next[which(is.na(train_x$van_humidity_last))]
train_x$van_humidex_last[which(is.na(train_x$van_humidex_last))]=train_x$van_humidex_next[which(is.na(train_x$van_humidex_last))]

train_x$vic_temperature_last[which(is.na(train_x$vic_temperature_last))]=train_x$vic_temperature_next[which(is.na(train_x$vic_temperature_last))]
train_x$vic_weather_last[which(is.na(train_x$vic_weather_last))]=train_x$vic_weather_next[which(is.na(train_x$vic_weather_last))]
train_x$vic_pressure_last[which(is.na(train_x$vic_pressure_last))]=train_x$vic_pressure_next[which(is.na(train_x$vic_pressure_last))]
train_x$vic_dew_point_temperature_last[which(is.na(train_x$vic_dew_point_temperature_last))]=train_x$vic_dew_point_temperature_next[which(is.na(train_x$vic_dew_point_temperature_last))]
train_x$vic_humidity_last[which(is.na(train_x$vic_humidity_last))]=train_x$vic_humidity_next[which(is.na(train_x$vic_humidity_last))]
train_x$vic_wind_direction_last[which(is.na(train_x$vic_wind_direction_last))]=train_x$vic_wind_direction_next[which(is.na(train_x$vic_wind_direction_last))]
train_x$vic_wind_speed_last[which(is.na(train_x$vic_wind_speed_last))]=train_x$vic_wind_speed_next[which(is.na(train_x$vic_wind_speed_last))]
train_x$vic_visibility_last[which(is.na(train_x$vic_visibility_last))]=train_x$vic_visibility_next[which(is.na(train_x$vic_visibility_last))]
which(is.na(train_x))

which(is.na(test_x$van_dew_point_temperature_last))
which(is.na(test_x$van_dew_point_temperature_next))

test_x$van_temperature_last[which(is.na(test_x$van_temperature_last))]=test_x$van_temperature_next[which(is.na(test_x$van_temperature_last))]
test_x$van_dew_point_temperature_last[which(is.na(test_x$van_dew_point_temperature_last))]=test_x$van_dew_point_temperature_next[which(is.na(test_x$van_dew_point_temperature_last))]
test_x$van_humidity_last[which(is.na(test_x$van_humidity_last))]=test_x$van_humidity_next[which(is.na(test_x$van_humidity_last))]
test_x$van_humidex_last[which(is.na(test_x$van_humidex_last))]=test_x$van_humidex_next[which(is.na(test_x$van_humidex_last))]

test_x$vic_temperature_last[which(is.na(test_x$vic_temperature_last))]=test_x$vic_temperature_next[which(is.na(test_x$vic_temperature_last))]
test_x$vic_weather_last[which(is.na(test_x$vic_weather_last))]=test_x$vic_weather_next[which(is.na(test_x$vic_weather_last))]
test_x$vic_pressure_last[which(is.na(test_x$vic_pressure_last))]=test_x$vic_pressure_next[which(is.na(test_x$vic_pressure_last))]
test_x$vic_dew_point_temperature_last[which(is.na(test_x$vic_dew_point_temperature_last))]=test_x$vic_dew_point_temperature_next[which(is.na(test_x$vic_dew_point_temperature_last))]
test_x$vic_humidity_last[which(is.na(test_x$vic_humidity_last))]=test_x$vic_humidity_next[which(is.na(test_x$vic_humidity_last))]
test_x$vic_wind_direction_last[which(is.na(test_x$vic_wind_direction_last))]=test_x$vic_wind_direction_next[which(is.na(test_x$vic_wind_direction_last))]
test_x$vic_wind_speed_last[which(is.na(test_x$vic_wind_speed_last))]=test_x$vic_wind_speed_next[which(is.na(test_x$vic_wind_speed_last))]
test_x$vic_visibility_last[which(is.na(test_x$vic_visibility_last))]=test_x$vic_visibility_next[which(is.na(test_x$vic_visibility_last))]


###################
## ML Algorithms ##
###################

# LASSO
train_x_matrix = as.matrix(train_x)
test_x_matrix = as.matrix(test_x)

lambda_seq <- 10^seq(2, -2, by = -.1)

# Splitting the data into test and train
set.seed(123)
train_ind <- sample(seq_len(nrow(train_x)), size = 35000)

train_train_x <- as.matrix(train_x[train_ind, ])
train_test_x <- as.matrix(train_x[-train_ind, ])
train_train_y <- train_y[train_ind]
train_test_y <- train_y[-train_ind]

cv_lasso <- cv.glmnet(train_train_x,as.numeric(train_train_y),nfolds = 1000,standardize=FALSE,type.measure = "mse")
cv_lasso

which(pred == "1")

m.glmnet = glmnet(train_train_x,train_train_y,)

pred <- predict(cv_lasso,newx = train_test_x)
train_train_y

pred.t <- predict(cv_lasso,newx = test_x_matrix)

which(pred > 0.5)

ID <- test$ID
Delay.Indicator <- pred.t
result = cbind(ID, Delay.Indicator)
write.csv(result,"Prediction_lasso.csv",row.names=FALSE)

# XGboost

library(xgboost)
# bst <- xgboost(data = train_train_x, label = train_train_y,max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 1)
# y_pred <- predict(bst, train_test_x)

xgb.train = xgb.DMatrix(data=train_train_x,label=train_train_y)
xgb.test = xgb.DMatrix(data=train_test_x,label=train_test_y)

# Define the parameters for multinomial classification
num_class = length(levels(as.factor(train_train_y)))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)
# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)
# Review the fit model
xgb.fit

# Predict outcomes with the test data
xgb.pred = predict(xgb.fit,train_test_x,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(as.factor(train_test_y))

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = levels(as.factor(train_train_y))[test.label+1]

# Calculate the final accuracy
result = sum(xgb.pred$prediction==train_test_y)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))

xgb.pred.t = predict(xgb.fit,test_x_matrix,reshape=T)
xgb.pred.t = as.data.frame(xgb.pred.t)
colnames(xgb.pred.t) = c("0","1")
xgb.pred.t$prediction = apply(xgb.pred.t,1,function(x) colnames(xgb.pred.t)[which.max(x)])

xgb.pred.t
which(xgb.pred.t$prediction == "1")

ID <- test$ID
Delay.Indicator <- xgb.pred.t$prediction
result = cbind(ID, Delay.Indicator)
write.csv(result,"Prediction.csv",row.names=FALSE)

str(test_x_matrix)

# Random Forest
library(randomForest)
rf<-randomForest(train_x_matrix,as.numeric(train_y))
p_rf = predict(rf)

ID <-test$ID
Delay.Indicator <- p_rf
result = cbind(ID, Delay.Indicator)
write.csv(result,"Prediction.csv",row.names=FALSE)

# Accuracy of Random Forest
rf_tab <- table(as.character(p_rf),as.character(train_y))
confusionMatrix(rf_tab)