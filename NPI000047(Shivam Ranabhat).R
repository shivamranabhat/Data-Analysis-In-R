

install.packages("readxl") #package which include functions to ready excel files
install.packages("colorspace") # package used for manipulating and assessing colors and palettes
install.packages("ggplot2") #package which includes ggplot functions
install.packages("dplyr") #package which includes filter functions)
install.packages("VIM") #package which include functions to read null value 
library(VIM)
library(readxl) 
library(colorspace) 
library(ggplot2) 
library(dplyr)  
Weather = read.csv("C:\\Users\\Info-chip\\Documents\\Hourlyweatherdata.csv" )
print(Weather) #To print the excel file
JFK = filter(Weather, origin  =="JFK")
LGA = filter(Weather, origin  =="LGA")

#--------------------------Analysis 1-----------------------------#

#Study Temperature of JFK and LGA for whole year of 2013 in form of geom smooth
ggplot(Weather, aes(x=month, y=temp, na.rm=TRUE, suppressWarnings(expr)))+geom_smooth(size= 1, se=FALSE, aes(color=origin))+ 
  labs(x="Month", y="Temperature of Both JFK and LGA Airport")+ scale_x_discrete (limits=c(1:12))
summary(JFK$temp)
summary(LGA$temp)
#------------------------------------------------------------#

#--------------------------Analysis 2-----------------------------#

#Study Humidity of JFK and LGA for whole year of 2013
#Humidity comparison for JFK and LGA for for whole year of 2013

Comparehumidity=ggplot(Weather,aes(x=origin,y=humid))+geom_boxplot()+
  labs(title = "Humidity recorded from JFK and LGA weather Stations",x="weather station",
       y="Humidity");
print(Comparehumidity)
summary(JFK$humid)
summary(LGA$humid)
#------------------------------------------------------------#


#--------------------------Analysis 3-----------------------------#

#Study Wind Direction of JFK and LGA for whole year of 2013
#Wind direction comparison for JFK  for for whole year of 2013
options(warn = -1)

JFK %>% ggplot(aes(wind_dir))+ geom_bar(aes(color=wind_dir,na.rm=TRUE ))+ facet_wrap(~month)+
  labs(title="Wind direction for each day of Year 2013 of JFK Airport", x= "Wind direction of JFK Airport")
summary(JFK$wind_dir)
#Wind direction comparison for LGA  for for whole year of 2013
options(warn = -1)
LGA %>% ggplot(aes(wind_dir))+ geom_bar(aes(color=wind_dir,na.rm=TRUE ))+ facet_wrap(~month)+
  labs(title="Wind direction for each day of Year 2013 of LGA Airport", x= "Wind direction of LGA Airport")
summary(LGA$wind_dir)

#------------------------------------------------------------#


#--------------------------Analysis 4-----------------------------#
#Study Wind Speed of JFK and LGA for whole year of 2013
#Wind speed comparison for JFK  for for whole year of 2013
ggplot(JFK,aes(x=day, y=wind_speed, na.rm=TRUE))+ geom_point()+ facet_wrap(~month)+
  labs(title="Wind speed for each day of Year 2013 of JFK Airport",x="Days" ,y= "Wind speed of JFK Airport")
summary(JFK$wind_speed)
#Wind speed comparison for LGA  for for whole year of 2013
ggplot(LGA,aes(x=day, y=wind_speed, na.rm=TRUE))+ geom_point()+ facet_wrap(~month)+
  labs(title="Wind speed for each day of Year 2013 of LGA Airport",x="Days" , y= "Wind speed of LGA Airport")
summary(LGA$wind_speed)
#-----------------------------------------------------------------#


#--------------------------Analysis 5-----------------------------#
#Study  maximum precipitation of JFK and LGA for whole year of 2013
#Monthly maximum precipitation comparison for JFK  for for whole year of 2013
ggplot(JFK %>% group_by(month) %>%summarise(precip = max(precip)),aes(x = month, y = precip)) + 
  geom_point()+geom_line() + scale_x_discrete(limits = c(1:12)) + 
  labs(title="Maximum Precipitation  of JFK in 2013",x="Month", y="Maximum")
summary(JFK$precip)
#Monthly maximum precipitation comparison for LGA  for for whole year of 2013
ggplot(LGA %>% group_by(month) %>%summarise(precip = max(precip)),aes(x = month, y = precip)) + 
  geom_point()+geom_line() + scale_x_discrete(limits = c(1:12)) + 
  labs(title="Maximum precipitation  of LGA in 2013",x="Month", y="Maximum")
summary(LGA$precip)
#-----------------------------------------------------------------#


#--------------------------Analysis 6-----------------------------#
#Study  Dew point of JFK and LGA for whole year of 2013
#Dew point of JFK airport of whole year 2013
JFK %>% ggplot(aes(x=month, y=dewp,na.rm=TRUE))+ geom_bar(stat="identity", color="blue", fill="white")+ 
  scale_x_continuous(breaks = seq(0,12, by=1))+
  labs(title="Dew point for  Year 2013 of JFK Airport", x= "Months")
summary(JFK$dewp)
#Dew point of LGA airport of whole year 2013
LGA %>% ggplot(aes(x=month, y=dewp,na.rm=TRUE))+ geom_bar(stat="identity", color="blue",fill="white")+ 
  scale_x_discrete(limit=c(1:12))+ labs(title="Dew point for  Year 2013 of LGA Airport", x= "Months")
summary(LGA$dewp)
#-----------------------------------------------------------------#

#--------------------------Analysis 7-----------------------------#
#Study  wind gust of JFK and LGA for whole year of 2013
#Wind gust of JFK airport of whole year 2013
ggplot(JFK %>% group_by(month) %>%summarise(wind_gust, na.rm=TRUE),aes(x = month, y = wind_gust)) + 
  geom_point()+geom_line() + scale_x_discrete(limits = c(1:12)) + 
  labs(title="Wind gust  of JFK in 2013",x="Month", y="Overall wind gust")
summary(JFK$wind_gust)
#wind gust of LGA airport of whole year 2013
ggplot(LGA %>% group_by(month) %>% summarise(wind_gust,na.rm=TRUE),aes(x = month, y = wind_gust)) + 
  geom_point()+ geom_line() + scale_x_discrete(limits = c(1:12)) + 
  labs(title="Wind gust  of LGA in 2013",x="Month", y="Overall wind gust")
summary(LGA$wind_gust)
#-----------------------------------------------------------------#

#--------------------------Analysis 8-----------------------------#
#Finding null value of JFK for  January of 2013
#Pre-processing
JFKJan = filter(Weather, origin  =="JFK", month=="1")
JFKNA = JFKJan %>% select(temp:visib) %>% aggr(prop=T, numbers=T) 
summary(JFKNA)
#Finding null value of LGA for  January of 2013
#Pre-processing
LGAJan = filter(Weather, origin  =="LGA", month=="1")
LGANA = LGAJan %>% select(temp:visib) %>% aggr(prop=T, numbers=T) 
summary(LGANA)
#-----------------------------------------------------------------#
#--------------------------Analysis 9-----------------------------#
#Study  temperature below ice point of JFK for January of 2013
#Pre-processing
icepointJFK= JFKJan%>% group_by(day)%>% filter(origin=="JFK")%>%filter(temp <= 32)%>%
  select(month,day,temp)
ggplot(icepointJFK, aes(x=day, y=temp,)) + geom_point() + facet_wrap(~month)+
  labs(title="Temperature below ice point for January of Year 2013 of JFK", 
       x= "Days of January")
summary(icepointJFK$temp)

#Study  temperature below ice point of LGA for January of 2013
#Pre-processing
icepointLGA= LGAJan%>% group_by(day)%>% filter(origin=="LGA")%>%filter(temp <= 32)%>%
  select(month,day,temp)
ggplot(icepointLGA, aes(x=day, y=temp,)) + geom_point() + facet_wrap(~month)+
  labs(title="Temperature below ice point for January of Year 2013 of LGA ", 
       x= "Days of January")
summary(icepointLGA$temp)

#-----------------------------------------------------------------#

#--------------------------Analysis 10-----------------------------#
#Study  the relation of temperature and visibility  of spring season  2013
# Wind speed and pressure In JFK Airport
#Pre-processing for JFK
springJFK= JFK%>% group_by(month)%>% filter(origin=="JFK")%>%filter(month =="3"|month =="4"|month =="5" )%>%
  select(origin,month,day,wind_speed, pressure)

ggplot(springJFK,aes(wind_speed,pressure,color=origin))+geom_point()+geom_smooth(colour = "blue", size=3)+
  facet_wrap(~month)+
  labs(title="Wind Speed VS Pressure comparison for JFK",
       x="Wind Speed", y="Pressure ")
summary(springJFK[,4:5])
# Wind speed and pressure In LGA Airport
#Pre-processing for LGA
springLGA= LGA%>% group_by(month)%>% filter(origin=="LGA")%>%filter(month =="3"|month =="4"|month =="5" )%>%
  select(origin,month,day,wind_speed, pressure)

ggplot(springLGA,aes(wind_speed,pressure,color=origin))+geom_point()+geom_smooth(colour = "blue", size=3)+
  facet_wrap(~month)+
  labs(title="Wind Speed VS Pressure comparison for LGA",
       x="Wind Speed", y="Pressure ")
summary(springLGA[,4:5])

#-----------------------------------------------------------------#

#--------------------------Analysis 11-----------------------------#
#Study  the  temperature of December 22 of 2013 JFK airport
#Pre-processing
JFKDec= JFK%>% group_by(day)%>% filter(origin=="JFK")%>% filter(month=="12")%>% filter(day=="22")%>% 
  select(origin, month, day,hour, temp)
ggplot(JFKDec, aes(x=hour, y=temp))+ geom_line()+ geom_point()+ scale_x_discrete(limits=c(1:23))+
 labs(title="Temperature of 22 dec of JFK",
       x="Hour", y="Temperature ")
summary(JFKDec$temp)
#Study  the  temperature of December 22 of 2013 LGA airport
LGADec= LGA%>% group_by(day)%>% filter(origin=="LGA")%>% filter(month=="12")%>% filter(day=="22")%>% 
  select(origin, month, day,hour, temp)
ggplot(LGADec, aes(x=hour, y=temp))+ geom_line()+ geom_point()+ scale_x_discrete(limits=c(1:23))+
  labs(title="Temperature of 22 dec of LGA",
         x="Hour", y="Temperature ")
summary(LGADec$temp)
#-----------------------------------------------------------------#
#--------------------------Analysis 12-----------------------------#
#Study  the   dew point of 1st January of 2013 JFK And LGA airport
#Pre-processing for JFK 
option(warn=-1)
Jan_dewpointJFK = JFK%>% group_by(day)%>% filter(origin=="JFK")%>% filter(month=="1")%>%filter(day=="1")%>%
  select(origin,month, day, hour,dewp)
ggplot(Jan_dewpointJFK, aes(x=hour, y=dewp))+ geom_line()+geom_point()+ scale_x_discrete(limits=c(1:23))+
  labs(title="Dew point of 1st dec of JFK",
         x="Hour", y="Dew point ")
summary(Jan_dewpointJFK$dewp)

#Pre-processing for LGA 
option(warn=-1)
Jan_dewpointLGA = LGA%>% group_by(day)%>% filter(origin=="LGA")%>% filter(month=="1")%>%filter(day=="1")%>%
  select(origin,month, day, hour,dewp)
ggplot(Jan_dewpointLGA, aes(x=hour, y=dewp))+ geom_line()+geom_point()+ scale_x_discrete(limits=c(1:23))+
  labs(title="Dew point of 1st dec of LGA",
       x="Hour", y="Dew point ")
summary(Jan_dewpointLGA$dewp)
#-----------------------------------------------------------------#

#--------------------------Analysis 13-----------------------------#
#Study  the  visibility of December of 2013 JFK And LGA airport
#Pre-processing for JFK airport
JFKDecm= JFK%>% group_by(day)%>% filter(origin=="JFK")%>% filter(month=="12")%>% 
  select(origin, month, day,hour, visib)
ggplot(JFKDecm, aes(x=day, y=visib)) + geom_point()+scale_x_discrete(limits=c(1:30))+
  labs(title="Visibility dec of JFK",
       x="Hour", y="Visibility ")
summary(JFKDecm$visib)
#Pre-processing for  LGA airport
LGADecm= LGA%>% group_by(day)%>% filter(origin=="LGA")%>% filter(month=="12")%>% 
  select(origin, month, day,hour, visib)
ggplot(LGADecm, aes(x=day, y=visib)) + geom_point()+scale_x_discrete(limits=c(1:30))+
  labs(title="Visibility dec of LGA",
       x="Hour", y="Visibility ")
summary(LGADecm$visib)
#-----------------------------------------------------------------#

#--------------------------Analysis 14-----------------------------#
#Study  the  maximum pressure of 2013 JFK And LGA airport
# for JFK airport
option(warn=-1)
ggplot(JFK %>% group_by(month) %>%summarise(pressure = max(pressure, na.rm=TRUE)),aes(x = month, y = pressure)) + 
  geom_point()+geom_line() + scale_x_discrete(limits = c(1:12)) + 
  labs(title="Maximum pressure  of JFK in 2013",x="Month", y="Maximum Pressure")
summary(JFK$pressure)
# for LGA airport
option(warn=-1)
ggplot(LGA %>% group_by(month) %>%summarise(pressure = max(pressure, na.rm=TRUE)),aes(x = month, y = pressure)) + 
  geom_point()+geom_line() + scale_x_discrete(limits = c(1:12)) + 
  labs(title="Maximum pressure  of LGA in 2013",x="Month", y="Maximum Pressure")
summary(LGA$pressure)
#-----------------------------------------------------------------#























