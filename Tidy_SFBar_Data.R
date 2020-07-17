##### set working directory ###############################
rm(list = ls())

getwd()
setwd('C:/Users/e/Desktop/REU DATA/BUOY Data')

#load libraries #############################
library(ggplot2)
library(scales)
library(Scale) #Not a package?
library(grid)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyr)

#LOAD THE FILE~~~~~~~~~~~~~~~~~~~~~
list.files()

choose.files('SFBar_TempBUOY.csv')

 #~ enter as a data set in environment:
SFBar <- read.csv('SFBar_TempBUOY.csv', na.strings=c(""," ","NaN")) 


#Separate the T and Z from Date&Time
colnames(SFBar)
SFBar$Date_Time<- ymd_hms(SFBar$time)

#Extracting the Date to be in a column
SFBar$Date<- format(as.Date(SFBar$Date_Time, format="%Y-%m-%d"),"%m/%d/%Y")

#Extracting time to be in a column
SFBar$Time_formatted <- format(as.POSIXct(strptime(SFBar$Date_Time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")

#Creating a years only column
SFBar$year <- as.numeric(format(as.Date(SFBar$Date, format="%Y-%m-%d"),"%Y"))

rm(SFBar.byAvg)
# TO GET AVG TEMP BY DAY
SFBar.byAvg <- SFBar %>%
  group_by(Date) %>%
  summarise(sea_water_temperature = mean(sea_water_temperature),
            n= n()) %>%
  arrange(Date$year)



# season time bounds to use in limiting the x axis
t1 <-  "2009-01-1"
t2 <- "2020-07-13"

#Graphing Daily Avg Temp over Time
summary(SFBar.byAvg$sea_water_temperature)


p <- ggplot(SFBar.byAvg, aes(Date, sea_water_temperature))

SFBarPlot <- p + geom_point(aes(), size = 0.25, color = "red3") +
  xlab("Time") + #last x label sets the time axis label
  ylab("Water Temp (degC)")+ 
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d"),
         as.POSIXct(t2, format = "%Y-%m-%d"))) +
  ylim(0, 15) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(SFBarPlot)