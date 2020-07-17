##### set working directory ###############################
rm(list = ls())

#getwd()
setwd("C:/Users/915712257/Desktop/learningR follow up")

#load libraries #############################
library(ggplot2)
library(scales)
library(grid)
library(dplyr)
library(lubridate)
#library(readxl)
#library(tidyr)

#LOAD THE FILE~~~~~~~~~~~~~~~~~~~~~
list.files()

#choose.files('SFBar_TempBUOY.csv')

 #~ enter as a data set in environment:
SFBar <- read.csv('SFBar_TempBUOY.csv', sep = ",", 
                  header = T, stringsAsFactors = F, 
                  na.strings = "NaN") 



#Separate the T and Z from Date&Time and force to POSIXct format with UTC tz
colnames(SFBar)
SFBar$Date_Time<- ymd_hms(SFBar$time, tz = "UTC")

str(SFBar)

#Extracting the Date to be in a column
SFBar$Date<- as.Date(SFBar$Date_Time)

str(SFBar)



#Creating a years only column
SFBar$year <- as.numeric(format(as.Date(SFBar$Date, format="%Y-%m-%d"),"%Y"))

str(SFBar)

# rm(SFBar.byAvg)


# TO GET AVG TEMP BY DAY
# SFBar.byAvg <- SFBar %>%
#   group_by(Date) %>%
#   summarise(daily_SST = mean(sea_water_temperature), n = n()) %>%
#   arrange(Date$year)


# TO GET AVG TEMP BY DAY
SFBar.byAvg <- SFBar %>%
  group_by(Date) %>%
  summarise(daily_SST = mean(sea_water_temperature, na.rm = T)) 

# TO GET AVG TEMP BY DAY
SFBar.byAvg <- SFBar %>%
  group_by(Date) %>%
  summarise(daily_SST = mean(sea_water_temperature, na.rm = T),
            datetime = median(Date_Time))

SFBar.byAvg$datetime <- round_date(SFBar.byAvg$datetime, unit = "day")

str(SFBar.byAvg)

# datetime = median(Date_Time)

# str(SFBar.byAvg)
# 
# SFBar.byAvg$Date <- as.Date(SFBar.byAvg$Date)

# season time bounds to use in limiting the x axis

t1 <-  "2009-01-01 00:00:00"
t2 <- "2020-07-14 00:00:00"

#Graphing Daily Avg Temp over Time
summary(SFBar.byAvg$daily_SST)


p <- ggplot(SFBar.byAvg, aes(datetime, daily_SST))

SFBarPlot <- p + geom_point(aes(), size = 0.25, color = "red3") +
  xlab("Time") + #last x label sets the time axis label
  ylab("Water Temp (degC)")+ 
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8, 19) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


# theoretical second plot graph

#make sure the data range is the longest for the x definition of datetime
p <- ggplot(SFBar.byAvg, aes(x = datetime)) 

SFBarPlot <- p + 
  geom_point(aes(data = SFBar.byAvg, y = dailySST),size = 0.25, color = "red3") +
  geom_point(aes(data = df2, y = plot_parameter),size = 0.25, color = "blue") +
  xlab("Time") + #last x label sets the time axis label
  ylab("Water Temp (degC)")+ 
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8, 19) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))




print(SFBarPlot)
