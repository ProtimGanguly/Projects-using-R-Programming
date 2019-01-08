library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)

#Loading the energy dataset
ener <- read_excel("C:/Users/prodi/Desktop/Books/Programming for Data Analytics in R/IrelandData January 2017.xlsx")

print(ener)

#Using TidyR function separate() to add columns Date and Time 
ener <- ener %>% separate(DateTime,into = c("Date" , "Time"), sep = " ", remove = FALSE)
#Using Lubridate function hour,minute,wday to add all the other columns to the tibble
ener <- mutate(ener ,HourOfDay = hour(DateTime),MinuteOfDay = minute(DateTime) ,
               DayOfWeek = wday(DateTime, label = TRUE),
               NIFlow = case_when(
                 NetImports < 0 ~ "Exporting",
                 NetImports > 0 ~ "Importing"))

print(ener)


#Plotting graph of Net Import Data vs Time, colour by NIFlow and facet by day of week.
ggplot(data = ener ) + 
  ggtitle("Time v Net Imports By Day of Week") +
  geom_point(mapping = aes(x = HourOfDay,y = NetImports, color = NIFlow)) + 
  xlab("Time (Hour of Day)") +
  ylab("Net Imports") +
  scale_color_discrete( breaks=c("Exporting" , "Importing"), # For changing the Legend Text
                      labels=c("Exports" , "Imports")) +
  facet_wrap(~ DayOfWeek)



#Plotting graph of Wind Generation vs CO2 Emissions
ggplot(data = ener) + 
  ggtitle("Wind Generation v CO2 Emissions") +
  geom_point(mapping = aes(x = Wind,y = CO2, color = NIFlow)) + 
  xlab("Wind Genearation") +
  ylab("CO2 Emissions") +
  scale_color_discrete( breaks=c("Exporting" , "Importing"), # For changing the Legend Text
                        labels=c("Exports" , "Imports"))

# Loading the Weather Data Set
weather <- read_excel("C:/Users/prodi/Desktop/Books/Programming for Data Analytics in R/Mac Head Wind Data.xlsx")

print(weather)

#Separating DATETIME to extract Date using TidyR function separate()
weather <- weather %>% separate(Date ,into = c("Date"), extra = "merge",
                                sep = " ", convert = TRUE)
#Converting to Date Format
weather$Date = date(weather$Date) 


#Graph of average wind speed
ggplot(data = weather, aes(x = Date,y = AVRWind)) + 
  geom_point(color = "blue") + 
  geom_line(linetype="dashed") +
  xlab("Date") +
  ylab("Average Wind Speed(Knots)")


#Converting Date column to Date format from chr format
ener$Date = date(ener$Date)
#Grouping the energy Dataset by Date column to pass into summarise() function
by_date <- group_by(ener,Date)
#Summarising the average wind generation for every day in the dataset
avr_daily_wind <- summarise(by_date,AverageWindGeneration = mean(Wind))
print(avr_daily_wind)

#Combining the two Weather and Enery Dataset with Inner Join
compare_wind <- inner_join(weather, avr_daily_wind , by = 'Date')
#Plotting graph of Wind Speed vs Wind Power Generated
ggplot(data = compare_wind, aes(x = AVRWind,y = AverageWindGeneration)) + 
  ggtitle("Wind Speed v Wind Power Generated") +
  geom_point() +
  xlab("Average Wind Speed(Mace Head)") +
  ylab("Average Wind Generation")

#Plotting graph of Wind Speed vs Wind Power Generated with Linear Model
ggplot(data = compare_wind, aes(x = AVRWind,y = AverageWindGeneration)) + 
  ggtitle("Wind Speed v Wind Power Generated with Linear model") +
  geom_point() +
  geom_smooth(method=lm , se = TRUE) +
  xlab("Average Wind Speed(Mace Head)") +
  ylab("Average Wind Generation")

#Plotting graph of Wind Speed vs Wind Power Generated with Loess Model
ggplot(data = compare_wind, aes(x = AVRWind,y = AverageWindGeneration)) + 
  ggtitle("Wind Speed v Wind Power Generated with Loess model") +
  geom_point() +
  geom_smooth(method=loess) +
  xlab("Average Wind Speed(Mace Head)") +
  ylab("Average Wind Generation")

