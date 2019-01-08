library(nycflights13)
library(dplyr)
library(ggplot2)
library(lubridate)

######## To check the Tibbles present in package nycflights13#################
data(package='nycflights13')

############ Creating a local Copy #######################
my_flights <- flights


############ Selecting the mentioned columns ################
my_flights <- select(my_flights,time_hour,origin,dest,carrier,dep_delay,arr_delay,air_time,distance)
############ Filtering out the missing values for dep_delay and arr_delay#############
my_flights <- my_flights %>% filter(!(is.na(dep_delay)),!(is.na(arr_delay)))
print(my_flights)

############ Adding Columns DayofWeek and Hourof Day #######################
my_flights <- mutate(my_flights,DayOfWeek = wday(time_hour, label = TRUE), HourOfDay = hour(time_hour) , Month = month(time_hour , label = TRUE))
select(my_flights,time_hour,DayOfWeek,HourOfDay,everything())

################ Grouping the tibble my_flights by Hour of the Day
by_hour <- group_by(my_flights,HourOfDay)
by_hour
############ Computing the statistics for dep_delay by  Hour ################
delay_hourly <- summarise(by_hour,AvrDepDelay = mean(dep_delay),
          SD = sd(dep_delay), MinDelay = min(dep_delay),
          MaxDelay = max(dep_delay),
          MaxDelayHours = MaxDelay/60) %>% arrange(-AvrDepDelay)
print(delay_hourly)

############ Computing the statistics for dep_delay by  Month ################
################ Grouping the tibble my_flights by Month
by_month <- group_by(my_flights,Month)
delay_monthly <- summarise(by_month,AvrDepDelay = mean(dep_delay),
                          SD = sd(dep_delay), MinDelay = min(dep_delay),
                          MaxDelay = max(dep_delay),
                          MaxDelayHours = MaxDelay/60) %>% arrange(-AvrDepDelay)
print(delay_monthly)

############ Computing the statistics for dep_delay by  Carrier ################
################ Grouping the tibble my_flights by Carrier
by_carrier <- group_by(my_flights,carrier)
delay_carrier <- summarise(by_carrier,AvrDepDelay = mean(dep_delay),
                           SD = sd(dep_delay), MinDelay = min(dep_delay),
                           MaxDelay = max(dep_delay),
                           MaxDelayHours = MaxDelay/60,
                           NObs = sum(complete.cases(carrier))) %>% arrange(-AvrDepDelay)
print(delay_carrier)

############ Computing the statistics for dep_delay by  Airport and Month ################
################ Grouping the tibble my_flights by Origin and Month
by_airport_month <- group_by(my_flights,origin,Month)
delay_airport_month <- summarise(by_airport_month,AvrDepDelay = mean(dep_delay),
                           SD = sd(dep_delay), MinDelay = min(dep_delay),
                           MaxDelay = max(dep_delay),
                           MaxDelayHours = MaxDelay/60,
                           NObs = sum(complete.cases(Month))) %>% arrange(Month)
print(delay_airport_month)

############ Computing the statistics for dep_delay by  Airport and Hour ################
################ Grouping the tibble my_flights by Origin and Hour
by_airport_time <- group_by(my_flights,HourOfDay,origin)
delay_airport_time <- summarise(by_airport_time,AvrDepDelay = mean(dep_delay),
                                 SD = sd(dep_delay), MinDelay = min(dep_delay),
                                 MaxDelay = max(dep_delay),
                                 MaxDelayHours = MaxDelay/60,
                                 NObs = sum(complete.cases(HourOfDay))) %>% arrange(HourOfDay)
print(delay_airport_time)


################### Adding a new column for determining the hour of the day ################
my_flights <-  my_flights %>% mutate(
      DaySection = case_when(
      HourOfDay %in% 5:12 ~ "Morning",
      HourOfDay %in% 12:18 ~ "Afternoon",
      HourOfDay >= 18 ~  "Evening"
    ))
select(my_flights,DaySection,everything())
################### Creating a sample dataset and filtering out departure delay values ########

set.seed(99)
c <- sample_n(my_flights, 10000)
myf_sample <- filter(c,dep_delay <= 180)
myf_sample

###################Boxplot to visualise the departure delay by the three different time#############

ggplot(data = myf_sample) +
  geom_boxplot(mapping=aes(x=Month,y=dep_delay, color = DaySection)) +
  ylab("Departure Delay") +
  xlab("Month")

