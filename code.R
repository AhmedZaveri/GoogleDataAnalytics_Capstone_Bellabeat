
#Installing Packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("cowplot")
install.packages("hms")
install.packages("ggrepel")
  
#Loading Packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)
library(skimr)
library(janitor)
library(cowplot)
library(plotly)
library(hms)
library(ggrepel)
library(knitr)
  
#Checking Current Working Directory and Set Working Directory (If required)
getwd()
setwd("~/Zaveri Data/TUM/9th Semester/Google Data Analytics/Capstone - Bellabeat/Bellabeat Datasets")

#Reading CSV Files 
daily_activity <- read_csv("dailyActivity_merged.csv")
head(daily_activity)
colnames(daily_activity)
str(daily_activity)
glimpse(daily_activity)
  
daily_calories <- read_csv("dailyCalories_merged.csv")
head(daily_calories)
colnames(daily_calories)
str(daily_calories)
glimpse(daily_activity)
  
daily_intensities <- read_csv("dailyIntensities_merged.csv")
head(daily_intensities)
colnames(daily_intensities)
str(daily_intensities)
glimpse(daily_intensities)
  
heartrate <- read_csv("heartrate_seconds_merged.csv")
head(heartrate)
colnames(heartrate)
str(heartrate)
glimpse(heartrate)
  
hourly_calories <- read_csv("hourlyCalories_merged.csv")
head(hourly_calories)
colnames(hourly_calories)
str(hourly_calories)
glimpse(hourly_calories)
  
hourly_steps <- read_csv("hourlySteps_merged.csv")
head(hourly_steps)
colnames(hourly_steps)
str(hourly_steps)
glimpse(hourly_steps)
  
sleep <- read_csv("sleepDay_merged.csv")
head(sleep)
colnames(sleep)
str(sleep)
glimpse(sleep)
  
weight <- read_csv("weightLogInfo_merged.csv")
head(weight)
colnames(weight)
str(weight)
glimpse(weight)

#Checking how many NA values are there in each Dataset
sum(is.na(daily_activity))
sum(is.na(daily_calories))
sum(is.na(daily_intensities))
sum(is.na(heartrate))
sum(is.na(hourly_calories))
sum(is.na(hourly_steps))
sum(is.na(sleep))
sum(is.na(weight))

#Checking duplication of data
sum(duplicated(daily_activity))
sum(duplicated(daily_calories))
sum(duplicated(daily_intensities))
sum(duplicated(heartrate))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_steps))
sum(duplicated(sleep))
sum(duplicated(weight))

#Removing duplicated data in sleep dataset
sleep <- distinct(sleep)
sum(duplicated(sleep))

#Removing "Fat" column which had NA values
weight <- weight[,-5]
sum(is.na(weight))

#Adding Weekday column to Sleep Dataset
library(dplyr)
sleep <- sleep %>% mutate(Weekday = weekdays(as.Date(SleepDay, "%m/%d/%Y")))

#Adding Weekday Column to daily_activity Dataset
daily_activity <- daily_activity %>% 
  mutate(Weekday = weekdays(as.Date(ActivityDate, "%m/%d/%Y")))

#Changing Date Column from Character to Date 
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, "%m/%d/%Y")
daily_calories$ActivityDay <- as.Date(daily_calories$ActivityDay, "%m/%d/%Y")
daily_intensities$ActivityDay <- as.Date(daily_intensities$ActivityDay,
                                         "%m/%d/%Y")
sleep$Date <- as.Date(sleep$SleepDay, "%m/%d/%Y")
weight$Date_new <- as.Date(weight$Date, "%m/%d/%Y")

#Splitting Datetime Column into separate Date and Time Columns 
heartrate$datetime <- str_split_fixed(heartrate$Time, " ", n = 2)
heartrate$Date <- as.Date(heartrate$Time, "%m/%d/%Y")
heartrate$Time_new <- format(strptime(heartrate$datetime[,2], "%I:%M:%S %p"), 
                             format="%H:%M:%S")
heartrate <- heartrate[-4]
heartrate$Time_new <- as_hms(heartrate$Time_new)

hourly_calories$datetime <- str_split_fixed(hourly_calories$ActivityHour, " ",
                                            n = 2)
hourly_calories$Date <- as.Date(hourly_calories$ActivityHour, "%m/%d/%Y")
hourly_calories$time_new <- format(strptime(hourly_calories$datetime[,2],
                                            "%I:%M:%S %p"), format="%H:%M:%S")
hourly_calories <- hourly_calories %>%  rename("Time" = "time_new")
hourly_calories <- hourly_calories[-4]
hourly_calories$Time <- as_hms(hourly_calories$Time)

hourly_steps$datetime <- str_split_fixed(hourly_steps$ActivityHour, " ", n = 2)
hourly_steps$Date <- as.Date(hourly_steps$ActivityHour, "%m/%d/%Y")
hourly_steps$Time <- format(strptime(hourly_steps$datetime[,2], "%I:%M:%S %p"),
                            format="%H:%M:%S")
hourly_steps <- hourly_steps[-4]
hourly_steps$Time <- as_hms(hourly_steps$Time)

#Counting Distinct IDs in each dataset
count(daily_activity %>% distinct(Id))
count(daily_calories %>% distinct(Id))
count(daily_intensities %>% distinct(Id))
count(heartrate %>% distinct(Id))
count(sleep %>% distinct(Id))
count(weight %>% distinct(Id))
count(hourly_steps %>% distinct(Id))
count(hourly_calories %>% distinct(Id))

#Sedentary Minutes vs. Total Steps
ggplot(data = daily_activity, aes(x = TotalSteps, y = SedentaryMinutes)) + 
       geom_point() + geom_smooth(method = lm, se = FALSE) + 
       labs(title="Sedentary Minutes vs. Total Steps") + xlab("Total Steps") +
       ylab("Sedentary Minutes")

#Calories vs. Total Steps
ggplot(data = daily_activity, aes(x = TotalSteps, y = Calories)) + 
       geom_point() + geom_smooth(method = lm, se = FALSE) + 
       labs(title = "Calories vs. Total Steps") + xlab("Total Steps") +
       ylab("Calories")

#Weekday order to to arrange bars of bar plot as per day of the week sequence   
days_of_the_week <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                      "Friday", "Saturday")

#Average Daily Steps on Each Day of The Week
ggplot(data = daily_activity, aes(x = factor(Weekday, days_of_the_week),
       y = TotalSteps)) + geom_bar(stat = "summary", fun = "mean") + 
       labs(title = "Average Daily Steps on Each Day of The Week") + 
       xlab("Day of the Week") + ylab("Average No. of Steps") 

#Changing ID Column from double to character
daily_activity$Id <- as.character(daily_activity$Id)
sleep$Id <- as.character(sleep$Id)

#Average Steps of Each Individual
AverageSteps <- daily_activity %>% select(Id, TotalSteps) %>% group_by(Id) %>% 
  summarise(mean(TotalSteps))

#Adding new column to distribute in different groups 
AverageSteps <- AverageSteps %>% rename("AvgSteps" = "mean(TotalSteps)")
AverageSteps <- AverageSteps %>% 
  mutate(
    ActivityIntensity = case_when(
      AvgSteps >= 12000 ~ "Very Active",
      AvgSteps >= 8000 & AvgSteps < 12000 ~ "Active",
      AvgSteps >= 5000 & AvgSteps < 8000  ~ "Slightly Active",
      AvgSteps < 5000 ~ "Inactive"
    )
  )

#Average Steps of Each Individual
ggplot(data = AverageSteps) + geom_col(mapping = aes(x = Id, y = AvgSteps, 
       fill = ActivityIntensity)) + theme(axis.text.x = element_text(angle = 90,
       vjust = 0.5, hjust=1)) + labs(title = "Average Steps of Each Individual")
       + xlab("ID of Individual") + ylab("Average No. of Steps") +
       geom_hline(yintercept = mean(AverageSteps$AvgSteps)) + 
       scale_fill_discrete(name = "Intensity of Actiivty")
#OR
ggplot(data = daily_activity, aes(x = Id, y = TotalSteps)) + 
       geom_bar(stat = "summary", fun = "mean") + 
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
       labs(title = "Average Steps of Each Individual") + 
       xlab("ID of Individual") + ylab("Average No. of Steps") 

#Average Minutes of Sleep of Each Individual
ggplot(data = sleep, aes(x = Id, y = TotalMinutesAsleep)) + 
       geom_bar(stat = "summary", fun = "mean") + 
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
       labs(title = "Average Minutes of Sleep of Each Individual") + 
       xlab("ID of Individual") + ylab("Average Minutes of Sleep") + 
       geom_hline(yintercept = mean(sleep$TotalMinutesAsleep))

#Converting activity dataset from wide to long    
daily_activity_long <- gather(daily_activity, key = "Activitytype", 
                              value = "Minutes",11:14)

#Minutes Spent on Each Type of Activity by Each Individual
ggplot(data = daily_activity_long, aes(fill = Activitytype, x = Id, 
       y = Minutes)) + geom_bar(position = "stack", stat = "summary", 
       fun = "mean") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5,
       hjust=1)) + labs(title = "Minutes Spent on Each Type of Activity by Each 
       Individual") + xlab("ID of Individual") + ylab("Minutes of Activity") + 
       scale_fill_discrete(name = "Type of Activity",labels = c("Fairly Active", 
       "Lightly Active", "Sedentary", "Very Active"))
       

#Average Minutes for each type of Activity
AverageActivityMinutes <- daily_activity_long %>% 
  select(Activitytype, Minutes) %>% group_by(Activitytype) %>% 
  summarise(mean(Minutes))
AverageActivityMinutes <- AverageActivityMinutes %>% 
  rename("AvgMins" = "mean(Minutes)")
AverageActivityMinutes$AvgMins <- round(AverageActivityMinutes$AvgMins, 1)

#Pie Chart to show Average Minutes for each type of Activity
# ggplot(data = AverageActivityMinutes, aes(x = "", y = AvgMins, fill = Activitytype)) +
#        geom_col(color = "black") + geom_bar() + coord_polar(theta = "y") +
#        geom_text_repel(aes(label = AvgMins, accuracy = 1),
#        position = position_stack(vjust = 0.5)) + scale_fill_discrete(name = "Type of Activity",
#        labels = c("Fairly Active", "Lightly Active", "Sedentary", "Very Active")) +
#        labs(title = "Average Minutes for Each Type of Activity in a Day")

#Pie Chart to show Average Minutes for each type of Activity
plot_ly(AverageActivityMinutes, labels = ~Activitytype, values = ~AvgMins, 
        type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
        layout(title = 'Average Minutes for Each Type of Activity')
         
# pie(AverageActivityMinutes$AvgMins, labels = AverageActivityMinutes$Activitytype, 
#     edges = 200, radius = 0.8)

#Average Steps by hour 
AverageHourlySteps <- hourly_steps %>% select(Time, StepTotal) %>% 
  group_by(Time) %>% summarise(mean(StepTotal))  
AverageSteps <- daily_activity %>% select(Id, TotalSteps) %>% 
  group_by(Id) %>% summarise(mean(TotalSteps))
AverageHourlySteps <- AverageHourlySteps %>% rename("AvgSteps" = 
"mean(StepTotal)")
AverageHourlySteps$AvgSteps <- round(AverageHourlySteps$AvgSteps, 1)
AverageHourlySteps <- AverageHourlySteps %>% 
  mutate(
    StepsIntensity = case_when(
      AvgSteps >= 500 ~ "High Intensity Hours",
      AvgSteps < 100 ~ "Sleeping Hours",
      TRUE ~ "Low Intensity Hours"
    )
  )

AverageHourlySteps$Time <- as.character(AverageHourlySteps$Time)

ggplot(data = AverageHourlySteps) + geom_col(aes(x = Time, y = AvgSteps,
       fill = StepsIntensity)) + theme(axis.text.x = element_text(angle = 90, 
       vjust = 0.5, hjust=1)) + labs(title = "Average Steps by Hour") + 
       xlab("Time of the Day") + ylab("Average Steps") + 
       scale_fill_discrete(name = "Hours")

#Average Minutes of Sleep on Each Day of the Week
ggplot(data = sleep, aes(x = factor(Weekday, days_of_the_week), 
       y = TotalMinutesAsleep)) + geom_bar(stat = "summary", fun = "mean") + 
       labs(title = "Average Minutes of Sleep on Each Day of The Week") + 
       xlab("Day of the Week") + ylab("Average Minutes of Sleep") 

#Average Sedentary minutes on Each Day of the Week
ggplot(data = daily_activity, aes(x = factor(Weekday, days_of_the_week),
       y = SedentaryMinutes)) + geom_bar(stat = "summary", fun = "mean") + 
       labs(title = "Average Sedentary Minutes on Each Day of The Week") + 
       xlab("Day of the Week") + ylab("Average Sedentary Minutes") 

SedentaryMinutes <- daily_activity %>% select(Weekday, SedentaryMinutes) %>%
  group_by(Weekday) %>% summarise(mean(SedentaryMinutes)) 
SedentaryMinutes <- SedentaryMinutes %>% 
  rename("AvgSedentaryMins" = "mean(SedentaryMinutes)")
SedentaryMinutes$AvgSedentaryMins <- round(SedentaryMinutes$AvgSedentaryMins, 0)

SleepingMinutes <- sleep %>% select(Weekday, TotalMinutesAsleep) %>% 
  group_by(Weekday) %>% summarise(mean(TotalMinutesAsleep))
SleepingMinutes <- SleepingMinutes %>% 
  rename("AvgSleepingMins" = "mean(TotalMinutesAsleep)")
SleepingMinutes$AvgSleepingMins <- round(SleepingMinutes$AvgSleepingMins, 0)

merge_sleep_sedentary <- merge(SleepingMinutes, SedentaryMinutes, 
                               by = "Weekday")
merge_sleep_sedentary <- merge_sleep_sedentary %>% mutate(Total_Mins = 1440)
merge_sleep_sedentary_long <- gather(merge_sleep_sedentary, key = "SleepSed", 
                                     value = "Minutes", 2:4)
merge_sleep_sedentary_long <- merge_sleep_sedentary_long %>% 
  mutate(Percentage = Minutes * 100 / 1440)
merge_sleep_sedentary_long$Percentage <- 
  round(merge_sleep_sedentary_long$Percentage, 1) 

ggplot(data = merge_sleep_sedentary_long, aes(x = factor(Weekday, 
       days_of_the_week), y = Minutes, fill = SleepSed)) + 
       geom_bar(position="dodge", stat="identity") + geom_text(aes(label = 
       Minutes), position = position_dodge(0.9)) + xlab("Day of the Week") + 
       scale_fill_discrete(name = "Legend", breaks = c("Total_Mins", 
       "AvgSedentaryMins", "AvgSleepingMins"), labels = c("Total Minutes", 
       "Avg. Sedentary Mins", "Avg. Sleeping Mins")) + labs(title = "Sleeping 
       Minutes vs. Sedentary Minutes Relative to Total Minutes") + 
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
       
       