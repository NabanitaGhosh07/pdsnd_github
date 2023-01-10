
#--------------------------------Udacity Bike Share Data------------------------------------
setwd("~/LEARNING/Udacity/R Project")
#Reading the 3 city files into R
ny <- read.csv('C:/Users/NGhosh3/OneDrive - JNJ/Documents/LEARNING/Udacity/R Project/new-york-city.csv')
wash = read.csv('C:/Users/NGhosh3/OneDrive - JNJ/Documents/LEARNING/Udacity/R Project/washington.csv')
chi = read.csv('C:/Users/NGhosh3/OneDrive - JNJ/Documents/LEARNING/Udacity/R Project/chicago.csv')


library(lubridate)
library(ggplot2)
#install.packages(“dplyr”)
library(dplyr)


#Combining the 3 datasets
#Creating 3 new columns Gender,BirthYear & City in Washington. Gender & BirthYear have no values. 
#City will help to distinguish Datasets once union-ed together
#Renaming 1st Column Name to ID instead of Blank

Gen=""
Birth.Yr=""
City="Washington"
wash$Gender <-Gen
wash$Birth.Year <-Birth.Yr
wash$City <- City
colnames(wash)[1]="ID"
head (wash)

City="NYC"
ny$City <-City
colnames(ny)[1]="ID"
head(ny)

City="Chicago"
chi$City <-City
colnames(chi)[1]="ID"
head(chi)

total <- rbind( ny,chi,wash)
head(total)
summary(total)

#Converting Start.Time & End.Time from character data type to date time format
total$Start.Time=ymd_hms(total$Start.Time)
total$End.Time=ymd_hms(total$End.Time)

#total$Start.Time1<-format(as.Date(total$Start.Time, format="%Y-%m-%d %H:%M:%S")) -Another method to convert string to date
typeof(total$Start.Time)

total$Start.Time.Year=as.integer(year(total$Start.Time))
total$Start.Time.Year
typeof(total$Start.Time.Year) #Check to see if data type changed correctly

total$Start.Time.Mth=as.integer(month(total$Start.Time))
total$Start.Time.Mth
typeof(total$Start.Time.Mth) #Check to see if data type changed correctly

total<- total[,-13] # removing a column 

total$Start.Time.Wkday <- wday(total$Start.Time,label=TRUE)
total$Start.Time.wkday
typeof(total$Start.Time.wkday) #Check to see if data type changed correctly

total$Start.Time.Hour=as.integer(hour(total$Start.Time))
total$Start.Time.Hour
typeof(total$Start.Time.Hour) #Check to see if data type changed correctly

#1) POPULAR TIMES OF TRAVEL-
#a) What is the most common month? - Creating a bar graph from the month column based on it's count
#Observation - June is the month where most travel has started

common_mth<- total %>% group_by(City,Start.Time.Mth) %>% summarize(count_month=n())
print(common_mth)

ggplot(aes(x=Start.Time.Mth, fill=City), data=total) +
  geom_bar(position='dodge') + labs(x="MonthNum", y="Count of rides", title="Most Common Month") +
  geom_text(aes(label = ..count..), stat = "count", position=position_dodge(width=1), vjust=0.3, hjust=1.25,angle=90)


#b) What is the most common day of the week
#Observation - In Chicago, Tuesday was most common day for travel; & Wednesday was the most common day for NYC & Washington

common_day<- total %>% group_by(City,Start.Time.Wkday) %>% summarize(count_day=n())%>% arrange(-count_day) 
print(common_day)

ggplot(aes(x=Start.Time.Wkday, fill=City), data=total) +
  geom_bar(position='dodge') + labs(x="Weekday", y="Count of rides", title="Most Common Day of the week") +
  geom_text(aes(label = ..count..), stat = "count", position=position_dodge(width=1), vjust=0.5, hjust=1.25, angle=90)


#c) What is the most common hour of the day
#Observation - In Chicago & NYC, Evening 5pm or 1700HRS was most common time for travel start; & Morning 8am or 0800 HRS was the most common time in Washington
common_hr<- total %>% group_by(City,Start.Time.Hour) %>% summarize(count_hr=n())%>% arrange(-count_hr) %>% slice(1:3)
print(common_hr)

ggplot(aes(x=Start.Time.Hour, fill=City), data=total) +
  geom_bar(position='dodge',just=0.5) + labs(x="Hour of day", y="Count of rides", title="Most Common Hour of Day") +
  scale_x_continuous(breaks=seq(0,23,1))

#2 Popular stations and trip

# a) What is the most common start station?
# Observation - Among all cities the most common start station was Streeter Dr & Grand Ave from Chicago. However, within individual cities,
# Chicago= Streeter Dr & Grand Ave, NYC = Pershing Square North, Washington = Columbus Circle / Union Station

#Method 1:
common_Start_Station<- total %>% group_by(City,Start.Station) %>% summarize(count_station=n()) %>% arrange(-count_station) %>% slice(1:5) %>% mutate(Start.Station = factor(Start.Station, Start.Station))
print(common_Start_Station)

ggplot(common_Start_Station, aes(y=Start.Station, x=count_station, color=City, fill=City)) + 
  geom_bar(stat = "identity")  + labs(x="No. of times used", y="Start Station", title="Most Common Start Station") 


#Method 2:
common_Start_Station<- total %>% group_by(City,Start.Station) %>% summarize(count_station=n()) %>% arrange(-count_station) %>% slice(1:5) %>% mutate(Start.Station = factor(Start.Station, Start.Station))
print(common_Start_Station)

ggplot(common_Start_Station, aes(x=Start.Station,y=count_station,color=City, fill=City)) + 
  geom_bar(stat = "identity")  + labs(x="Start Station", y="No. of times used", title="Most Common Start Station") +
  geom_text( aes(label = count_station),color="black",hjust = 1)+
  coord_flip() 


# b) What is the most common end station?
# Observation - Among all cities the most common end station was Streeter Dr & Grand Ave from Chicago. However, within individual cities,
# Chicago= Streeter Dr & Grand Ave, NYC = Pershing Square North, Washington = Columbus Circle / Union Station

data_mod<- total %>% group_by(City,End.Station) %>% summarize(count_station=n()) %>% arrange(-count_station) %>% slice(1:5) %>% mutate(End.Station = factor(End.Station, End.Station))
print(data_mod)

ggplot(data_mod, aes(y=End.Station, x=count_station, color=City, fill=City)) + 
  geom_bar(stat = "identity")  + labs(x="No. of times used", y="End Station", title="Most Common End Station")

#Method 2:
common_End_Station<- total %>% group_by(City,End.Station) %>% summarize(count_station=n()) %>% arrange(-count_station) %>% slice(1:5)%>% mutate(End.Station = factor(End.Station, End.Station))
print(common_End_Station)

ggplot(common_End_Station, aes(x=End.Station,y=count_station,color=City, fill=City)) + 
  geom_bar(stat = "identity")  + labs(x="End Station", y="No. of times used", title="Most Common End Station") +
  geom_text( aes(label = count_station),color="black",hjust = 1)+
  coord_flip() 

# c) What is the most common trip from start to end (i.e., most frequent combination of start station and end station)?
# Observation - Among all cities, the most common trip is from Lake Shore Dr & Monroe St to Streeter Dr & Grand Ave in Chicago
start_end<- total %>% group_by(City,Start.Station,End.Station) %>% summarize(count_station=n()) %>% arrange(desc(count_station)) 
print(start_end)

# Make Combo and then create the graph

#3 Trip duration

# a) What is the total travel time for users in different cities?
# Observation - In this question I have split the response in 2: 
# i) the total travel time for all users in different cities, total trip duration summed up in seconds and in days
data_mod<- total %>% group_by(City) %>% summarize(Total_Trip_dur_sec=sum(Trip.Duration), Total_Trip_dur_day=sum(Trip.Duration)/(24*60*60))
print(data_mod)
# ii)the total travel time for different user types in different cities, total trip duration summed up in seconds and in days
data_mod<- total %>% group_by(User.Type,City) %>% summarize(Total_Trip_dur_sec=sum(Trip.Duration), Total_Trip_dur_day=sum(Trip.Duration)/(24*60*60))
print(data_mod)
# b) What is the average travel time for users in different cities?
# Observation - In this question I have split the response in 2: 
# i) the average travel time for all users in different cities, Average trip duration in seconds and in days
data_mod<- total %>% group_by(City) %>% summarize(Avg_Trip_dur_sec=mean(Trip.Duration), Avg_Trip_dur_hrs=mean(Trip.Duration)/(60))
print(data_mod)
# ii)the average travel time for different user types in different cities, Average trip duration in seconds and in days
data_mod<- total %>% group_by(User.Type,City) %>% summarize(Avg_Trip_dur_sec=mean(Trip.Duration), Avg_Trip_dur_day=mean(Trip.Duration)/(24*60*60))
print(data_mod)

#4 User info

# a)  What are the counts of each user type?
# Observation - In this question I have split the response in 2: 
# i) The overall counts of each user type across all cities
user_counts_all <-total %>% group_by(User.Type) %>% summarize(counts=n()) %>% arrange(desc(counts))
print(user_counts_all)

ggplot(user_counts_all, aes(y=counts, x=User.Type, color=User.Type, fill=User.Type)) +
  geom_bar(stat = "identity")  + labs(x="User Type", y="Counts", title="Counts of User Types")+
  geom_text(aes(label = counts), position=position_dodge(width=1), vjust=1, hjust=1, color='black', angle=90)

# ii) The counts of each user type for individual cities
user_counts <-total %>% group_by(City,User.Type) %>% summarize(counts=n()) %>% arrange(desc(counts))
print(user_counts)

ggplot(user_counts, aes(y=counts, x=User.Type, color=User.Type, fill=User.Type)) +
  geom_bar(stat = "identity")  + labs(x="User Type", y="Counts", title="Counts of User Types") + 
  geom_text(aes(label = counts), position=position_dodge(width=1), vjust=1, hjust=1, color='black',angle=90)+
  facet_grid(~City)


# b)  What are the counts of each gender (only available for NYC and Chicago)?
# Observation - In this question I have split the response in 2: 
# i) The overall counts of each gender across all cities
gender_counts <-total %>% group_by(Gender) %>% summarize(counts=n()) %>% arrange(desc(counts))
print(gender_counts)

ggplot(gender_counts, aes(y=counts, x=Gender, color=Gender, fill=Gender)) +
  geom_bar(stat = "identity")  + labs(x="Gender", y="Counts", title="Overall Counts per Gender Type")+
  geom_text(aes(label = counts), position=position_dodge(width=1), vjust=1, hjust=1, color='black',angle=90)

# ii) The counts of each gender for individual cities
gender_counts <-total %>% group_by(City,Gender) %>% summarize(counts=n()) %>% arrange(desc(counts))
print(gender_counts)

ggplot(gender_counts, aes(y=counts, x=Gender, color=Gender, fill=Gender)) +
  geom_bar(stat = "identity")  + labs(x="Gender", y="Counts", title="Counts for Gender Type by City") +
  geom_text(aes(label = counts), position=position_dodge(width=1), vjust=1, hjust=1, color='black',angle=90)+
  facet_grid(~City)

# c)  What are the earliest, most recent, most common year of birth (only available for NYC and Chicago)?
cat("The earliest year of birth in the dataset is :" , min(as.integer(total$Birth.Year),na.rm=TRUE))
cat("The most recent year of birth in the dataset is :" , max(as.integer(total$Birth.Year),na.rm=TRUE))
recent_year <-total %>% group_by(Birth.Year) %>% summarize(counts=n()) %>% arrange(desc(counts))
cat("The most common year of birth in the dataset is :" , recent_year[3,1]%>% pull())
