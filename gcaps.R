library(tidyverse) 
library(lubridate) 
library(hms)
library(data.table)
m1<-read_csv("202302-divvy-tripdata.csv")
m2<-read_csv("202301-divvy-tripdata.csv")
m3<-read_csv("202203-divvy-tripdata.csv")
m4<-read_csv("202204-divvy-tripdata.csv")
m5<-read_csv("202205-divvy-tripdata.csv")
m6<-read_csv("202206-divvy-tripdata.csv") 
m7<-read_csv("202207-divvy-tripdata.csv")
m8<-read_csv("202208-divvy-tripdata.csv")
m9<-read_csv("202209-divvy-publictripdata.csv")
m10<-read_csv("202210-divvy-tripdata.csv")
m11<-read_csv("202211-divvy-tripdata.csv")
m12<-read_csv("202212-divvy-tripdata.csv")

cyclistic_df<- rbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)
rm(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)

cyclistic_mod<- cyclistic_df
cyclistic_mod$ride_length= difftime(cyclistic_mod$ended_at,cyclistic_mod$started_at,units="min")
View(cyclistic_mod)
drop_na(cyclistic_mod)

##-------------------processing--------------------
cyclistic_mod$year <- as.numeric(format(cyclistic_mod$started_at, "%Y"))
cyclistic_mod$month <- as.numeric(format(cyclistic_mod$started_at, "%m"))
cyclistic_mod$day <- as.numeric(format(cyclistic_mod$started_at, "%D"))
cyclistic_mod$hour <- as.numeric(format(cyclistic_mod$started_at, "%H")) 
cyclistic_mod$day <- weekdays(as.Date(cyclistic_mod$started_at))
cyclistic_mod <-cyclistic_mod %>% mutate(time_of_day = 
                                            case_when(hour == "0" ~ "Night",
                                                        hour == "1" ~ "Night",
                                                        hour == "2" ~ "Night",
                                                        hour == "3" ~ "Night",
                                                        hour == "4" ~ "Night",
                                                        hour == "5" ~ "Night",
                                                        hour == "6" ~ "Morning",
                                                        hour == "7" ~ "Morning",
                                                        hour == "8" ~ "Morning",
                                                        hour == "9" ~ "Morning",
                                                        hour == "10" ~ "Morning",
                                                        hour == "11" ~ "Morning",
                                                        hour == "12" ~ "Afternoon",
                                                        hour == "13" ~ "Afternoon",
                                                        hour == "14" ~ "Afternoon",
                                                        hour == "15" ~ "Afternoon",
                                                        hour == "16" ~ "Afternoon",
                                                        hour == "17" ~ "Afternoon",
                                                        hour == "18" ~ "Evening",
                                                        hour == "19" ~ "Evening",
                                                        hour == "20" ~ "Evening",
                                                        hour == "21" ~ "Evening",
                                                        hour == "22" ~ "Evening",
                                                        hour == "23" ~ "Evening")
)
## -------------------cleaning the data------------
cyclistic_mod <- na.omit(cyclistic_mod) #remove rows with NA values
str(cyclistic_mod)#check formatting
cyclistic_mod <- distinct(cyclistic_mod)#removing duplicates
cyclistic_mod <- cyclistic_mod[!(cyclistic_mod$ride_length <=0),] #remove negative ride_length
#-------------------analysis---------------------------------------
#------------------member/casual statistics------------------------------
total_cm<- cyclistic_mod %>% 
  group_by(member_casual) %>% 
  count(member_casual)
total_cm
#-------------------no of rides------------------------------------------
#total rides
total_r<-nrow(cyclistic_mod)
total_r

#total rides with casuals/members
total_rcm<- cyclistic_mod %>% 
  group_by(member_casual) %>% 
  count(member_casual)
(total_rcm)

#total ride with time of day
total_rt<- cyclistic_mod %>% 
  group_by(time_of_day) %>% 
  count(time_of_day)
total_rt


#total ride with hour
total_rh<- cyclistic_mod %>% 
  group_by(hour) %>% 
  count(hour) 
  
total_rh
#total rides with days
total_rd<- cyclistic_mod %>% 
  group_by(day) %>% 
  count(day)
(total_rd)

#total rides with months
total_rm<- cyclistic_mod %>% 
  group_by(month) %>% 
  count(month)
arrange(total_rm)



#------------------average ride times----------------------------
#time with members
total_tcm<- cyclistic_mod %>% 
  group_by(member_casual) %>% 
  summarize(mean(ride_length))
(total_tcm)

#time with time of day
total_tt<-cyclistic_mod %>% 
  group_by(time_of_day) %>% 
  summarize(mean(ride_length))
(total_tt)

#time with hour
total_th<-cyclistic_mod %>% 
  group_by(hour) %>% 
  summarize(mean(ride_length))
(total_th)

#time with day
total_td<-cyclistic_mod %>% 
  group_by(day) %>% 
  summarize(mean(ride_length))
(total_td)

#time with month
total_tm<- cyclistic_mod %>% 
  group_by(month) %>% 
  summarize(mean(ride_length))
(total_tm)


#---------------------rideable type statistics -------------------------------
total_types<-cyclistic_mod %>% 
  group_by(rideable_type) %>% 
  count(rideable_type)
(total_types)

total_typescm<-cyclistic_mod %>% 
  group_by(rideable_type,member_casual) %>% 
  summarize(mean(ride_length))
(total_typescm)

#----------------------station stats---------------------------------------
#max start stations
station_startmax <- cyclistic_mod %>%
  group_by(start_station_name) %>% 
  summarize(start_count = n()) %>%
  arrange(desc(start_count)) %>%
  head()
station_startmax
#max end stations
station_endmax<- cyclistic_mod %>% 
  group_by(end_station_name) %>% 
  summarize(end_count = n()) %>%
  arrange(desc(end_count)) %>%
  head()
station_endmax

#casuals and stations
station_casual <- cyclistic_mod %>%
  group_by(start_station_name) %>%
  filter(member_casual=='casual') %>% 
  summarize(start_count = n()) %>%
  arrange(desc(start_count))
station_casual

#members and stations+
station_member<-cyclistic_mod %>%
  group_by(start_station_name) %>%
  filter(member_casual=='member') %>% 
  summarize(start_count = n()) %>%
  arrange(desc(start_count))
station_member

#--------------storing in csv file for tableau------------
fwrite(cyclistic_mod,"cyclistic1.csv")

