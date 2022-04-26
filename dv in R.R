Sys.setlocale("LC_TIME", "English")
#if you don't have those packages, you could install the packages by this command 
#install.packages(c("tidyverse","ggplot2","geosphere","scales", "ggfortify"))

library(tidyverse)
library(ggplot2)
library(geosphere)
library(ggmap)
library(scales)
library(ggfortify)
library(readr)

#Declare the data_year(4 digs), data_month(2 digs) as string, concat those into date_path (6 digs), this will be used in our file name.
# Using for-loop to load the CSVs into the data set
# The parameter "temp" is the current file in for-loop, "data" will be the merged data set in this case study.

data <- read.csv(paste0("~/GitHub/DV in R/202112-divvy-tripdata.csv"))

for(i in 1:11){
  if (i<=2){
    data_year <-"2022" 
  } else{
    data_year <- "2021"
  }
  
  data_month <- str_pad(i,2, side="left","0")
  date_path <- paste0(data_year,data_month)
  
  temp <- read.csv(paste0("~/GitHub/DV in R/202112-divvy-tripdata.csv"))
  data <- bind_rows(data, temp)
}

head(data)

# Exclude the NA from our data set
data <- data %>% 
  drop_na()

# Convert "started_at", "ended_at" columns form string to datetime and create a new columns "ride_time"
data$started_at <- as.POSIXct(data$started_at, "%Y-%m-%d %H:%M:%S", tz="UTC")
data$ended_at <- as.POSIXct(data$ended_at, "%Y-%m-%d %H:%M:%S", tz="UTC")
data$ride_time <- format(data$started_at, "%H")

# Create new columns "ride_date","date_of_week", "ride_date" will be the primary key of this table
data <- data %>% 
  mutate(ride_date=as.Date(data$started_at, "%Y-%m-%d"), date_of_week=format(data$started_at, "%a"))
data$date_of_week <- factor(data$date_of_week,levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# Create a new column "ride_distance" to present the user's riding distances
data$ride_distance <- distGeo(matrix(c(data$start_lng, data$start_lat), ncol = 2), matrix(c(data$end_lng, data$end_lat), ncol = 2))
data$ride_distance <- data$ride_distance/1000

# Create a new column "ride_min" to present the user's riding durations
data <- data %>% 
  mutate(ride_min=difftime(ended_at, started_at, units="mins"))
data$ride_min <- as.numeric(data$ride_min, "mins")

# Create a "ride_speed" column and reset the format of "ride_distance", "ride_min" and "ride_speed" as float
data$ride_speed <- data$ride_distance/(data$ride_min/60)
data$ride_speed <- data$ride_speed %>% round(2)
data$ride_distance <- data$ride_distance %>% round(2)
data$ride_min <- data$ride_min %>% round(2)

summary(data)

data <- data %>% filter(ride_distance>0 & ride_distance<mean(ride_distance)+3*sd(ride_distance)) %>% 
  filter(ride_min>0 & ride_min<mean(ride_min)+3*sd(ride_min))%>% 
  filter(ride_speed>0 & ride_speed<mean(ride_speed)+3*sd(ride_speed)) %>% 
  filter(rideable_type!="docked_bike")

head(data)

# Select the ride_date, member_casual, rideable_type, ride_id into the new data set called "daily-summary"
daily_summary <- data %>% 
  select(ride_date, member_casual, ride_id)

# Count the "ride_id" aggregated to get the daily num of ride in each kind of user types and ride types
daily_summary <- group_by(daily_summary, ride_date, member_casual) %>% 
  summarise("num_of_ride"=n(),.groups = "drop")

head(daily_summary)

rainfall_raw <- read.csv(paste0("~/GitHub/DV in R/rainfall.csv"))

head(rainfall_raw)

# Create the "ride_date" columns as the primary key of this table, that we will use "ride_date" to join other data
rainfall_raw <- rainfall_raw %>% 
  mutate(ride_date=substring(valid,1,10))

# Replace "M" and NA values of columns "p01m" into 0
# ("M" means the missing data)
rainfall_raw$p01m <- gsub("M", 0, rainfall_raw$p01m) %>% 
  as.numeric(rainfall_raw$p01m)
rainfall_raw$p01m[is.na(rainfall_raw$p01m)] <- 0

# Sum the "p01m" to get the daily rainfall data and rename as "rainfall_mm"
rainfall_cleaned <- group_by(rainfall_raw, ride_date) %>% 
  summarise(rainfall_mm=sum(p01m),.groups = "drop")

rainfall_cleaned$ride_date <- as.Date(rainfall_cleaned$ride_date, "%Y-%m-%d")

head(rainfall_cleaned)

# load COVID-19 data
covid_19_raw <- read.csv(paste0("~/GitHub/DV in R/COVID-19_Daily_Cases.csv"))

head(covid_19_raw)

#Reset the "lab_report_date" into datetime
covid_19_raw$lab_report_date <- covid_19_raw$lab_report_date %>% as.Date("%Y-%m-%d")

#Select the data between Mar. 2021 and Feb. 2022 and rename "lab_report_date" as "ride_date", this will be the PK of this table
covid_19_daily <- covid_19_raw %>% select(lab_report_date, cases_total, hospitalizations_total, deaths_total) %>% 
  filter(lab_report_date>='2021-03-01'&lab_report_date<='2022-02-28') %>% 
  rename(ride_date=lab_report_date)

head(covid_19_daily)

#Using "ride_date" as PK to join data
daily_summary <- daily_summary %>% 
  left_join(rainfall_cleaned, by="ride_date") %>% 
  left_join(covid_19_daily,by="ride_date")

head(daily_summary)

#Data Analysis

summary(data)
summary(daily_summary)

ggplot(daily_summary, aes(x=ride_date, y=num_of_ride))+geom_line(aes(color =member_casual))+
  facet_grid(~member_casual) + theme(legend.position="top") + 
  labs(title="c1. Bike Usage Growth", x="Date", y="Times") + 
  theme(plot.title = element_text(hjust = 0.5))

# [position="fill"] help us to convert bar chart into percentage bar chart
# [scale_y_continuous] could re-scale the y axis by using parameters "unit" and "scale"
ggplot(data) +
  geom_bar(aes(x=ride_date, fill=member_casual), position = "fill") + 
  scale_x_date(date_breaks = "1 month",date_labels = "%Y %b",expand = c(0,0)) +
  scale_y_continuous(labels = unit_format(unit = "%", scale = 1e+2)) +
  labs(title="c2. Distribution of User Type", x="Date", y="%") + 
  theme(plot.title = element_text(hjust = 0.5))

# We could align our title center by using "hjust = 0.5"
ggplot(data) + geom_bar(aes(x=member_casual, fill=rideable_type),position ="fill") +
  scale_y_continuous(labels = unit_format(unit = "%", scale = 1e+2)) +
  labs(title="c3. Bike Type Preference", x="User Type", y="%") + 
  theme(plot.title = element_text(hjust = 0.5))

#Group_by the data by "date_of_week","ride_time", "member_casual" as heatmap_pivot
#Tally() which could help us to count(ride_id) 
heatmap_pivot<- data %>% group_by(date_of_week, ride_time, member_casual) %>%
  tally()

#Using ggplot() to plot heatmap
#scale_fill_gradient could help us to customize the filling color
ggplot(heatmap_pivot,aes(x = date_of_week, y = ride_time, fill = n)) + 
  facet_wrap( ~member_casual) +
  geom_tile(colour = "white", size = 0.25) + 
  scale_y_discrete(limits=rev) +
  scale_fill_gradient(low="green", high="red")+
  labs(title="c4. Prime Time", x="Week Day", y="Time") + 
  theme(plot.title = element_text(hjust = 0.5))

#Group_by the data by "date_of_week","ride_time", "member_casual" as heatmap_pivot
#But this time we use mean() to get average of riding duration and riding distance
distance_duration_data <- data %>% group_by(ride_date, member_casual) %>%
  summarize(ride_min=mean(ride_min),ride_distance=mean(ride_distance))

ggplot(distance_duration_data)+geom_point(aes(x=ride_min, y=ride_distance, color=member_casual))+
  labs(title="c5. Riding Feature", x="Ride Duration", y="Ride Distance") + 
  theme(plot.title = element_text(hjust = 0.5))

#Remove duplicate of start station 
bike_station_1 <- data %>% select(station_id=start_station_id, 
                                  station_name=start_station_name, 
                                  station_lat=start_lat, 
                                  station_lng=start_lng) %>% 
  distinct(station_id, .keep_all=TRUE)

#Remove duplicate of end station 
bike_station_2 <- data %>% select(station_id=end_station_id, 
                                  station_name=end_station_name, 
                                  station_lat=end_lat, 
                                  station_lng=end_lng) %>% 
  distinct(station_id, .keep_all=TRUE)

#Union start station and end station, then remove duplicate again
bike_station <- union(bike_station_1, bike_station_2) %>% distinct(station_id, .keep_all=TRUE)

#Group data by "start_lng", "start_lat", "end_lng", "end_lat", "member_casual" and count each route's rode times.
route <- data %>% 
  filter(start_station_id != end_station_id & start_station_id != "" & end_station_id != "") %>%
  group_by(start_lng, start_lat, end_lng, end_lat, member_casual) %>%
  summarise(total = n(),.groups = "drop") %>% filter(total>300)

#Seperate data
casual_route <- route %>% filter(member_casual=="casual")
member_route <- route %>% filter(member_casual=="member")

#You could replace you google key under this commond.
register_google(key="AIz********kqCBx**7Q*************thiE")

#Casual route map
casual_route_map <- ggmap(get_googlemap(center=c(lon=-87.6197, lat=41.8888), zoom = 12, maptype="terrain", color="color"))+
  geom_point(data=bike_station, aes(x=station_lng, y=station_lat), size=0.8, color="orange") + 
  geom_curve(casual_route, mapping = aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, alpha= total), 
             color="purple", size = 1, curvature = 0.2, arrow = arrow(length=unit(0.3,"cm"), ends="first", type = "closed")) +
  coord_cartesian()+labs(title="c6. Riding Route of Casual", x="Longitude", y="Latitudee") + 
  theme(plot.title = element_text(hjust = 0.5))
print(casual_route_map)

#Member route map
member_route_map <- ggmap(get_googlemap(center=c(lon=-87.6197, lat=41.8888), zoom = 12, maptype="terrain", color="color"))+
  geom_point(data=bike_station, aes(x=station_lng, y=station_lat), size=0.8, color="orange") + 
  geom_curve(member_route, mapping = aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, alpha= total), 
             color="coral4", size = 1, curvature = 0.2, arrow = arrow(length=unit(0.3,"cm"), ends="first", type = "closed")) +
  coord_cartesian()+labs(title="c7. Riding Route of Member", x="Longitude", y="Latitudee") + 
  theme(plot.title = element_text(hjust = 0.5))
print(member_route_map)

#Seprate "daily_summary" into casual and member
casual<- daily_summary %>% filter(member_casual=='casual')
member<- daily_summary %>% filter(member_casual=='member')

#Calculate correlation between rainfall and casual users' riding times
print("t1. Correlation between Casual Riders' Usage and Rainfall")
cor.test(casual$rainfall_mm, casual$num_of_ride, method="pe", exact=FALSE)

#Calculate correlation between rainfall and annual members' riding times
print("t2. Correlation between Annual Members' Usage and Rainfall")
cor.test(member$rainfall_mm, member$num_of_ride, method="pe", exact=FALSE)

daily_summary %>% ggplot(aes(x=rainfall_mm, y=num_of_ride))+geom_point(aes(color=member_casual))+
  geom_smooth()+facet_wrap(~member_casual) +
  scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3)) +
  labs(title="c8. Daily Usages by Rainfall", x="Rainfall (mm)", y="Times") + 
  theme(plot.title = element_text(hjust = 0.5))


#Calculate correlation between COVID-19 daily case and casual users' riding times
print("t3. Correlation between Casual Riders' Usage and COVID-19 daily case")
cor.test(casual$cases_total, casual$num_of_ride, method="pe", exact=FALSE)

#Calculate correlation between COVID-19 daily case and annual members' riding times
print("t4. Correlation between Annual Members' Usage and COVID-19 daily case")
cor.test(member$cases_total, member$num_of_ride, method="pe", exact=FALSE)

daily_summary %>% ggplot(aes(x=cases_total, y=num_of_ride))+geom_point(aes(color=member_casual))+
  geom_smooth()+facet_wrap(~member_casual) +
  scale_x_continuous(labels = unit_format(unit = "k", scale = 1e-3)) +
  scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3)) +
  labs(title="c9. Daily Usages by COVID-19 Cases", x="COVID-19 Cases", y="Times") + 
  theme(plot.title = element_text(hjust = 0.5))

#Create regression modle to pridict casual users' riding times by COVID-19 daily cases.
covid_casual_lm <- lm(num_of_ride ~cases_total+log(cases_total) ,data = casual)
print("M1. Casual Users")
summary(covid_casual_lm)

#Create regression modle to pridict annual members' riding times by COVID-19 daily cases.
covid_member_lm <- lm(num_of_ride ~cases_total+log(cases_total) ,data = member)
print("M2. Annual Members")
summary(covid_member_lm)

autoplot(covid_casual_lm) +
  labs(title="c10. Residual analysis(casual rider)") + 
  theme(plot.title = element_text(hjust = 0.5))

autoplot(covid_member_lm) +
  labs(title="c11. Residual analysis(annual member)") + 
  theme(plot.title = element_text(hjust = 0.5))

