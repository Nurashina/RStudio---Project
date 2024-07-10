library(tidyverse) #helps wrangle data
#use the conflicted package to manage conflicts
library(conflicted)
#set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#STEP1: Collect Data
q1_2019 <- read.csv("C:/Users/Nurashina/Desktop/Data Analyst/Capstone project/Divy trips/Divy_trips_2019_q1/Divvy_Trips_2019_Q1.csv")
q1_2020 <- read.csv("C:/Users/Nurashina/Desktop/Data Analyst/Capstone project/Divy trips/Divvy_Trips_2020_Q1/Divvy_Trips_2020_Q1.csv")

#STEP 2: Wrangle Data and Combine Into Single File
#Compare column names if each files
# While the names don't have to be in the same order, they DO need to match perfectly before
# we can use a command to join them into one file
colnames(q1_2019)
colnames(q1_2020)
# Rename columns to make them consistent with q1_2020 (as this will be the supposed
# going-forward table design for Divvy)
(q1_2019 <- rename(q1_2019,
                   ride_id = trip_id,
                   rideable_type = bikeid,
                   started_at = start_time,
                   ended_at = end_time,
                   start_station_name = from_station_name,
                   start_station_id = from_station_id,
                   end_station_name = to_station_name,
                   end_station_id = to_station_id,
                   member_casual = usertype))
# Inspect the dataframes and look for incongruencies
str(q1_2019)
str(q1_2020)

#convert ride_id and rideable type to character so that they can stack correctly
q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

#Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q1_2019, q1_2020)

#remove lat, long, birthyear and gender fields as this data was dropped beginning
# in 2020 
all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear,
            gender, "tripduration"))

# STEP 3: Clean Up and Add Data to Prepare For Analysis
# Inspect the new table has been created
colnames(all_trips) # list of column names
nrow(all_trips) # number of rows
dim(all_trips) # dimension of data frames, column and row
head(all_trips) # see first 6 rows
tail(all_trips) # see last rows
str(all_trips) # list of column and data types
summary(all_trips) # statistical summary of data

#Problems need to fix

# 1- In the "member_casual" column, there are two names for members ("member" and
"Subscriber") 
# and two names for casual riders ("Customer" and "casual"). We will need to
# consolidate that from four to two labels

# 2- The data can only be aggregated at the ride-level, which is too granular. We will want to
# add some additional columns of data -- such as day, month, year -- that provide additional
# opportunities to aggregate the data.

# 3- We will want to add a calculated field for length of ride since the 2020Q1 data did not have
# the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency

# 4- There are some rides where tripduration shows up as negative, including several hundred
# rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to
# delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with
"casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make
# our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not
# contain any values from a specific level
# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)
#Reassign to the desired values (go with the current 2020 labels)
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual, "Subscriber" = "member", "Customer" = "casual"))
#check to makesure the proper number of observation were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing
# these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link

all_trips$date <- as.Date(all_trips$started_at) #the default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html

all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

#inspect the structure of column
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and
# checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ HR" | all_trips$ride_length<0),]

# STEP 4: Conduct Descrptive Analysis
# Descriptive analysis on ride_length (all figures in second)

mean(all_trips_v2$ride_length) #straight average (total ride length/seconds)
median(all_trips_v2$ride_length) # midpoint number in the ascending array of ride length
max(all_trips_v2$ride_length) # longest ride 
min(all_trips_v2$ride_length) # shortest ride

# condense using summary

summary(all_trips_v2$ride_length)

# Compare members and casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#see the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#analyze rideship data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% # creates weekday field using wday()
  group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n (), # calculate the number of rides and average duration
            average_duration = mean(ride_length)) %>% # calculated the average duration
  arrange(member_casual, weekday) #sort

#Visualize number of rides by rider type

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#visualization for average duration

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


