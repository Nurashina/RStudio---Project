library(tidyverse)
library(dplyr)
library(readr)
# Import data
netflix_titles <- read_csv("C:/Users/Nurashina/Desktop/Data Analyst/Portfolio project/netflix_titles.csv")
View(netflix_titles)
#Explore dataset
head(netflix_titles)
colnames(netflix_titles)
## Found 14 unnessecary column(empty column), need to remove
netflix_titles <- netflix_titles %>% 
  select(-...13, -...14, -...15, -...16, -...17, -...18, -...19, -...20, -...21, -...22, -...23, -...24,
         -...25, -...26)
## Checking the data after removing empty column
colnames(netflix_titles)
## checking null values in each column
na_count <- sapply(netflix_titles, function(x) sum(is.na(x)))
print(na_count)
## replace director, cast and country with unknown
netflix_titles <- netflix_titles %>%
  mutate(
    director = ifelse(is.na(director), "Unknown", director),
    cast = ifelse(is.na(cast), "Unknown", cast),
    country = ifelse(is.na(country), "Unknown", country)
  )
View(netflix_titles)
## generating random date in date added column based on release year
## extract release year where date_added is NA
a <- netflix_titles %>%
  filter(is.na(date_added)) %>%
  pull(release_year)
# replace NA values in date_added with generated dates
set.seed(42)
for (i in seq_along(a)) {
  if (i > length(a)) break
  # generate random date for replacement
  month<- sample(1:12, 1)
  day <- sample(1:28, 1)
  replacement_date <- sprintf("%d-%02d-%02d", a[i], month, day)
  netflix_titles$date_added[is.na(netflix_titles$date_added)] <- replacement_date
  
}
print(netflix_titles)
# Fill NA value in rating column with predefined values from a list
val <- c("R", "TV-MA", "TV-MA", "R", "TV-MA", "R", "TV-MA")
for (i in seq_len(sum(is.na(netflix_titles$rating)))) {
  if (i > length(val)) break
  netflix_titles$rating[which(is.na(netflix_titles$rating))[i]] <- val[i]
}
print(netflix_titles)
#Fill NA value in duration column with predefined values from a list
val <- c("74 min", "84 min", "66 min", "80 min")
# Step 1: Replace 'rating' with 'PG-13' where 'duration' is NA
netflix_titles <- netflix_titles %>%
  mutate(rating = ifelse(is.na(duration), "PG-13", rating))

# Step 2: Replace NA values in 'duration' with values from 'val'
for (i in seq_len(sum(is.na(netflix_titles$duration)))) {
  if (i > length(val)) break
  netflix_titles$duration[which(is.na(netflix_titles$duration))[i]] <- val[i]
}
print(netflix_titles)
# check if there is any null value left
na_count <- sapply(netflix_titles, function(x) sum(is.na(x)))
print(na_count)
# check the data type
str(netflix_titles)
# change date added data type from chr to date format
netflix_titles$date_added <- parse_date_time(netflix_titles$date_added, orders = c("ymd", "mdy", "dmy", "B d, Y"))
str(netflix_titles)
write.csv(netflix_titles, "netflix_titles_cleaned.csv", row.names = FALSE)
# Data visualization
# Distribution based on content type
library(ggplot2)
ggplot(data = netflix_titles) +
  geom_bar(mapping = aes(x = type, fill = type)) +
  labs(x = "Type", y = "Count", title = "Distribution of Content by Type") +
  geom_text(stat = "count", aes(x = type, label = ..count..), vjust = -0.2, color = "black") +
  theme_minimal()
# Distribution based on rating
netflix_titles <- netflix_titles %>%
  mutate(rating = factor(rating, levels = names(sort(table(rating), decreasing = TRUE))))
ggplot(data = netflix_titles) +
  geom_bar(mapping = aes(x = rating, fill = rating)) +
  labs(x = "Rating", y = "Count", title = "Distribution based on Rating") +
  geom_text(stat = "count", aes(x = rating, label = ..count..), vjust = -0.2, color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# Top 10 country produce movies
# Filter the data to include only movies
movies <- netflix_titles %>% 
  filter(type == "Movie") %>% 
  filter(country != "Unknown")

# Count the number of movies produced by each country
country_counts <- movies %>% 
  separate_rows(country, sep = ", ") %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Select the top 10 
top_10_countries <- country_counts %>% top_n(10, count)

# Create the bar chart
ggplot(top_10_countries, aes(x = reorder(country, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Countries Producing Movies on Netflix",
       x = "Country",
       y = "Number of Movies") + 
  geom_text(aes(label = count), vjust = -0.3, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Top 10 countries countries produced TV Show
# Filter the data to include only movies
TV_Show <- netflix_titles %>% 
  filter(type == "TV Show") %>% 
  filter(country != "Unknown")

# Count the number of TV Show produced by each country
country_counts <- TV_Show %>% 
  separate_rows(country, sep = ", ") %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Select the top 10 
top_10_countries <- country_counts %>% top_n(10, count)

# Create the bar chart
ggplot(top_10_countries, aes(x = reorder(country, -count), y = count)) +
  geom_bar(stat = "identity", fill = "maroon") +
  labs(title = "Top 10 Countries Producing TV Shows on Netflix",
       x = "Country",
       y = "Number of TV Show") + 
  geom_text(aes(label = count), vjust = -0.3, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Select the top 10 genres
# Separate multiple genres into individual rows and count the number of titles for each genre
genre_counts <- netflix_titles %>%
  separate_rows(listed_in, sep = ", ") %>%
  group_by(listed_in) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
top_10_genres <- genre_counts %>% top_n(10, count)
# Create the bar chart with data labels
ggplot(top_10_genres, aes(x = count, y = reorder(listed_in, count))) +
  geom_bar(stat = "identity", aes(fill = count), show.legend = FALSE) +
  geom_text(aes(label = count), hjust = -0.3, size = 3) +
  labs(title = "Top 10 Genres on Netflix",
       x = "Count",
       y = "Genre") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_fill_gradient(low = "lightblue", high = "steelblue")
