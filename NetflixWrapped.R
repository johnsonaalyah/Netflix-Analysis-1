library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)

#fetch dataset with all netflix titles and info about them
getwd()
setwd('C:/Users/Que/Desktop/NetflixAnalysis1')
alltitles <- read.csv("netflix_titles.csv")

allnftitles <- alltitles [, -c(1, 4, 5, 6, 7, 9, 12)]
#Created cleaned view a view with movie/tv show, title, date added to nf,
#release date, duration and tags


# READING DATA FROM CSV DOWNLOADED FROM NETFLIX ACCOUNT
getwd()
setwd('C:/Users/Que/Desktop/NetflixAnalysis1')
nfcsv <- read.csv("NetflixViewingHistory.csv")

#cleaned csv file from nf
nfhistory <- nfcsv%>%
  separate(col = Title, into = 
             c("title", "season", "episode_title"), sep = ': ')

nfhistory$Date <- as.Date(nfhistory$Date, format = "%d/%m/%Y")

#---------------------------------------------------------------------------------
#Most episodes in 1 day
marathons <- nfhistory%>%
  count(title, Date)

marathons <- marathons[order(marathons$n, decreasing = TRUE), ]
overallmostwatched <- marathons[1, ]



#most episodes in 1 day in current year
currentnf <- nfhistory %>%
  filter(year(Date) == 2023)
currentmarathons <- currentnf%>%
  count(title, Date)
currentmarathons <- currentmarathons[order(currentmarathons$n, 
                                           decreasing = TRUE), ]
currentmostwatched <- currentmarathons[1, ]
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
#total titles for year
totaltitles <- currentnf%>%
  count(title)
total_rows <- nrow(totaltitles)
print(total_rows)

# finding which are movies
movies <- currentnf[is.na(currentnf$season) 
                               & is.na(currentnf $episode_title),]
totalmovies <- nrow(movies)
print(totalmovies)

#and how many are series
totalseries <- total_rows - totalmovies
series <- anti_join(totaltitles, movies)
#-------------------------------------------------------------------------------

anftitles <- allnftitles [, -c(1,3)]
#Cleaned title view even more to only include title, duration and tags

#getting total watch time
seriesdata <- inner_join(anftitles, series, by = "title")
moviedata <- inner_join(anftitles, movies, by = "title")





#total movie time---------------------------------------------------------------

# Assuming your dataframe is named "data" and the column you 
#want to sum is named "column_name"

# Extract numeric values from the strings using regular expressions
numeric_values <- as.numeric(gsub("[^0-9.]+", "", moviedata$duration))
# Sum the extracted numeric values
column_sum <- sum(numeric_values)
# Print the sum
print(column_sum)




#total series time---------------------------------------------------------------

# Assuming your dataframe is named "data" and the column you 
#want to sum is named "column_name"

# Extract numeric values from the strings using regular expressions
numeric <- as.numeric(gsub("[^0-9.]+", "", seriesdata$duration))
# Sum the extracted numeric values
column <- sum(numeric)
# Print the sum
print(column)


# Extract numeric values from the strings using regular expressions
numeric2 <- as.numeric(gsub("[^0-9.]+", "", seriesdata$n))
# Sum the extracted numeric values
column2 <- sum(numeric2)
# Print the sum
print(column2)

totalstime <- column2 * 45
overalltottime <- totalstime + column_sum
#total time spent on movies and series -----------------------------------------


#Most episodes in month--------------------------------------------------------

# Convert the date column to a Date object
marathons2 <- marathons
marathons2$Date <- as.Date(marathons2$Date)


# Group the data by month and summarize the sum of rows in each month
monthly_totals <- marathons2 %>%
  group_by(month = format(Date, "%Y-%m")) %>%
  summarize(total_rows = n())
#--------------
# Assuming your dataset with monthly totals is named "monthly_totals" 
#and the month column is named "month"
  
# Convert the month column to a Date object with the first day of each month
monthly_totals$month <- as.Date(paste0(monthly_totals$month, "-01"))

# Rename the month column to the desired format
monthly_totals <- monthly_totals %>%
mutate(month = format(month, "%B %Y"))

#---------------------

# Convert the date column to a Date object
marathons3 <- marathons
marathons3$Date <- as.Date(marathons3$Date)


# Group the data by month and summarize the sum of rows in each month
monthly_totals2 <- marathons3 %>%
  group_by(month = format(Date, "%Y-%m")) %>%
  summarize(total_rows = n())

monthly_totals2$year <- format(as.Date(paste0(monthly_totals2$month, "-01")), "%Y")
monthly_totals2$month <- format(as.Date(paste0(monthly_totals2$month, "-01")), "%m")

# Find the highest monthly total per year
highest_totals_per_year <- monthly_totals2 %>%
  group_by(year) %>%
  filter(total_rows == max(total_rows)) %>%
  ungroup()

# Print the highest monthly totals per year
print(highest_totals_per_year)

#-------------------------------------------------------------------------------
# Split the genres into separate elements
all_genres <- unlist(strsplit(seriesdata$listed_in, ","))
print(all_genres)
# Count the occurrences of each genre
genre_counts <- table(all_genres)

genre_counts_df <- as.data.frame(genre_counts)
# Rename the column in the dataframe
names(genre_counts_df) <- c("Genre", "Count")
# Print the genre counts dataframe
print(genre_counts_df)



#------------------------movies-------------------
# Split the genres into separate elements
all_genres2 <- unlist(strsplit(moviedata$listed_in, ","))

# Count the occurrences of each genre
genre_counts2 <- table(all_genres2)

genre_counts_df2 <- as.data.frame(genre_counts2)
# Rename the column in the dataframe
names(genre_counts_df2) <- c("Genre", "Count")
# Print the genre counts dataframe
print(genre_counts_df2)









#---------------------------------------------------------Top 3 Series

# Sort the dataframe by the "n" column in descending order
sorted_series <- series %>% arrange(desc(n))

# Get the title(s) with the most watched episodes
most_watched_titles <- sorted_series %>% filter(n == max(n))

# Get the top three titles with the most watched episodes
top_three_titles <- sorted_series %>% slice(1:3)

# Print the top three titles with the most watched episodes
print(top_three_titles)

#--------------------------------------------------------Top 3 Movies

# Sort the dataframe by the "n" column in descending order
sorted_movies <- movies %>% arrange(desc(Date))

# Get the title(s) with the most watched episodes
most_watched_movies <- sorted_movies %>% filter(Date == max(Date))

# Get the top three titles with the most watched episodes
top_three_movies <- sorted_movies %>% slice(1:3)

# Print the top three titles with the most watched episodes
print(top_three_movies)


