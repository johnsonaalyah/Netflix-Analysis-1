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

currentmarathons <- currentmarathons[order(currentmarathons$n, decreasing = TRUE), ]
currentmostwatched <- marathons[1, ]




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

anftitles <- allnftitles [, -c(1,3)]
#Cleaned title view even more to only include title, duration and tags


