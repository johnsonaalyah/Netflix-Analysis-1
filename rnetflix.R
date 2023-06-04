# LIBRARIES
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)


# READING DATA FROM CSV DOWNLOADED FROM NETFLIX ACCOUNT

getwd()
setwd('C:/Users/Que/Desktop/NetflixAnalysis1')

mynetflix <- read.csv("NetflixViewingHistory.csv")
str(mynetflix)
mynetflix$Date <- dmy(mynetflix$Date)

# SEPARATE TITLE COLUMN IN TITLE OF TV SERIES, SEASON AND EPISODE TITLE
mynetflixseries <- mynetflix %>%
  separate(col = Title, into = c("title", "season", "episode_title"), sep = ': ')


# REMOVE OCCURRENCES WHERE SEASON AND EPISODE ARE EMPTY (BECAUSE THEY ARE NOT TV SERIES)
mynetflixseries <- mynetflixseries[!is.na(mynetflixseries$season),]
mynetflixseries <- mynetflixseries[!is.na(mynetflixseries$episode_title),]



# RECORD OF NUMBER OF EPISODES VIEWED PER DAY, PER SERIES
mynetflix_marathons <- mynetflixseries%>%
  count(title, Date)



# LET'S CONSIDER "BINGE-WATCHING" 6 OR MORE EPISODES PER DAY AND SORT BY DATE
mynetflix_marathons <- mynetflix_marathons[mynetflix_marathons$n >= 6,]
mynetflix_marathons
mynetflix_marathons <- mynetflix_marathons[order(mynetflix_marathons$Date),]
mynetflix_marathons

# GROUPING DATA BY TV SERIES TITLE AND SORTING BY NUMBER OF EPISODES VIEWED
allmarathons <- mynetflix_marathons %>% 
  group_by(title) %>% 
  summarise(n = sum(n)) %>%
  arrange(desc(n))
# PLOTTING TOP 10 OF BINGE-WATCHING TV SERIES
topmarathons <- allmarathons %>% 
  top_n(10) %>%
  ggplot(aes(x = reorder(title, n), y = n)) +
  geom_col(fill = "#0097d6") +
  coord_flip() +
  ggtitle("Top 10 Most Watched Series On My Netflix", "4 or more episodes per day") +
  labs(x = "Series", y = "Total Viewed Episodes") +
  theme_minimal()
topmarathons

# EPISODES PER DAY
dailyepisodes <- mynetflix %>%
  count(Date) %>%
  arrange(desc(n))

# PLOTTING EPISODES PER DAY
dailyepisodesplot <- ggplot(aes(x = Date, y = n, color = n), data = dailyepisodes) +
  geom_col(color = c("#f16727")) +
  theme_minimal() +
  ggtitle("Episodes Watched on My Netflix per day") +
  labs(x = "Date", y = "Episodes watched") 
dailyepisodesplot



# CALENDAR WITH NUMBER OF EPISODES SEEN PER DAY IN HEATMAP
dailyepisodes <- dailyepisodes[order(dailyepisodes$Date),]
dailyepisodes$weekday <- wday(dailyepisodes$Date)
dailyepisodes$weekdayF <- weekdays(dailyepisodes$Date, abbreviate = T)
dailyepisodes$monthF <- months(dailyepisodes$Date, abbreviate = T)



dailyepisodes$weekdayF <-factor(dailyepisodes$weekday, levels = rev(1:7), 
                                          labels = rev(c("Mon","Tue","Wed","Thur","Fri","Sat","Sun")),
                                          ordered = TRUE)
dailyepisodes$monthF <- factor(month(dailyepisodes$Date),levels = as.character(1:12), 
                                     labels = c("Jan","Feb","Mar","April","May","June","July","August",
                                                "Sept","Oct","Nov","Dec"),ordered = TRUE)
dailyepisodes$yearmonth <- factor(as.yearmon(dailyepisodes$Date)) 
dailyepisodes$week <- as.numeric(format(dailyepisodes$Date,"%W"))
dailyepisodes$weekmonth <- ceiling(day(dailyepisodes$Date) / 7)

dailyepisodes_calendar <- ggplot(dailyepisodes, aes(weekmonth, weekdayF, fill = dailyepisodes$n)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(dailyepisodes$Date) ~ monthF) + 
  scale_fill_gradient(low = "#FFD000", high = "#FF1919") + 
  ggtitle("Episodes Watched per Day", "Heatmap for Days of the week, Month and year") +
  labs(x = "Week", y = "Day of the week") +
  labs(fill = "No. Episodes")
dailyepisodes_calendar




# FREQUENCY OF ACTIVITY IN MY NETFLIX ACCOUNT PER DAY
viewday <- dailyepisodes %>%
  count(weekdayF)
viewday
viewday_plot <- viewday %>% 
  ggplot(aes(weekdayF, n)) +
  geom_col(fill = "#5b59d6") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Frequency of episodes watched", 
          "Activity by Days of the Week on my  Netflix")
viewday_plot


# FREQUENCY OF ACTIVITY IN MY NETFLIX ACCOUNT PER MONTH
viewmonth <- dailyepisodes %>%
  count(monthF)
viewmonth
viewmonth_plot <- viewmonth %>% 
  ggplot(aes(monthF, n)) +
  geom_col(fill = "#808000") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  ggtitle("Frequency of episodes watched", 
          "Activity by Month on my Netflix") 
viewmonth_plot

# FREQUENCY OF ACTIVITY IN MY NETFLIX ACCOUNT PER YEAR
viewyear <- dailyepisodes %>%
  count(yearmonth)
viewyear
viewyear_plot <- viewyear %>% 
  ggplot(aes(yearmonth, n)) +
  geom_col(fill = "#1a954d") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  ggtitle("Frequency of episodes watched", 
          "Activity by Month on my Netflix")
viewyear_plot
