# Importing the required libraries
library(tidyverse)
library(ggplot2)

# Loading the cleaned dataset
df <- read.csv('C:\\Users\\bhagy\\Programming CourseWork\\Python\\Cleaned_Dataframe_for_Flight_Analysis.csv')
# Viewing the dataset
head(df)

# Creating a new column 'TotalDelayTime' by adding 'ArrDelay' and 'DepDelay'
df$TotalDelayTime <- df$ArrDelay + df$DepDelay
head(df) 
# Replacing negative values with 0 in the 'TotalDelayTime' column
df$TotalDelayTime[df$TotalDelayTime < 0] <- 0
# View dataset
head(df) 

#Creating 6 sessions using different time periods of the day in relation to DepTime
timeperiod <- function(x) {
  if (x < 400) {
    return('00:00-04:00')
  } else if (x >= 400 && x < 800) {
    return('04:00-08:00')
  } else if (x >= 800 && x < 1200) {
    return('08:00-12:00')
  } else if (x >= 1200 && x < 1600) {
    return('12:00-16:00')
  } else if (x >= 1600 && x < 2000) {
    return('16:00-20:00')
  } else if (x >= 2000 && x < 2400) {
    return('20:00-24:00')
  }
}

#Creating a new column 'Session' for the 6 sessions created.
df$Session <- sapply(df$DepTime, timeperiod)

#Splitting the dataframe into each year for the application of the question
df_2006 <- df[df$Year == 2006, ]
df_2007 <- df[df$Year == 2007, ]

#What are the best times of the day to minimise delays each year?

#Creating a dataframe to analyse the mean total delay for each session for the year 2006.
AvgDelay_session06 <- df_2006 %>%
  group_by(Session) %>%
  summarize(AvgDelay = mean(TotalDelayTime)) %>%
  ungroup()
AvgDelay_session06
#The session 04:00 - 08:00 is the best time of the day to minimize delays for the year 2006.

#Creating a dataframe to analyse the mean total delay for each session for the year 2007.
AvgDelay_session07 <- df_2007 %>%
  group_by(Session) %>%
  summarize(AvgDelay = mean(TotalDelayTime)) %>%
  ungroup()
AvgDelay_session07
#The session 04:00 - 08:00 is the best time of the day to minimise delays for the year 2007.

#Visualisation

# Creating a dataframe for visualization purposes (2006)
df06 <- AvgDelay_session06
names(df06) <- c('Session', 'MeanDelay')
df06$MeanDelay <- round(df06$MeanDelay, digits = 0)
df06 <- df06 %>% mutate(index = row_number()) %>% select(index, everything())
df06
# Creating a dataframe for visualization purposes (2007)
df07 <- AvgDelay_session07
names(df07) <- c('Session', 'MeanDelay')
df07$MeanDelay <- round(df07$MeanDelay, digits = 0)
df07 <- df07 %>% mutate(index = row_number()) %>% select(index, everything())
df07
# Combine data from df06 and df07 and create a new dataframe df0607
df0607 <- bind_rows(df06 %>% mutate(Year = '2006'), df07 %>% mutate(Year = '2007'))

# Creating a barplot for visualization
ggplot(df0607, aes(x = Session, y = MeanDelay, fill = Year)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = 'Session', y = 'Mean Delay', fill = 'Years') +
  scale_fill_manual(values = c('2006' = 'darkseagreen', '2007' = 'darkcyan'))

# We can see a similar trend when pinpointing the best time of the day to minimise delays for both years. It's evident that the morning (4am - 12pm) is the best time of the day


# What are the best days of the week to minimise delays each year?

# Creating a dataframe to analyze the mean total delay for each day of the week for the year 2006
AvgDelay_day06 <- df_2006 %>%
  group_by(DayOfWeek) %>%
  summarize(MeanTotalDelay = mean(TotalDelayTime)) %>%
  ungroup()
#Converting numerical values in the 'DayOfWeek' column into corresponding days of the week
AvgDelay_day06$DayOfWeek <- factor(AvgDelay_day06$DayOfWeek, levels = 1:7,
                                   labels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
# Renaming columns
names(AvgDelay_day06) <- c('DayOfWeek', 'MeanTotalDelay')
# Resetting row indices
AvgDelay_day06 <- AvgDelay_day06 %>% mutate(index = row_number()) %>% select(index, everything())
AvgDelay_day06

#Saturday has the mean total delay for the year 2006
#Best days to minimise delays are ranked in the order; Saturday, Tuesday, Wednesday, Sunday, Monday, Thursday and Friday.


# Creating a dataframe to analyze the mean total delay for each day of the week for the year 2007
AvgDelay_day07 <- df_2007 %>%
  group_by(DayOfWeek) %>%
  summarize(MeanTotalDelay = mean(TotalDelayTime)) %>%
  ungroup()
#Converting numerical values in the 'DayOfWeek' column into corresponding days of the week
AvgDelay_day07$DayOfWeek <- factor(AvgDelay_day07$DayOfWeek, levels = 1:7,
                                   labels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
# Renaming columns
names(AvgDelay_day07) <- c('DayOfWeek', 'MeanTotalDelay')
# Resetting row indices
AvgDelay_day07 <- AvgDelay_day07 %>% mutate(index = row_number()) %>% select(index, everything())
AvgDelay_day07

#Saturday has lowest mean total delay for the year 2007.
#Best days to minimise delays are ranked in the order; Saturday, Tuesday, Wednesday, Monday, Sunday, Thursday and Friday.

# Creating a lineplot for visualization
ggplot() +
  geom_line(data = AvgDelay_day06, aes(x = DayOfWeek, y = MeanTotalDelay, color = '2006'), linewidth = 1.5, group = 1) +
  geom_line(data = AvgDelay_day07, aes(x = DayOfWeek, y = MeanTotalDelay, color = '2007'), linewidth = 1.5, group = 1) +
  scale_color_manual(values = c('2006' = 'blue', '2007' = 'darkcyan')) +
  labs(x = 'Day of Week', y = 'Mean Total Delay', color = 'Years') +
  theme_minimal() +
  theme(legend.position = 'right')

#Eventhough there is a slight difference in the ranking, we are able to witness a pattern where Saturday and Tuesday are the best days of the week to minimise delays for both years.

#It's evident that there is an increase in the mean total delay annually in this sample of data.