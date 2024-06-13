#Importing the required libraries
library(dplyr)
library(ggplot2)

#Loading the cleaned dataframes
df <- read.csv('C:/Users/bhagy/Programming CourseWork/Python/Cleaned_Dataframe_for_Flight_Analysis.csv')
df_planedata <- read.csv('C:/Users/bhagy/Programming CourseWork/Python/Plane_Data_Cleaned.csv')
#Viewing the cleaned dataset
head(df)
#Viewing plane data
head(df_planedata)

#Removing unnecessary columns and creating a simpler dataframe
df1 <- df[c('TailNum', 'Year', 'Month', 'DayofMonth', 'DayOfWeek', 'DepDelay', 'ArrDelay', 'DepTime')]
head(df1)

#Renaming columns in 'df_planedata' for convenient merging
names(df_planedata)[names(df_planedata) == 'tailnum'] <- 'TailNum'
names(df_planedata)[names(df_planedata) == 'year'] <- 'YearOfManufacture'
head(df_planedata)

#Merging the two dataframes on TailNum
merged_df <- merge(df1, df_planedata, by = 'TailNum')
head(merged_df)

#Adding a new column TotalDelay and replacing negative values with 0
merged_df$TotalDelay <- merged_df$ArrDelay + merged_df$DepDelay
merged_df$TotalDelay <- pmax(merged_df$TotalDelay, 0)
head(merged_df)

# Checking for unreasonable values in the Year of Manufacture column
table(merged_df$YearOfManufacture)
#'0' has to be removed
merged_df <- merged_df[merged_df$YearOfManufacture != '0', ]
table(merged_df$YearOfManufacture)

#Checking for null values
colSums(is.na(merged_df))
#There are no null values

#Seperating New Airplanes and Old Airplanes
merged_df$YearOfManufacture <- as.integer(merged_df$YearOfManufacture)

#Assumption : Planes manufactured before the year 1982 are considered old airplanes.
age <- function(x) {
  if (x < 1982) {
    return('Old')
  } else {
    return('New')
  }
}
#Creating a new column 'PlaneCondition' using function(x)
merged_df$PlaneCondition <- sapply(merged_df$YearOfManufacture, age)
head(merged_df)

#Create a new column 'PlaneAge'
merged_df$PlaneAge <- as.integer(merged_df$Year) - as.integer(merged_df$YearOfManufacture)
head(merged_df)

#Checking for unreasonable entries in 'PlaneAge'
table(merged_df$PlaneAge)
#Removing the -1 entry in Plane Age
merged_df <- merged_df[merged_df$PlaneAge != -1, ]
#Rechecking after removing
table(merged_df$PlaneAge)


#Splitting the dataframe into the 2 years for the analysis
df_2006 <- merged_df[merged_df$Year == 2006, ]
df_2007 <- merged_df[merged_df$Year == 2007, ]


#Simplifying the dataframe by taking only the required columns
df_2006 <- df_2006[, c('Year', 'YearOfManufacture', 'TotalDelay', 'PlaneCondition', 'PlaneAge')]
df_2007 <- df_2007[, c('Year', 'YearOfManufacture', 'TotalDelay', 'PlaneCondition', 'PlaneAge')]
head(df_2006)
head(df_2007)

#ANALYSING 2006

#Creating a dataframe obtaining the mean total delay for every year of manufacture for planes.
AvgDelay_MfgYear_06 <- aggregate(TotalDelay ~ YearOfManufacture, data = df_2006, FUN = mean)
AvgDelay_MfgYear_06

# Obtaining a new average delay with respect to Old and New airplanes
PlaneCondition_Delay2006 <- aggregate(TotalDelay ~ PlaneCondition, data = df_2006, FUN = mean)
colnames(PlaneCondition_Delay2006) <- c('PlaneCondition', 'AvgTotalDelay')
PlaneCondition_Delay2006
#In 2006, the mean total delay is greater for newer planes in comparsion to older planes.


#Obtaining a new dataframe with Plane Age and Mean Total Delay.  
AvgDelay_Age06 <- aggregate(TotalDelay ~ PlaneAge, data = df_2006, FUN = mean)
colnames(AvgDelay_Age06) <- c('PlaneAge', 'MeanTotalDelay')
AvgDelay_Age06

#Correlation Coefficient
cor(AvgDelay_Age06$PlaneAge, AvgDelay_Age06$MeanTotalDelay)
#For the year 2006, the average delay has a decreasing trend when the age of the plane increases.

#ANALYSING 2007

#Creating a dataframe obtaining the mean total delay for every year of manufacture for planes.
AvgDelay_MfgYear_07 <- aggregate(TotalDelay ~ YearOfManufacture, data = df_2007, FUN = mean)
AvgDelay_MfgYear_07

# Obtaining a new average delay with respect to Old and New airplanes
PlaneCondition_Delay2007 <- aggregate(TotalDelay ~ PlaneCondition, data = df_2007, FUN = mean)
colnames(PlaneCondition_Delay2007) <- c('PlaneCondition', 'AvgTotalDelay')
PlaneCondition_Delay2007
#In 2007, the mean total delay is greater for older planes in comparsion to newer planes.

#Obtaining a new dataframe with Plane Age and Mean Total Delay  
AvgDelay_Age07 <- aggregate(TotalDelay ~ PlaneAge, data = df_2007, FUN = mean)
colnames(AvgDelay_Age07) <- c('PlaneAge', 'MeanTotalDelay')
AvgDelay_Age07

#Correlation coefficient
cor(AvgDelay_Age07$PlaneAge, AvgDelay_Age07$MeanTotalDelay)
#For the year 2007, the average delay has an increasing trend when the age of the plane increases.

# Visualisations

#Visualising both years on the same figure to obtain the mean total delay based on plane condition
#Differentiating 2006 and 2007 by creating a new merged dataframe for visualisation purposes
PlaneCondition_Delay2006$Year <- 2006
PlaneCondition_Delay2007$Year <- 2007
#Merging the dataframes after seperating the 2 years.
PlaneCondition_Delay0607 <- rbind(PlaneCondition_Delay2006, PlaneCondition_Delay2007)

#Creating the figure (barplot)
ggplot(PlaneCondition_Delay0607, aes(x = factor(Year), y = AvgTotalDelay, fill = PlaneCondition)) +
  geom_bar(stat = 'summary', fun = 'mean', position = 'dodge') +
  labs(x = 'Year', y = 'Average Total Delay', fill = 'Plane Condition') +
  scale_fill_manual(values = c('darkcyan', 'darkslategray')) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = 'gray90', color = NA),
    panel.grid.major = element_line(color = 'white', linetype = 'solid'),
    panel.grid.minor = element_blank()
  )

#Plotting a regression plot for both years
ggplot() +
  geom_point(data = AvgDelay_Age07, aes(x = PlaneAge, y = MeanTotalDelay, color = '2007'), size = 3) +
  geom_smooth(data = AvgDelay_Age07, aes(x = PlaneAge, y = MeanTotalDelay, color = '2007'), method = 'lm', se = FALSE) +
  geom_point(data = AvgDelay_Age06, aes(x = PlaneAge, y = MeanTotalDelay, color = '2006'), size = 3) +
  geom_smooth(data = AvgDelay_Age06, aes(x = PlaneAge, y = MeanTotalDelay, color = '2006'), method = 'lm', se = FALSE) +
  labs(title = 'Average Delay vs Age of Plane', x = 'Age of Plane', y = 'Average Delay', color = 'Year') +
  scale_color_manual(values = c('2007' = 'indianred', '2006' = 'darkcyan')) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = 'white', color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'right'
  )