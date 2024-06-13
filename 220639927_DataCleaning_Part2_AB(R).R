#Importing the required libraries
library('readr')
library('tidyverse')

#Loading the dataframes from 2006 and 2007
df06 <- read.csv('C:\\Users\\bhagy\\Programming CourseWork\\Python\\2006.csv')
df07 <- read.csv('C:\\Users\\bhagy\\Programming CourseWork\\Python\\2007.csv')
#Combining the two dataframes into one dataframe
df <- rbind(df06, df07)
head(df)

#Identifying the null values in the dataframe
nullvalues <- colSums(is.na(df))
print(nullvalues)

#Removing rows containing null values
df <- na.omit(df)

#Rechecking for null values after removing
nullvalues <- colSums(is.na(df))
print(nullvalues)


#Checking for empty values
emptyvalues <- colSums(df == '')
print(emptyvalues)

#Dropping the column 'CancellationCode'
df <- df[, -which(names(df) == 'CancellationCode')]
head(df)

#Checking for unreasonable values/outliers by analysing the minimum and maximum values of each column
for (col_name in names(df)) {
  min_val <- min(df[[col_name]], na.rm = TRUE)
  max_val <- max(df[[col_name]], na.rm = TRUE)
  cat('Colname:', col_name, ', min:', min_val, ', max:', max_val, '\n')
}

#All data for 'Cancelled' and 'Diverted' flights have been removed, but they won't be needed for the first two parts of the question.
#The negative values existing in the columns 'ArrDelay' and 'DepDelay' are assumed to be a result of early flights.
#The maximum values in 'DepTime' and 'AirTime' have exceeded 2400, which is unreasonable.
#The maximum values are therefore rectified to 2359.
df <- subset(df, DepTime < 2400 & ArrTime < 2400)

#Rechecking after rectification
for (col_name in names(df)) {
  min_val <- min(df[[col_name]], na.rm = TRUE)
  max_val <- max(df[[col_name]], na.rm = TRUE)
  cat('Colname:', col_name, ', min:', min_val, ', max:', max_val, '\n')
}


#Saving the cleaned dataframe for Flight Analysis
write.csv(df, file = 'Cleaned_Dataframe_for_Flight_Analysis(R).csv')

#Importing 'plane-data' for cleaning
df_planedata <- read.csv('C:\\Users\\bhagy\\Programming CourseWork\\Python\\plane-data.csv')
#View the plane data
head(df_planedata)

#Checking for null values
nullvalues <- colSums(is.na(df_planedata))
print(nullvalues)
#Checking for empty values
emptyvalues <- colSums(df_planedata == '')
print(emptyvalues)

#Removing the empty values
df_planedata <- df_planedata[rowSums(df_planedata == '' | is.na(df_planedata)) == 0, ]
#Rechecking for empty values
emptyvalues <- colSums(df_planedata == '')
print(emptyvalues)

#Checking the number of rows and columns
dimensions <- dim(df_planedata)
print(dimensions)


#View the plane data
head(df_planedata)

# Saving the cleaned dataframe for Flight Analysis
write.csv(df, file = 'Planedata_Cleaned(R).csv')

#Data cleaning is now complete. However, data cleaning will be seperately conducted for Part 2 Qc.