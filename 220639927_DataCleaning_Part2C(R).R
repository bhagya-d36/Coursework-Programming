# Importing the required libraries
library('readr')
library('tidyverse')

# Loading the dataframes from 2006 and 2007
df06 <- read.csv('C:\\Users\\bhagy\\Programming CourseWork\\Python\\2006.csv')
df07 <- read.csv('C:\\Users\\bhagy\\Programming CourseWork\\Python\\2007.csv')
# Combining the two dataframes into one dataframe
df <- rbind(df06, df07)
head(df)

#Identifying the null values in the dataframe
nullvalues <- colSums(is.na(df))
print(nullvalues)

# Removing the columns containing null/empty values
Nullcolumns <- c('ArrTime','DepTime', 'ActualElapsedTime','CRSElapsedTime', 'AirTime','ArrDelay','DepDelay','CancellationCode')
df <- df[, !names(df) %in% Nullcolumns]
#Rechecking null values in the dataframe
nullvalues <- colSums(is.na(df))
print(nullvalues)


#Inspecting value counts for 'Diverted'
table(df$Diverted)


# Checking for unreasonable values/outliers by analysing the minimum and maximum values of each column
for (col_name in names(df)) {
  min_val <- min(df[[col_name]], na.rm = TRUE)
  max_val <- max(df[[col_name]], na.rm = TRUE)
  cat('Colname:', col_name, ', min:', min_val, ', max:', max_val, '\n')
}


#Rectifying the maximum value in CRS DepTime
df <- df[df$CRSArrTime < 2400, ]
#Rechecking after rectification
for (col_name in names(df)) {
  min_val <- min(df[[col_name]], na.rm = TRUE)
  max_val <- max(df[[col_name]], na.rm = TRUE)
  cat('Colname:', col_name, ', min:', min_val, ', max:', max_val, '\n')
}

# Saving the cleaned dataframe for Flight Analysis QC
write.csv(df, file = 'Cleaned_Dataframe_for_Flight_Analysis_QC(R).csv')

#Importing the 'airports' dataset
df_a <- read.csv('C:\\Users\\bhagy\\Programming CourseWork\\Python\\airports.csv')
#View the dataset
head(df_a)


#Identifying the null values in the dataframe
nullvalues <- colSums(is.na(df_a))
print(nullvalues)


#Removing rows containing null values
df_a <- df_a[complete.cases(df_a), ]
#Rechecking for null values after removing rows
colSums(is.na(df_a))


#Checking the number of rows and columns left
dim(df_a)


# Saving the cleaned 'airports' dataframe for Flight Analysis QC
write.csv(df, file = 'Airports_Cleaned.csv')



#Importing the 'carriers' dataset
df_c <- read.csv('C:\\Users\\bhagy\\Programming CourseWork\\Python\\carriers.csv')
#View the dataset
head(df_c)


#Identifying the null values in the dataframe
nullvalues <- colSums(is.na(df_c))
print(nullvalues)

#Removing rows containing null values
df_c <- df_c[complete.cases(df_c), ]
#Rechecking for null values after removing rows
colSums(is.na(df_c))


#Checking the number of rows and columns left
dim(df_c)

# Saving the cleaned 'carriers' dataframe for Flight Analysis QC
write.csv(df, file = 'Carriers_Cleaned.csv')

#Data cleaning is now complete.