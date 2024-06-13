#Part2QC - Creating a logistic regression model and visualizing the coefficients across years

#Data wrangling

#Loading the required libraries for data wrangling and visualizations.
library(dplyr)
library(reshape2)
library(caret)
library(ROSE)
library(corrplot)
library(RColorBrewer)

#Loading the seperately cleaned dataset for Part2 QC 
df_main <- read.csv('C:/Users/bhagy/Programming CourseWork/Python/Cleaned_Dataframe_Part2QC_for_Flight_Analysis.csv') 
#Loading the airports dataset
df_a <- read.csv('C:/Users/bhagy/Programming CourseWork/Python/Airports_Cleaned.csv')

#View the dataset
head(df_main)

#View the dataset
head(df_a)

#Renaming the column 'iata' to 'IATA Code'
df_a <- df_a %>% rename(`IATA Code` = iata)
head(df_a)

#Dropping the useless columns in the dataframes
df_main <- df_main[, -which(names(df_main) == 'X')]
df_a <- df_a[, -which(names(df_a) == 'X')]
head(df_main)
head(df_a)

#Checking for null values in the dataframe
null_counts <- colSums(is.na(df_main))
print(null_counts)
#No null values in the dataframe

#Merging the dataframe with the airports dataframe to obtain the corresponding coordinates(lat & long) of the  Origin and Destination airports

#Origin
df_main <- merge(df_main, df_a[, c('IATA Code', 'lat', 'long')], by.x = 'Origin', by.y = 'IATA Code', all.x = TRUE)
names(df_main)[names(df_main) == 'lat'] <- 'Origin_Lat'
names(df_main)[names(df_main) == 'long'] <- 'Origin_Long'

#Dest
df_main <- merge(df_main, df_a[, c('IATA Code', 'lat', 'long')], by.x = 'Dest', by.y = 'IATA Code', all.x = TRUE)
names(df_main)[names(df_main) == 'lat'] <- 'Dest_Lat'
names(df_main)[names(df_main) == 'long'] <- 'Dest_Long'

#Viewing the dataset
head(df_main)

#Checking for any missing values in the new columns
sum(is.na(df_main$Origin_Lat))
sum(is.na(df_main$Origin_Long))
sum(is.na(df_main$Dest_Lat))
sum(is.na(df_main$Dest_Long))

#Removing the missing values from the columns
df_main <- df_main[complete.cases(df_main$Origin_Lat, df_main$Origin_Long, df_main$Dest_Lat, df_main$Dest_Long), ]

#Rechecking for missing values
sum(is.na(df_main$Origin_Lat))
sum(is.na(df_main$Origin_Long))
sum(is.na(df_main$Dest_Lat))
sum(is.na(df_main$Dest_Long))

#Rounding off the 'Origin Lat', 'Origin Long', 'Dest Lat', and 'Dest Long' columns to 1 decimal place
df_main$Origin_Lat <- round(df_main$Origin_Lat, 1)
df_main$Origin_Long <- round(df_main$Origin_Long, 1)
df_main$Dest_Lat <- round(df_main$Dest_Lat, 1)
df_main$Dest_Long <- round(df_main$Dest_Long, 1)

#Viewing the dataset
head(df_main)

#Checking for any data types that require encoding
str(df_main)

#Encoding the data
df_main$UniqueCarrier <- factor(df_main$UniqueCarrier)
df_main$Origin <- factor(df_main$Origin)
df_main$Dest <- factor(df_main$Dest)

#Rechecking the data types
str(df_main)

#Convert factor variables to numeric
df_main$Dest <- as.numeric(df_main$Dest)
df_main$Origin <- as.numeric(df_main$Origin)
df_main$UniqueCarrier <- as.numeric(df_main$UniqueCarrier)

#Rechecking after conversion
str(df_main)

#viewing the dataset
head(df_main)

#Inspecting value counts for 'Diverted' flights
table(df_main$Diverted)

#Splitting the dataframe for yearly analysis
df_2006 <- df_main[df_main$Year == 2006, ]
df_2007 <- df_main[df_main$Year == 2007, ]

#Inspecting value counts for the 'Diverted' flights in each year

#2006
table(df_2006$Diverted)

#2007
table(df_2007$Diverted)

#LOGISTIC REGRESSION MODEL

#2006

#Checking for columns with zero variance
columns_0var <- colnames(df_2006)[apply(df_2006, 2, function(x) length(unique(x))) == 1]
columns_0var
#Removing the  column 'Year' since it has 0 variance
df_2006 <- df_2006[, !colnames(df_2006) %in% columns_0var]
columns_0var

#Correlation heatmap to analyse the correlation among potential features and target variable
columns <- c('Month','DayofMonth','DayOfWeek','CRSDepTime','CRSArrTime','UniqueCarrier','Origin', 'Dest','Distance','Cancelled','TaxiIn','TaxiOut','Origin_Lat','Origin_Long','Dest_Lat','Dest_Long', 'CarrierDelay', 'WeatherDelay', 'NASDelay', 'SecurityDelay', 'LateAircraftDelay', 'Diverted')
#Forming the correlation matrix
features_correlation <- cor(df_2006[, columns])
blue_palette <- brewer.pal(10, 'Blues')
#Plotting the correlation heatmap
corrplot(features_correlation, method = 'color', col = blue_palette, type = 'full', order = 'original', addCoef.col = 'black', tl.col = 'black', tl.srt = 90, diag = TRUE, tl.pos = 'lt', number.cex = 0.4, tl.cex = 0.6, addgrid = TRUE)


#Splitting the dataset into the selected features and target variable
features <- c('Month', 'DayofMonth', 'DayOfWeek', 'CRSDepTime', 'CRSArrTime', 'UniqueCarrier','Distance', 'TaxiOut', 'Origin_Lat','Origin_Long','Dest_Lat','Dest_Long')
X <- df_2006[,features]
Y <- df_2006$Diverted  

#Seperating the dataset into train data and test data
set.seed(24)
train_split <- createDataPartition(Y, p = 0.8, list = FALSE)
X_train <- X[train_split, ]
Y_train <- Y[train_split]
X_test <- X[-train_split, ]
Y_test <- Y[-train_split]

#Feature scaling (Standardization)
scaling_parameters <- preProcess(X_train, method = c('center', 'scale'))
X_train_scaled <- predict(scaling_parameters, X_train)
X_test_scaled <- predict(scaling_parameters, X_test)

#Combine features and target variable for resampling (train data only)
train_data <- cbind(X_train_scaled, Y_train)
train_data$Y_train <- factor(train_data$Y_train)
#Undersampling the majority class
resampled_data <- downSample(x = train_data[, -ncol(train_data)],
                             y = train_data[, ncol(train_data)],
                             yname = 'Y_train',
                             list = FALSE)

#Separating the features and target variable after undersampling
X_train_resampled <- resampled_data[, -ncol(resampled_data)]
Y_train_resampled <- resampled_data[, ncol(resampled_data)]

#Creating and training the logistic regression model
logreg_model <- glm(Y_train_resampled ~ ., data = X_train_resampled, family = binomial)
Y_prediction_probability <- predict(logreg_model, newdata = X_test_scaled, type = 'response')
Y_prediction <- ifelse(Y_prediction_probability > 0.5, 1, 0)

#Creating the confusion matrix
conf_matrix <- table(Y_test, Y_prediction) #M
#Visualising the confusion matrix
ConfusionMatrixMelted <- melt(conf_matrix)
colnames(ConfusionMatrixMelted) <- c('Actual', 'Predicted', 'Frequency')
ggplot(data = ConfusionMatrixMelted, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), color = 'black', fontface = 'bold') +
  scale_fill_gradient(low = 'lightblue', high = 'darkblue') +
  labs(title = 'Confusion matrix', x = 'Actual', y = 'Predicted') +
  theme_minimal()

#Obtaining the classification report of the model
print(caret::confusionMatrix(as.factor(Y_prediction), as.factor(Y_test)))

#Calculating the accuracy
accuracy <- sum(Y_test == Y_prediction) / length(Y_test)
print(paste('Accuracy:', accuracy))

#Visualising the coeffients

#Obtaining the coefficient values from the model
coefficients <- coef(logreg_model)[-1]

#Obtaining the features from the columns
feature_list <- colnames(X_train_scaled)

#Creating a new dataframe for plotting purposes
coef_df <- data.frame(feature_list, coefficients)


#Visualising the coeffiecients 
ggplot(coef_df, aes(x = coefficients, y = feature_list)) +
  geom_bar(stat = 'identity', fill = 'steelblue', color = 'black') +
  labs(x = 'Coefficient Value', y = 'Features', title = 'Coefficient Visualization - Logistic Regression') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 10))

#2007


#Checking for columns with zero variance
columns_0var <- colnames(df_2007)[apply(df_2007, 2, function(x) length(unique(x))) == 1]
columns_0var
#Removing the  column 'Year' since it has 0 variance
df_2007 <- df_2007[, !colnames(df_2007) %in% columns_0var]
columns_0var

#Correlation heatmap to analyse the correlation among potential features and target variable
columns <- c('Month','DayofMonth','DayOfWeek','CRSDepTime','CRSArrTime','UniqueCarrier','Origin', 'Dest','Distance','Cancelled','TaxiIn','TaxiOut','Origin_Lat','Origin_Long','Dest_Lat','Dest_Long', 'CarrierDelay', 'WeatherDelay', 'NASDelay', 'SecurityDelay', 'LateAircraftDelay', 'Diverted')
#Forming the correlation matrix
features_correlation <- cor(df_2007[, columns])
#Colors
blue_palette <- brewer.pal(10, 'Blues')
#Plotting the correlation heatmap
corrplot(features_correlation, method = 'color', col = blue_palette, type = 'full', order = 'original', addCoef.col = 'black', tl.col = 'black', tl.srt = 90, diag = TRUE, tl.pos = 'lt', number.cex = 0.4, tl.cex = 0.6, addgrid = TRUE)


#Splitting the dataset into the selected features and target variable
features <- c('Month', 'DayofMonth', 'DayOfWeek', 'CRSDepTime', 'CRSArrTime', 'UniqueCarrier','Distance' ,'TaxiOut', 'Origin_Lat','Origin_Long','Dest_Lat','Dest_Long')
X <- df_2007[,features]
Y <- df_2007$Diverted  

#Seperating the dataset into train data and test data
set.seed(24)
train_split <- createDataPartition(Y, p = 0.8, list = FALSE)
X_train <- X[train_split, ]
Y_train <- Y[train_split]
X_test <- X[-train_split, ]
Y_test <- Y[-train_split]

#Feature scaling (Standardization)
scaling_parameters <- preProcess(X_train, method = c('center', 'scale'))
X_train_scaled <- predict(scaling_parameters, X_train)
X_test_scaled <- predict(scaling_parameters, X_test)


#Combine features and target variable for resampling (train data only)
train_data <- cbind(X_train_scaled, Y_train)
train_data$Y_train <- factor(train_data$Y_train)
#Undersampling the majority class
resampled_data <- downSample(x = train_data[, -ncol(train_data)],
                             y = train_data[, ncol(train_data)],
                             yname = 'Y_train',
                             list = FALSE)

#Separating the features and target variable after undersampling
X_train_resampled <- resampled_data[, -ncol(resampled_data)]
Y_train_resampled <- resampled_data[, ncol(resampled_data)]

#Creating and training the logistic regression model
logreg_model <- glm(Y_train_resampled ~ ., data = X_train_resampled, family = binomial)
Y_prediction_probability <- predict(logreg_model, newdata = X_test_scaled, type = 'response')
Y_prediction <- ifelse(Y_prediction_probability > 0.5, 1, 0)

#Creating the confusion matrix
conf_matrix <- table(Y_test, Y_prediction)
#Plotting the confusion matrix
ConfusionMatrixMelted <- melt(conf_matrix)
colnames(ConfusionMatrixMelted) <- c('Actual', 'Predicted', 'Frequency')
ggplot(data = ConfusionMatrixMelted, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), color = 'black', fontface = 'bold') +
  scale_fill_gradient(low = 'lightblue', high = 'darkblue') +
  labs(title = 'Confusion matrix', x = 'Actual', y = 'Predicted') +
  theme_minimal()

#Obtaining the classification report of the model
print(caret::confusionMatrix(as.factor(Y_prediction), as.factor(Y_test)))

#Calculating the accuracy
accuracy <- sum(Y_test == Y_prediction) / length(Y_test)
print(paste('Accuracy:', accuracy))


#Visualising the coeffients

#Obtaining the coefficient values from the model
coefficients <- coef(logreg_model)[-1]

#Obtaining the features from the columns
feature_list <- colnames(X_train_scaled)

#Creating a new dataframe for plotting purposes
coef_df <- data.frame(feature_list, coefficients)

#Visualising the coeffiecients 
ggplot(coef_df, aes(x = coefficients, y = feature_list)) +
  geom_bar(stat = 'identity', fill = 'steelblue', color = 'black') + 
  labs(x = 'Coefficient Value', y = 'Features', title = 'Coefficient Visualization - Logistic Regression') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 10))
