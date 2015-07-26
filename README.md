# Course-Project-G-C-Data
Repository of the course project of Getting and Cleaning Data

This repository contains the scripts, files and descriptions necessary to analyze and interpret a study using 
mobile phone sensors (accelerometer and gyroscope) to understand subjects movement.

run_analysis.R runs the script that extract and orders data from the original files.
CodeBook.md describes every column presented in the results displayed by the run_analysis.R script

30 subjects took runs on training and testing sessions meaning that there are 30 different ID's for every single subject
ranging from 1 to 30.

These subjects performed 6 different activities:
  1. WALKING
  2. WALKING_UPSTAIRS
  3. WALKING_DOWNSTAIRS
  4. SITTING
  5. STANDING
  6. LAYING

The signals from the mobile phone sensors were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

* tBodyAcc-XYZ
* tGravityAcc-XYZ
* tBodyAccJerk-XYZ
* tBodyGyro-XYZ
* tBodyGyroJerk-XYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag
* fBodyAcc-XYZ
* fBodyAccJerk-XYZ
* fBodyGyro-XYZ
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag

According to requisites to create this tidy data set, mean and standard deviation variables have to be ones to be extracted.
The set of variables that were estimated from these signals are: 

* mean(): Mean value
* std(): Standard deviation
* meanFreq(): Weighted average of the frequency components to obtain a mean frequency

meanFreq() was included due to mean() being a variable asked in the requisites and frequency is a domain analyzed in the signals.

Means from the angle() variable where not considered because they involved a whole different set of information.

This gives a total of 81 columns including 79 variable columns, 1 subject column and 1 activity column.
If meanFreq() variables had left out we would see a total of 68 variable columns.

A litte change in the variable names was made to follow better practices of variable names topics.

## Script run_analysis and description:

*#* run_analysis.R is an script which returns a tidy data set of all the files that are in "UCI HAR Dataset"

*#* WARNING: the script only works with the all the files described below in one directory:

*#* "subject_test.txt", "subject_train.txt", "X_test.txt", "X_train.txt", "y_test.txt", "y_train.txt", "features.txt"

*#* if you want to make it work in your computer be sure to make one folder and put in all the files

*#* mentioned above and change the variable "path" inside the function.

*#* The dplyr library must be added to run this script


library(dplyr)
 
run_analysis <- function(){

*#* Path is the a character vector containing the directory where your files are.

*#* In my case, I grouped all the files that I was going to use in one folder called "test y train"

path = "C:/Users/Jesús/Documents/R 2015/Course03/Project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test y train"

dir <- getwd()

setwd(path)

*#* Here is the file reading of all the files to be used.

subject_test <- read.table("subject_test.txt")

subject_train <- read.table("subject_train.txt")

X_test <- read.table("X_test.txt")

X_train <- read.table("X_train.txt")

y_test <- read.table("y_test.txt")

y_train <- read.table("y_train.txt")

activity_labels <- read.table("activity_labels.txt")

features <- read.table("features.txt")

*#* 1.- Merges the training and the test sets to create one data set.

*#* All the test files are bind together into one table with the command cbind()

*#* where the first column is "y_test", the second is "subject_test", and so on,

*#* creating a table of the following dimensions 2946 x 563

data_test <- cbind(y_test, subject_test, X_test)


*#* The train files are bind together into one table with the command cbind()

*#* creating a table of 7352 x 563.

data_train <- cbind(y_train, subject_train, X_train)



*#* The tables data_test and data_train are bind together, with the command rbind()

*#* creating a table or data frame of dimensions 10299 x 563

*#* also is important to mention that data_train is above of data_test.

merged_data <- rbind(data_train,data_test)


*#* 2.- Extracts only the measurements on the mean and standard deviation for each measurement.

*#* measurements is a vector with all the names of the measured signals extracted from "features.txt"

measurements <- as.character(features$V2)


*#* The function grepl() is used for looking for the "mean" and "std" words in the measurementes vector.

*#* Also the function which is used to obtaing the index of the vector elements that contain the words

*#* before described.

meanIndex <- which(grepl("mean",measurements))

stdIndex <- which(grepl("std",measurements))


*#* Here the vectors meanIndex and stdIndex are concatenated, and also they are ordered with

*#* the function sort() from less to greater

allIndex <- sort(c(meanIndex, stdIndex))

*#* I added 2 units to the index to make everything fit as it should be.

*#* The columns that corresponds start in column 3, so thats why is added 2.

*#* In other words, the first 2 columns of "merged_data" correspond to the activities and the subjects.


allIndex_2 <- allIndex + 2


*#* A data frame called red_data is created with the first 2 columns intact of the "merged_data" data frame

*#* but all the other columns are about mean and standard deviation. By consecuence this reduces the data set.

red_data <- merged_data[,c(1,2,allIndex_2)]


*#* 3.- Uses descriptive activity names to name the activities in the data set

*#* The first column of "red_data" is changed into descriptive names to make it more readable.

*#* the function gsub() is used 6 times for replacing the numbers for the corresponding activites.

activity_names_data <- red_data[,1]

activity_names_data <- gsub(1, "WALKING", activity_names_data)

activity_names_data <- gsub(2, "WALKING UPSTAIRS", activity_names_data)

activity_names_data <- gsub(3, "WALKING DOWNSTAIRS", activity_names_data)

activity_names_data <- gsub(4, "SITTING", activity_names_data)

activity_names_data <- gsub(5, "STANDING", activity_names_data)

activity_names_data <- gsub(6, "LAYING", activity_names_data)


*#* A new data frame equal to "red_data" is created, but the first column is changed for the vector

*#* "activity_name_data" to accomplish the purposes described before.

activ_data <- red_data

activ_data[,1] <- activity_names_data


*#* 4.- Appropriately labels the data set with descriptive variable names.

*#* A new data frame called tidy_data is created equal to activ_data, with the difference

*#* that the column names are changed with more descriptive names.

tidy_data <- activ_data

redMeasurements <- measurements[allIndex]

names(tidy_data) <- c("Activity", "Subject", redMeasurements)

names(tidy_data) <- gsub("[^[:alnum:] ]", "", names(tidy_data))


*#* 5.- From the data set in step 4, creates a second, independent tidy data set with the 

*#* average of each variable for each activity and each subject.

*#* A new data frame called new_table is created to keep a look in the steps and the data frames created.


new_table <- tidy_data


*#* An empty list called "df" is created with 30 elements because there are 30 subjects

*#* which means that every element in the list refers to every subject.

*#* Every element in the list contains the means of the signals obtained when the subjects performed 

*#* each one the 6 different activities

df <- vector(mode="list", length = 30)

for (i in 1:30){

  prueba <- filter(new_table, Subject == i)

  

  standing <- filter(prueba, Activity == "STANDING")

  standing <- standing[,3:81]

  standMeans <- colMeans(standing)
  
  sitting <- filter(prueba, Activity == "SITTING")

  sitting <- sitting[,3:81]

  sittMeans <- colMeans(sitting)
  
  laying <- filter(prueba, Activity == "LAYING")

  laying <- laying[,3:81]

  layMeans <- colMeans(laying)
  
  walking <- filter(prueba, Activity == "WALKING")

  walking <- walking[,3:81]

  walkMeans <- colMeans(walking)
  
  walk_down <- filter(prueba, Activity == "WALKING DOWNSTAIRS")

  walk_down <- walk_down[,3:81]

  walkdownMeans <- colMeans(walk_down)
  
  walk_up <- filter(prueba, Activity == "WALKING UPSTAIRS")

  walk_up <- walk_up[,3:81]

  walkupMeans <- colMeans(walk_up)

  
  df[[i]] <- rbind(standMeans,sittMeans,layMeans,walkMeans,walkdownMeans,walkupMeans)

  
  }


*#* Every element of the "df" list is bind together with rbind() to make a data frame called "tidy_data_mean"

tidy_data_mean <- as.data.frame(rbind(df[[1]],  df[[2]],  df[[3]],  df[[4]],  df[[5]],  df[[6]],
                                      df[[7]],  df[[8]],  df[[9]],  df[[10]], df[[11]], df[[12]],
                                      df[[13]], df[[14]], df[[15]], df[[16]], df[[17]], df[[18]],
                                      df[[19]], df[[20]], df[[21]], df[[22]], df[[23]], df[[24]],
                                      df[[25]], df[[26]], df[[27]], df[[28]], df[[29]], df[[30]]))

*#* final_tidy_data is the last data frame to be created, because it will store the result of the step 5

final_tidy_data <- tidy_data_mean

row.names(final_tidy_data) <- NULL

*#* Here are created the missing columns ("Activity" and "Subject")

Subject <- rep(1:30, each = 6)

act <- c("STANDING","SITTING","LAYING","WALKING","WALKING DOWNSTAIRS","WALKING UPSTAIRS")

Activity <- rep(act, times = 30)

*#* Reseting the working directory

setwd(dir)

*#* All the columns are bind together with cbind() to final_tidy_data

*#* It is worth to mention that all the values of this table are mean values, with the exception of

*#* the first 2 columns.

final_tidy_data <- cbind(Subject, Activity, final_tidy_data)

}
