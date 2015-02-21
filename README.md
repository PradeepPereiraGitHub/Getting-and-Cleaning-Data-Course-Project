---
title: "README.MD"
author: "Pradeep Pereira"
date: "Friday, February 20, 2015"
output: html_document
---

# Getting and Cleaning Data - Course Project

run_analysis.r script file is available here -> https://github.com/PradeepPereiraGitHub/Getting-and-Cleaning-Data-Course-Project/blob/master/run_analysis.R

code book for the project is available here -> https://github.com/PradeepPereiraGitHub/Getting-and-Cleaning-Data-Course-Project/blob/master/CodeBook.md

The script does the following in sequence as per the assignment instructions

### You should create one R script called run_analysis.R that does the following.


### STEP 1. Merges the training and the test sets to create one data set.

Final data set is constructed in 3 parts.

The rows of the data table of part 1 of the final data set are created from the data in the train/X_train.txt and test/X_test.txt files. It will have multiple columns with names taken from the 'features' file. The data is convatenated/bound row wise.

Create a vector storing the column names of all the 'features' measurements for part 1 the final data set taken from features.txt file. The column names for part 1 of the final data set are written using this vector

Part 2 of the final data set will be a single column data table called 'subject' will hold the identity of the test subjects that were put through the tests in both 'train' and 'test' routines to give us the 'features' data.

These values are extracted from the subject_train.txt and subject_test.txt files in the 'train' and 'test' folders respectively. The data is concatenated/bound row wise.

Part 3 of the final data set will be a single column data table 'activity' will hold activity labels of the test subjects that were put through the tests in both
'train' and 'test' routines to give us the features data.

These values are extracted from the y_train.txt and y_test.txt files in the train and test folders respectively. The data is concatenated/bound row wise.

Part 1, 2 and 3 are then bound column wise to a data table which is ou final data set

### STEP 2. Extracts only the measurements on the mean and standard deviation for each measurement.

Selectively keep only the columns from part one by removing all columns by name that do not have a mean or standard deviation measurement listing (column names that dont have the 'mean()' or 'std()' string in them)

### STEP 3. Uses descriptive activity names to name the activities in the data set

There are 6 activities the test subjects engaged in - read them from the activity_labels.txt file and use that to map them to their respective numerical identifiers
1 => WALKING    2 => WALKING_UPSTAIRS  3 => WALKING_DOWNSTAIRS
4 => SITTING    5 => STANDING          6 => LAYING  

Replace the numerical identifiers for the activities listed in the activity column of the final data set with the decriptive labels as above.          

### STEP 4. Appropriately labels the data set with descriptive variable names.

Replace abbreviated words in the activity names specified in the column names of the final data set with decriptive and readable names. (t is replaced with time, f is replaced with frequency, acc with accelerometer, gyro is replaced with gyroscope etc)

### STEP 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Creates the final tidy data from the final data set (created in the previous step) by first aggregating it on the subject and activity values and getting the mean value across all of them on all selected measurements. Then ordering the aggregated data set by subject and activity and writing the final tidy data set to a text file.

