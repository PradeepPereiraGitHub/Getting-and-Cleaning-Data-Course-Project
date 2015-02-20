---
title: "Codebook.MD"
author: "Pradeep Pereira"
date: "Friday, February 20, 2015"
output: html_document
---

##Data Processing Pre-Requisities:

The data for this assignment is available for download at

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

1. The above file is to be downloaded and unzipped to the default data directory for a standard R Studio install.

2. All files for the project are to be extracted using the default directory setting while un-zipping. This will create the following parent directory in the above mentioned default data directory
.\getdata_projectfiles_UCI HAR Dataset.

3. All processing in the runanalysis.R script has been coded to source the requisite data files from the above directory.

##Data Files Used In Processing:

a. The data available to us is from a sequence of different tests across different 'features' - the names of which are provided in the 'features' file (below)


```{r}
./getdata_projectfiles_UCI HAR Dataset/features.txt
```

b. The readings for these tests for the 'features' mentioned above have been measured in both a 'train' and 'test' environments - values for which are provided in the 'X_train' and 'X_test' files (below)

Note: For our assigment we will be extracting selected features - where the feature name contains 'mean()' and 'std()' only. The rest will be ignored.


```{r}
./getdata_projectfiles_UCI HAR Dataset/train/X_train.txt
./getdata_projectfiles_UCI HAR Dataset/test/X_test.txt

```

c. These measurements have been carried out on 'subjects' who have been identified by a serial number across both the 'test' and 'train' environments and are available in the 'subject_train' and 'subject_test' files (below)


```{r}
./getdata_projectfiles_UCI HAR Dataset/train/subject_train.txt
./getdata_projectfiles_UCI HAR Dataset/test/subject_test.txt

```

d. The 'subjects' have been put through the tests for the above mentioned  'features' being measured across various types of 'activities' that are identified by number - 1 through 6 - and available in the 'y_train' and 'y_test' files (below)

```{r}
./getdata_projectfiles_UCI HAR Dataset/train/y_train.txt
./getdata_projectfiles_UCI HAR Dataset/test/y_test.txt

```

e. The above 'activities' identified by number need to be mapped to a descriptive name (STANDING,WALKING etc) which is provided in the 'activity_labels' file (below)

```{r}
./getdata_projectfiles_UCI HAR Dataset/activity_labels.txt

```

##Data Processing Sequence:

We will build 3 different data sets and combine them for our final data set

###The First Data Set:

We first build a data table of the 'train' and 'test' data with ALL the 'features' as the column names

```{r}
#BUILDING THE FIRST DATA SET OF COLUMNS
#The first data set of columns of this table will be a concatenated listing of the data in the train/X_train.txt and test/X_test.txt

#The first data set column header with variable names as column names will be taken from features.txt 
#Read them into a table with 2 columns

FirstColFeaturesData_ColNames <- read.table("./getdata_projectfiles_UCI HAR Dataset/features.txt",head=FALSE)

#Give descriptive column names for the table
names(FirstColFeaturesData_ColNames)<-c("Num","Names")

#The first set of rows of our column content from the training data
FirstColFeaturesData_Train <- read.table("./getdata_projectfiles_UCI HAR Dataset/train/X_train.txt",header = FALSE)

#The second set of rows of our column content from the test data
FirstColFeaturesData_Test <- read.table("./getdata_projectfiles_UCI HAR Dataset/test/X_test.txt",header = FALSE)

#Combine the 2 data table columns - ROW WISE
FirstColFeaturesData_All<- rbind(FirstColFeaturesData_Train, FirstColFeaturesData_Test)

#Map the 561 column values from the column 'Names'
#of the FirstColFeaturesData_ColNames table to the 561 columns 
#of the combined table
names(FirstColFeaturesData_All) <- FirstColFeaturesData_ColNames$Names

```

###The Second Data Set:

We next build the second to data set of the 'subject' data as below.

```{r}
#BUILDING THE SECOND DATA SET COLUMN
#The second data set column called 'Subject' will hold the identity numbers
#of the test subjects that were put through the tests in both
#'train' and 'test' routines to give us the features data
#These values are stored in the subject_train.txt and subject_test.txt
#files in the train and test folders respectively 

SecondColSubjectData_Train <- read.table("./getdata_projectfiles_UCI HAR Dataset/train/subject_train.txt",header = FALSE)

SecondColSubjectData_Test <- read.table("./getdata_projectfiles_UCI HAR Dataset/test/subject_test.txt",header = FALSE)

#Combine the 2 data sets - ROW WISE
#IMPORTANT - TRAIN HAS TO COME FIRST THEN FOLLOWED BY TEST
#IF NOT YOU WILL SKEW THE SUBJECT IDENTITY NUMBER MAPPINGS
SecondColSubjectData_All<- rbind(SecondColSubjectData_Train, SecondColSubjectData_Test)

#Assign the variable (column) name for the column
names(SecondColSubjectData_All) <- c("subject")


```

###The Third Data Set:

We next build the third data set of the 'activity' data as below.


```{r}
#BUILDING THE THIRD DATA SET COLUMN
#The third data set column called 'activity' will hold activity labels
#of the test subjects that were put through the tests in both
#'train' and 'test' routines to give us the features data
#These values are stored in the y_train.txt and y_test.txt
#files in the train and test folders respectively 

ThirdColActivityData_Train <- read.table("./getdata_projectfiles_UCI HAR Dataset/train/y_train.txt",header = FALSE)

ThirdColActivityData_Test <- read.table("./getdata_projectfiles_UCI HAR Dataset/test/y_test.txt",header = FALSE)

#Combine the 2 data sets - ROW WISE
#IMPORTANT - TRAIN data HAS TO COME FIRST THEN FOLLOWED BY TEST data
#IF NOT YOU WILL SKEW THE SUBJECT SERIAL NUMBER MAPPINGS
ThirdColActivityData_All<- rbind(ThirdColActivityData_Train, ThirdColActivityData_Test)

#Assign the variable (column) name for the column
names(ThirdColActivityData_All) <- c("activity")

```

###Combine the above 3 sets of data into our final data set - column wise

```{r}
#Now column bind all the three data sets to get the complete table
#we need for processing

Combined_Data_First_Second_Col <- cbind(FirstColFeaturesData_All, SecondColSubjectData_All)
Combined_Data_All_Col <- cbind(Combined_Data_First_Second_Col, ThirdColActivityData_All)

```

##Tidying Up:

###Step 1:
For our assigment we need to be using only mean and standard deviation measurements so we now need to extract only the 'features' that contain the 'mean()' and 'std()' test values (column names that contain the occurences of 'mean()' and 'std()')

```{r}
#Now extract only the columns we need for our processing
#Those are the ones with the string 'mean()' and string 'std()' in them
#This will be in our first column header values
#these were extracted from the features.txt file and the
#column was named "Names"

subset_ColNames_We_Need<-FirstColFeaturesData_ColNames$Names[grep("mean\\(\\)|std\\(\\)", FirstColFeaturesData_ColNames$Names)]

#Keep the above column names from the features in the first column
#we built along with the second (subject) and third (activity) columns we built
subset_ColNames_To_Keep<-c(as.character(subset_ColNames_We_Need), "subject", "activity" )

#Use the above vector of column names to extract a subset of data
#we need for our processing

Combined_Data_All_Col_Subset<-subset(Combined_Data_All_Col,select=subset_ColNames_To_Keep)
DeepData<-Combined_Data_All_Col_Subset

```

###We now have a combined and complete data set as a table which we can process for our tidy data set.

###Step 2:

We next map our acivity labels to their respective identifying numbers, so as to get a decriptive label for the activity the subjects were engaged in.

```{r}
#Get the activity labels to map and replace the 'activity' column
#number values. 
#There are 6 activities the test subjects engaged in - read them from
#the activity_labels.txt file
#[1] WALKING    WALKING_UPSTAIRS   WALKING_DOWNSTAIRS
#[4] SITTING    STANDING           LAYING            

activityLabels <- read.table("./getdata_projectfiles_UCI HAR Dataset/activity_labels.txt",head=FALSE)

#Rename the columns from V1 and V2 to "Num" and "Names" respy
names(activityLabels)<-c("Num","Names")

#Map the activity by number to the activity name for the 'activity' column
DeepData$activity<-factor(DeepData$activity, levels = activityLabels$Num, labels = activityLabels$Names) 

```

###Step 3:
Next we need to substitute descriptive readable column names for the existing cryptic column names that are based on the 'features' names

```{r}
names(DeepData)<-gsub("^t", "time", names(DeepData))
names(DeepData)<-gsub("^f", "frequency", names(DeepData))
names(DeepData)<-gsub("Acc", "Accelerometer", names(DeepData))
names(DeepData)<-gsub("Gyro", "Gyroscope", names(DeepData))
names(DeepData)<-gsub("Mag", "Magnitude", names(DeepData))
names(DeepData)<-gsub("BodyBody", "Body", names(DeepData))

```

###Step 4:
Now rearrange the above data set by the average of each 'feature' variable for each activity and each subject 


```{r}
DeepDataAggregated<-aggregate(. ~subject + activity, DeepData, mean)
DeepDataTidy<-DeepDataAggregated[order(DeepDataAggregated$subject,DeepDataAggregated$activity),]
```

##ARE WE THERE YET??? - FINALLY, YES!!! 

As our final step, write this data set to a table and save it as a text file

```{r}
write.table(DeepDataTidy, file = "DeepDataTidy.txt",row.name=FALSE)

```
