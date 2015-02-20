# You should create one R script called run_analysis.R that does the following.
# STEP 1.Merges the training and the test sets to create one data set.
# STEP 2.Extracts only the measurements on the mean and standard deviation for each measurement.
# STEP 3.Uses descriptive activity names to name the activities in the data set
# STEP 4.Appropriately labels the data set with descriptive variable names.
# STEP 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


library(plyr)
library(dplyr)
library(knitr)


# STEP 1.Merges the training and the test sets to create one data set.
cat("[RunAnalysis.R] - RUNNING STEP 1. Merges the training and the test sets to create one data set.", "\n") 
#Collect the data to create the data table to run our processing
#The data we need to create this table needs to be gathered from two sets of
#data 'test' and 'train'(stored in folders with the same respective names). 

#We will build the table column wise, a column at a time and finally
#using a column binding combine them to complete the table

#BUILDING THE FIRST DATA SET OF COLUMNS
#The first data set of columns of this table will be a concatenated listing of the data in the train/X_train.txt and test/X_test.txt

#The first data set column header with variable names as column names will be taken from features.txt. 
#Read them into a table with 2 columns
FirstColFeaturesData_ColNames <- read.table("./getdata_projectfiles_UCI HAR Dataset/features.txt",head=FALSE)

#Give descriptive column names for the table
names(FirstColFeaturesData_ColNames)<-c("Num","Names")

#The first set of rows of our column content from the training data
FirstColFeaturesData_Train <- read.table("./getdata_projectfiles_UCI HAR Dataset/train/X_train.txt",header = FALSE)

#The second set of rows of our column content from the test data
FirstColFeaturesData_Test <- read.table("./getdata_projectfiles_UCI HAR Dataset/test/X_test.txt",header = FALSE)

#Run a check of the number of rows/columns - should have same number of cols
##str(FirstColFeaturesData_ColNames)
#the 561 observations for the second column 'names' correspond to 
#the names of the 561 variables (columns) of the below to tables
##str(FirstColFeaturesData_Train)
#Has 7352 observations(rows) of 561 variables (columns)
##str(FirstColFeaturesData_Test)
#Has 2947 observations(rows) of 561 variables (columns)

#Combine the 2 data table columns - ROW WISE
FirstColFeaturesData_All<- rbind(FirstColFeaturesData_Train, FirstColFeaturesData_Test)

#Map the 561 column values from the column 'Names'
#of the FirstColFeaturesData_ColNames table to the 561 columns 
#of the combined table
names(FirstColFeaturesData_All) <- FirstColFeaturesData_ColNames$Names

#Check the properties
##str(FirstColFeaturesData_All)
#Should have 10299 (7352+2947) observations (rows) and 561 variables (columns)

#BUILDING THE SECOND DATA SET COLUMN
#The second data set column called 'Subject' will hold the identity numbers
#of the test subjects that were put through the tests in both
#'train' and 'test' routines to give us the features data
#These values are stored in the subject_train.txt and subject_test.txt
#files in the train and test folders respectively 

SecondColSubjectData_Train <- read.table("./getdata_projectfiles_UCI HAR Dataset/train/subject_train.txt",header = FALSE)

SecondColSubjectData_Test <- read.table("./getdata_projectfiles_UCI HAR Dataset/test/subject_test.txt",header = FALSE)

#Run a check of the number of rows/columns 
#should have 7352 observatons (rows) and one variable (column) - with
#the train subject's serial number
##str(SecondColSubjectData_Train)

#should have 2947 observatons (rows) and one variable (column)- with
#the test subject's serial number
##str(SecondColSubjectData_Test)

#Combine the 2 data sets - ROW WISE
#IMPORTANT - TRAIN HAS TO COME FIRST THEN FOLLOWED BY TEST
#IF NOT YOU WILL SKEW THE SUBJECT SERIAL NUMBER MAPPINGS
SecondColSubjectData_All<- rbind(SecondColSubjectData_Train, SecondColSubjectData_Test)

#Assign the variable (column) name for the second column
names(SecondColSubjectData_All) <- c("subject")

#BUILDING THE THIRD DATA SET COLUMN
#The third data set column called 'activity' will hold activity labels
#of the test subjects that were put through the tests in both
#'train' and 'test' routines to give us the features data
#These values are stored in the y_train.txt and y_test.txt
#files in the train and test folders respectively 

ThirdColActivityData_Train <- read.table("./getdata_projectfiles_UCI HAR Dataset/train/y_train.txt",header = FALSE)

ThirdColActivityData_Test <- read.table("./getdata_projectfiles_UCI HAR Dataset/test/y_test.txt",header = FALSE)

#Run a check of the number of rows/columns 
#should have 7352 observatons (rows) and one variable (column) - with
#the train subject's serial number
##str(ThirdColActivityData_Train)

#should have 2947 observatons (rows) and one variable (column)- with
#the test subject's serial number
##str(ThirdColActivityData_Test)

#Combine the 2 data sets - ROW WISE
#IMPORTANT - TRAIN data HAS TO COME FIRST THEN FOLLOWED BY TEST data
#IF NOT YOU WILL SKEW THE SUBJECT SERIAL NUMBER MAPPINGS
ThirdColActivityData_All<- rbind(ThirdColActivityData_Train, ThirdColActivityData_Test)

#Assign the variable (column) name for the second column
names(ThirdColActivityData_All) <- c("activity")



#Now column bind all the three columns to get the complete table
#we need for processing

Combined_Data_First_Second_Col <- cbind(FirstColFeaturesData_All, SecondColSubjectData_All)
Combined_Data_All_Col <- cbind(Combined_Data_First_Second_Col, ThirdColActivityData_All)

#STEP 2.Extracts only the measurements on the mean and standard deviation for each measurement.
cat("[RunAnalysis.R] - RUNNING STEP 2. Extracts only the measurements on the mean and standard deviation for each measurement.", "\n") 
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

##########################################################
#Using DPLYR
#There seem to be duplicate column names in the first data set of
#561 columns containing the features data, so we get the unique list
#DPLYR' select statement does not like that and flags an error

uniqFeaturesColNames<-unique(names(FirstColFeaturesData_All)) 
#477 columns vs 561 with duplicates
FirstColFeaturesData_All_UNQ<-subset(FirstColFeaturesData_All,select=uniqFeaturesColNames)
###FirstColFeaturesData_All_UNQ<-tbl_df(FirstColFeaturesData_All_UNQ)

#STEP 2.Extracts only the measurements on the mean and standard deviation for each measurement.
cat("[RunAnalysis.R] - RUNNING STEP 2. Extracts only the measurements on the mean and standard deviation for each measurement.", "\n") 
#Retain only the columns with names that match 'mean()' and 'std()'
FirstColFeaturesData_All_UNQ<-select(FirstColFeaturesData_All_UNQ, matches("mean\\(\\)|std\\(\\)"))
#This will leave us with 66 columns
#Add the 'subject' column
Combined_Data_First_Second_Col <- cbind(FirstColFeaturesData_All_UNQ, SecondColSubjectData_All)
#Add the 'activity' column
Combined_Data_All_Col <- cbind(Combined_Data_First_Second_Col, ThirdColActivityData_All)
DeepData<-Combined_Data_All_Col
#####################################################################

#STEP 3.Uses descriptive activity names to name the activities in the data set
cat("[RunAnalysis.R] - RUNNING STEP 3. Uses descriptive activity names to name the activities in the data set.", "\n") 

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
#activities.t$activity <- factor(activities.t$activity, levels = activities, labels = activity.names) 
#tact$activity <- factor(tact$activity, levels = activityLabels$Num, labels = activityLabels$Names) 

DeepData$activity<-factor(DeepData$activity, levels = activityLabels$Num, labels = activityLabels$Names) 

#STEP 4.Appropriately labels the data set with descriptive variable names.
cat("[RunAnalysis.R] - RUNNING STEP 4. Appropriately labels the data set with descriptive variable names.", "\n") 

names(DeepData)<-gsub("^t", "time", names(DeepData))
names(DeepData)<-gsub("^f", "frequency", names(DeepData))
names(DeepData)<-gsub("Acc", "Accelerometer", names(DeepData))
names(DeepData)<-gsub("Gyro", "Gyroscope", names(DeepData))
names(DeepData)<-gsub("Mag", "Magnitude", names(DeepData))
names(DeepData)<-gsub("BodyBody", "Body", names(DeepData))

#verify the change
#names(DeepData)
# STEP 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
cat("[RunAnalysis.R] - RUNNING STEP 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.", "\n") 


DeepDataAggregated<-aggregate(. ~subject + activity, DeepData, mean)
DeepDataTidy<-DeepDataAggregated[order(DeepDataAggregated$subject,DeepDataAggregated$activity),]
write.table(DeepDataTidy, file = "DeepDataTidy.txt",row.name=FALSE)
#FromDeepDataTidy <- read.table("./DeepDataTidy.txt",header = FALSE)


#knit2html("Runanalysis.md")
