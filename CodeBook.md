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

Data Files Used In Processing:

./getdata_projectfiles_UCI HAR Dataset/features.txt
./getdata_projectfiles_UCI HAR Dataset/train/X_train.txt
./getdata_projectfiles_UCI HAR Dataset/test/X_test.txt
./getdata_projectfiles_UCI HAR Dataset/train/y_train.txt
./getdata_projectfiles_UCI HAR Dataset/test/y_test.txt
./getdata_projectfiles_UCI HAR Dataset/train/subject_train.txt
./getdata_projectfiles_UCI HAR Dataset/test/subject_test.txt
./getdata_projectfiles_UCI HAR Dataset/activity_labels.txt

```{r}
summary(cars)
```

You can also embed plots, for example:

```{r, echo=FALSE}
plot(cars)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
