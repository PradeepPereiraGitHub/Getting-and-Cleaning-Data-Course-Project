---
title: "Codebook.MD"
author: "Pradeep Pereira"
date: "Friday, February 20, 2015"
output: html_document
---
ALL INSTRUCTIONS GIVEN BELOW ARE FOR A WINDOWS 8 OPERATING SYSTEM

Data Processing Pre-Requisities:

The data for this assignment is available at

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

1. The above file is to be downloaded and unzipped to the default data directory (C:\Users\<USER NAME>\Documents) - for a standard R Studio install.

2. All files for the project are to be extracted using the default directory setting while un-zipping. This will create the following parent directory
C:\Users\<USER NAME>\Documents\getdata_projectfiles_UCI HAR Dataset.

3. All processing in the runanalysis.R script has been coded to source the requisite data files from the above directory.


This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
summary(cars)
```

You can also embed plots, for example:

```{r, echo=FALSE}
plot(cars)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
