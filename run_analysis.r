# Selcuk Fidan
# Getting Cleaning Data Course Project

#############################################################################
# clear the Environment
rm(list=ls())
# install packages
install.packages("plyr")       ##need for join()
install.packages("reshape2")   ##need for melt() and dcast()
# load packages
library(plyr)         
library(reshape2)     
# set working directory
setwd("C:/Users/Selcuk Fidan/Desktop/0_Programming_Assignment/GettingCleaningData_CourseProject")
if(!file.exists("./dataStore")){dir.create("./dataStore")}
# activity monitoring data
get.data.project <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(get.data.project,destfile="./dataStore/getdata-projectfiles-UCI HAR Dataset.zip",method="auto")  

  # make sure the site is live, if it is not live stop function terminate the program
  check.url <- file(get.data.project,"r")
  if (!isOpen(check.url)) {
    stop(paste("There's a problem with the data:",geterrmessage()))
  }
  # zipfile.data is the variable to keep the *.zip file
  zipfile.data = "getdata-projectfiles-UCI HAR Dataset.zip"
  
  # make sure the data in the working directory if not download the zip file into the to zipfile.data and unzip the zipfile.data
  if(!file.exists(zipfile.data)) {        
        # download.file(get.data.project,zipfile.data)
        unzip(zipfile="./dataStore/getdata-projectfiles-UCI HAR Dataset.zip",exdir="./dataStore")
   } 
path_rf <- file.path("./dataStore" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)
files

setwd("C:/Users/Selcuk Fidan/Desktop/0_Programming_Assignment/GettingCleaningData_CourseProject/dataStore/UCI HAR Dataset")
#You should create one R script called run_analysis.R that does the following. 
##############################################################################
#1.Merges the test and the training sets to create one data set.
y.test<-(read.table("./test/y_test.txt"))
table(y.test)
x.test<- (read.table("./test/X_test.txt"))
dim(x.test)
head(x.test)
subject.test<- read.table("./test/subject_test.txt",col.names=c("Subject")) 

# read in train dataset
y.train<- read.table("./train/y_train.txt") 
table(y.train)
x.train<- read.table("./train/X_train.txt")
dim(x.train)
head(x.train)
subject.train<- read.table("./train/subject_train.txt", col.names=c("Subject")) 

####### now combine train and test sets into the single data frame
# combine y.test and y.train
y.test.train.combined<-rbind(y.test,y.train)
dim(y.test.train.combined)
# combine x.test and x.train
x.test.train.combined<-rbind(x.test,x.train)
# combine subject.test and subject.train
subject.test.train.combined<-rbind(subject.test,subject.train)
#####################################################################################
#====================================================================================
#####################################################################################
# 2.Extracts only the measurements on the mean and standard deviation for each measurement.

feature.list <- read.table("features.txt", col.names=c("index", "feature_labels"))
feature.list[,2] = gsub('-mean', 'Mean', feature.list[,2])
feature.list[,2] = gsub('-std', 'Std', feature.list[,2])
feature.list[,2] = gsub('[-()]', '', feature.list[,2])
feature.labels <- feature.list$feature_labels
features.subset <- grepl('Mean|Std',feature.labels)
feature.list <- as.character(feature.labels[features.subset])
colnames(x.test.train.combined) <- feature.labels
x.test.train.combined <- x.test.train.combined[,features.subset]
colnames(y.test.train.combined) <- "activityLabel"
#####################################################################################
#====================================================================================
#####################################################################################
#3.Uses descriptive activity names to name the activities in the data set

activity.labels<-read.table("activity_labels.txt",sep=" ",col.names=c("activityLabel","Activity"))
activity.labels[, 2] <- tolower(gsub("_", "", activity.labels[, 2]))

y.test.train.combined<-join(y.test.train.combined,activity.labels,by="activityLabel",type="left")
#Drop activity numbers
y.test.train.combined$activityLabel <- NULL
#####################################################################################
#====================================================================================
#####################################################################################
# Step4. Appropriately labels the data set with descriptive activity names.         
#Combine Actitivies, Subjects and Features all into one data frame
all.combined.df <- cbind(x.test.train.combined, y.test.train.combined, subject.test.train.combined)
dim(all.combined.df)   

#####################################################################################
#====================================================================================
#####################################################################################
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#Melt data frame for reshaping
all.combined.df.reshaped <- melt(all.combined.df, id = c("Subject", "Activity"), measure.vars=feature.list)
dim(all.combined.df.reshaped)          
#Reshape into tidy data frame by mean using the reshape2 package
all.combined.df.reshaped <- dcast(all.combined.df.reshaped, Activity + Subject ~ variable, mean)
dim(all.combined.df.reshaped)        
#Reorder by Subject then Activity
all.combined.df.reshaped <- all.combined.df.reshaped[order(all.combined.df.reshaped$Subject, all.combined.df.reshaped$Activity),]
dim(all.combined.df.reshaped)        
#Reindex Rows and move Subject to Column 1
rownames(all.combined.df.reshaped) <- 1:nrow(all.combined.df.reshaped)
all.combined.df.reshaped <- all.combined.df.reshaped[,c(2,1,3:68)]
dim(all.combined.df.reshaped)    
names(all.combined.df.reshaped)     
#Output file
write.table(all.combined.df.reshaped,file="tidyDataset.txt",row.name=FALSE)
