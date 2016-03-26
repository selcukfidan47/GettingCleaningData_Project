Getting and Cleaning Data Project
================

### Description

Additional information about the variables, data and transformations used in the course project for the Getting and Cleaning Data Course.

### Source Data

A full description of the data used in this project can be found at [<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>](hhttp://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

[<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

### Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING\_UPSTAIRS, WALKING\_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Attribute Information

For each record in the dataset it is provided: - Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. - Triaxial Angular velocity from the gyroscope. - A 561-feature vector with time and frequency domain variables. - Its activity label. - An identifier of the subject who carried out the experiment.

### The dataset includes the following files:

-   `README.txt`

-   `features_info.txt`: Shows information about the variables used on the feature vector.

-   `features.txt`: List of all features.

-   `activity_labels.txt`: Links the class labels with their activity name.

-   `train/X_train.txt`: Training set.

-   `train/y_train.txt`: Training labels.

-   `test/X_test.txt`: Test set.

-   `test/y_test.txt`: Test labels.

The following files are available for the train and test data. Their descriptions are equivalent.

-   `train/subject_train.txt`: Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.

-   `train/Inertial Signals/total_acc_x_train.txt`: The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the `total_acc_x_train.txt` and `total_acc_z_train.txt'` files for the Y and Z axis.

-   `train/Inertial Signals/body_acc_x_train.txt`: The body acceleration signal obtained by subtracting the gravity from the total acceleration.

-   `train/Inertial Signals/body_gyro_x_train.txt`: The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second.

### Notes:

-   Features are normalized and bounded within [-1,1].
-   Each feature vector is a row on the text file.

For more information about this dataset contact: [<activityrecognition@smartlab.ws>](activityrecognition@smartlab.ws)

Section 1. Merge the training and the test sets to create one data set.
-----------------------------------------------------------------------

``` r
# install packages
require("plyr")       ##need for join()
```

    ## Loading required package: plyr

``` r
require("reshape2")   ##need for melt() and dcast()
```

    ## Loading required package: reshape2

``` r
# load packages
library(plyr)         
library(reshape2)     
```

``` r
# 1.Merges the test and the training sets to create one data set.
y.test<-(read.table("./test/y_test.txt"))
table(y.test)
```

    ## y.test
    ##   1   2   3   4   5   6 
    ## 496 471 420 491 532 537

``` r
x.test<- (read.table("./test/X_test.txt"))
dim(x.test)
```

    ## [1] 2947  561

``` r
subject.test<- read.table("./test/subject_test.txt",col.names=c("Subject")) 

# read in train dataset
y.train<- read.table("./train/y_train.txt") 
table(y.train)
```

    ## y.train
    ##    1    2    3    4    5    6 
    ## 1226 1073  986 1286 1374 1407

``` r
x.train<- read.table("./train/X_train.txt")
dim(x.train)
```

    ## [1] 7352  561

``` r
subject.train<- read.table("./train/subject_train.txt", col.names=c("Subject")) 

####### now combine train and test sets into the single data frame
# combine y.test and y.train
y.test.train.combined<-rbind(y.test,y.train)
dim(y.test.train.combined)
```

    ## [1] 10299     1

``` r
# combine x.test and x.train
x.test.train.combined<-rbind(x.test,x.train)
# combine subject.test and subject.train
subject.test.train.combined<-rbind(subject.test,subject.train)
```

Section 2. Extract only the measurements on the mean and standard deviation for each measurement.
-------------------------------------------------------------------------------------------------

``` r
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
```

Section 3. Use descriptive activity names to name the activities in the data set
--------------------------------------------------------------------------------

``` r
#3.Uses descriptive activity names to name the activities in the data set

activity.labels<-read.table("activity_labels.txt",sep=" ",col.names=c("activityLabel","Activity"))
activity.labels[, 2] <- tolower(gsub("_", "", activity.labels[, 2]))

y.test.train.combined<-join(y.test.train.combined,activity.labels,by="activityLabel",type="left")
#Drop activity numbers
y.test.train.combined$activityLabel <- NULL
```

Section 4. Appropriately label the data set with descriptive activity names.
----------------------------------------------------------------------------

``` r
# Step4. Appropriately labels the data set with descriptive activity names.         
#Combine Actitivies, Subjects and Features all into one data frame
all.combined.df <- cbind(x.test.train.combined, y.test.train.combined, subject.test.train.combined)
dim(all.combined.df)   
```

    ## [1] 10299    88

Section 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
---------------------------------------------------------------------------------------------------------------------------

``` r
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#Melt data frame for reshaping
all.combined.df.reshaped <- melt(all.combined.df, id = c("Subject", "Activity"), measure.vars=feature.list)
dim(all.combined.df.reshaped)          
```

    ## [1] 885714      4

``` r
#Reshape into tidy data frame by mean using the reshape2 package
all.combined.df.reshaped <- dcast(all.combined.df.reshaped, Activity + Subject ~ variable, mean)
dim(all.combined.df.reshaped)        
```

    ## [1] 180  88

``` r
#Reorder by Subject then Activity
all.combined.df.reshaped <- all.combined.df.reshaped[order(all.combined.df.reshaped$Subject, all.combined.df.reshaped$Activity),]
dim(all.combined.df.reshaped)        
```

    ## [1] 180  88

``` r
#Reindex Rows and move Subject to Column 1
rownames(all.combined.df.reshaped) <- 1:nrow(all.combined.df.reshaped)
all.combined.df.reshaped <- all.combined.df.reshaped[,c(2,1,3:68)]
dim(all.combined.df.reshaped)    
```

    ## [1] 180  68

``` r
names(all.combined.df.reshaped)     
```

    ##  [1] "Subject"               "Activity"             
    ##  [3] "tBodyAccMeanX"         "tBodyAccMeanY"        
    ##  [5] "tBodyAccMeanZ"         "tBodyAccStdX"         
    ##  [7] "tBodyAccStdY"          "tBodyAccStdZ"         
    ##  [9] "tGravityAccMeanX"      "tGravityAccMeanY"     
    ## [11] "tGravityAccMeanZ"      "tGravityAccStdX"      
    ## [13] "tGravityAccStdY"       "tGravityAccStdZ"      
    ## [15] "tBodyAccJerkMeanX"     "tBodyAccJerkMeanY"    
    ## [17] "tBodyAccJerkMeanZ"     "tBodyAccJerkStdX"     
    ## [19] "tBodyAccJerkStdY"      "tBodyAccJerkStdZ"     
    ## [21] "tBodyGyroMeanX"        "tBodyGyroMeanY"       
    ## [23] "tBodyGyroMeanZ"        "tBodyGyroStdX"        
    ## [25] "tBodyGyroStdY"         "tBodyGyroStdZ"        
    ## [27] "tBodyGyroJerkMeanX"    "tBodyGyroJerkMeanY"   
    ## [29] "tBodyGyroJerkMeanZ"    "tBodyGyroJerkStdX"    
    ## [31] "tBodyGyroJerkStdY"     "tBodyGyroJerkStdZ"    
    ## [33] "tBodyAccMagMean"       "tBodyAccMagStd"       
    ## [35] "tGravityAccMagMean"    "tGravityAccMagStd"    
    ## [37] "tBodyAccJerkMagMean"   "tBodyAccJerkMagStd"   
    ## [39] "tBodyGyroMagMean"      "tBodyGyroMagStd"      
    ## [41] "tBodyGyroJerkMagMean"  "tBodyGyroJerkMagStd"  
    ## [43] "fBodyAccMeanX"         "fBodyAccMeanY"        
    ## [45] "fBodyAccMeanZ"         "fBodyAccStdX"         
    ## [47] "fBodyAccStdY"          "fBodyAccStdZ"         
    ## [49] "fBodyAccMeanFreqX"     "fBodyAccMeanFreqY"    
    ## [51] "fBodyAccMeanFreqZ"     "fBodyAccJerkMeanX"    
    ## [53] "fBodyAccJerkMeanY"     "fBodyAccJerkMeanZ"    
    ## [55] "fBodyAccJerkStdX"      "fBodyAccJerkStdY"     
    ## [57] "fBodyAccJerkStdZ"      "fBodyAccJerkMeanFreqX"
    ## [59] "fBodyAccJerkMeanFreqY" "fBodyAccJerkMeanFreqZ"
    ## [61] "fBodyGyroMeanX"        "fBodyGyroMeanY"       
    ## [63] "fBodyGyroMeanZ"        "fBodyGyroStdX"        
    ## [65] "fBodyGyroStdY"         "fBodyGyroStdZ"        
    ## [67] "fBodyGyroMeanFreqX"    "fBodyGyroMeanFreqY"

``` r
#Output file
write.table(all.combined.df.reshaped,file="tidy_dataset.txt")
```
