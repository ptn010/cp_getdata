## READ ME

The run_analysis.R is written in completing as course project of "getting and cleaning data" at Coursera.
The raw data is retrieved from Human Activity Recognition Using Smartphones Dataset Version 1.0 (Smartlab - Non Linear Complex Systems Laboratory,www.smartlab.ws), resource provided by the project assignment.


This R script called run_analysis.R that does the following. 
- S1.Merges the training and the test sets to create one data set.
- S2.Extracts only the measurements on the mean and standard deviation for each measurement. 
- S3.Uses descriptive activity names to name the activities in the data set
- S4.Appropriately labels the data set with descriptive variable names. 
- S5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


### Step 0. Preparation: Read Data and load libraries
In respective directories (train and test), retrieve data (X), features(y), subject(subject),and activity labels from corresponding files indicated in the data package.

```r
#load training data
#--Training set
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt", header=F,sep="")
#--Training labels
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt", header=F,sep="")

#load test data
#--Test set
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt", header=F,sep="")
#--Test labels
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt", header=F,sep="")

#load subject identification
#-Each row identifies the subject who performed the activity for each window sample
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = c("Subject"))
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names=c("Subject"))

#load feature and activity label
features<-read.table("./UCI HAR Dataset/features.txt", header=F,sep="")
act_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.0.3
```

```r
library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 3.0.3
```


### S1.Merges the training and the test sets to create one data set.
A labeling of "training" and "test" was added here for the sake of completeness.However this label was not used anywhere for the purpose of this assignment.

```r
X_One<-rbind(cbind(X_train, "Set"=c("Training")), cbind(X_test, "Set"=c("Test")))
y_One<-rbind(y_train, y_test)
subject_One<-rbind(subject_train,subject_test)
```


### S2.Extracts only the measurements on the mean and standard deviation for each measurement. 

```r
#extract feature according to label 
f_extracted<-grepl("mean|std", features[,2])
X_use<- X_One[, c(f_extracted,T)]
```

### S3.Uses descriptive activity names to name the activities in the data set

```r
y_One_dscp<-join(y_One, act_labels,  match="first")
#using join() to preserve row order in y_One
```


### S4.Appropriately labels the data set with descriptive variable names. 

```r
names(y_One_dscp)=c("ActCode","ActLabel")
names(X_use)=features[f_extracted,2]
names(X_use)[80]<-"Set"
X_use$ActivityCode<-y_One_dscp[,1]
X_use$Activity<-y_One_dscp[,2]
X_use$Subject<-subject_One[,1]
```

### S5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


```r
means<-aggregate(X_use, by=list(X_use$Activity, X_use$Subject),mean)
#Since training and testing datasets were randomly selected from the same set 
#with same index of subjects, "Set" is not used as a factor in by=list()

tidy<-means[,1:(ncol(means)-4)]
colnames(tidy)[1] <- "Activity"
colnames(tidy)[2] <- "Subject"

write.table(tidy, file='proj_tidydata.txt', row.names=FALSE)
```

