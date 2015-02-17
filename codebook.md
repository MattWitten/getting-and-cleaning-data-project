---
title: "codebook"
author: "Matt Witten"
date: "Monday, February 16, 2015"
output: html_document
---

run_analysis.R
matt.witten

Mon Feb 16 22:50:25 2015

#load the tidy, sqldf and dplyr packages that will be used 
#library(tidyr)
library(sqldf)
## Warning: package 'sqldf' was built under R version 3.1.2
## Loading required package: gsubfn
## Warning: package 'gsubfn' was built under R version 3.1.2
## Loading required package: proto
## Warning: package 'proto' was built under R version 3.1.1
## Loading required package: RSQLite
## Warning: package 'RSQLite' was built under R version 3.1.2
## Loading required package: DBI
## Warning: package 'DBI' was built under R version 3.1.2
#library(dplyr)
library(data.table)
## Warning: package 'data.table' was built under R version 3.1.2
#make a tempspace to download zip file to
temp <- tempfile()

#do this to make markdown work?
setInternet2(use = TRUE)


#download zip file
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)


#get all the data needed from the folders

xtrain <- read.table(unzip(temp, "UCI HAR Dataset/train/X_train.txt"))
xtest <- read.table(unzip(temp, "UCI HAR Dataset/test/X_test.txt"))
ytrain <- read.table(unzip(temp, "UCI HAR Dataset/train/y_train.txt"))
ytest <- read.table(unzip(temp, "UCI HAR Dataset/test/y_test.txt"))
subjecttest <- read.table(unzip(temp, "UCI HAR Dataset/test/subject_test.txt"))
subjecttrain <- read.table(unzip(temp, "UCI HAR Dataset/train/subject_train.txt"))

#I believe this gives me column names etc. for part 2
features <- read.table(unzip(temp, "UCI HAR Dataset/features.txt")) 

#get activity labels to join in for part 3
activity_labels <- read.table(unzip(temp, "UCI HAR Dataset/activity_labels.txt")) 


#get rid of temp connection when done, per the internet
unlink(temp) 


#1 Merges the training and the test sets to create one data set. added a column source just in case I need it later
xtesttrain <- sqldf("select *,'test' as source from xtest union all select *,'train' as source from xtrain")
## Loading required package: tcltk
ytesttrain <- sqldf("select * from ytest union all select * from ytrain")
subjecttesttrain <- sqldf("select V1 as subject from subjecttest union all select V1 as subject from subjecttrain")


#add source to features
features <- rbind(features, data.frame(V1 = 562, V2 = "Source"))

#makes everythine lower case to simplify parrt 2 later
features <- data.frame(sapply(features, tolower))

#add column names to xtrain and xtest
names(xtesttrain) = features[,2]

#2 Extracts only the measurements on the mean and standard deviation for each measurement. 
xtesttraintwo <- xtesttrain[ , grepl( "mean" , names(xtesttrain ))|grepl( "std" , names(xtesttrain )) ]
# get rid of meanfreq and angles
xtesttraintwo <- xtesttraintwo[ , !grepl( "angle" , names(xtesttraintwo))]
xtesttraintwo <- xtesttraintwo[ , !grepl( "meanfreq" , names(xtesttraintwo))]

#3 Uses descriptive activity names to name the activities in the data set
ytesttrain_activity_labels <- sqldf("select V2 as activity from ytesttrain Y inner join activity_labels A on Y.V1 = A.V1")


#Add primary keys to data sets
id1 <- 1:nrow(xtesttraintwo)
id2 <- 1:nrow(ytesttrain_activity_labels)
id3 <- 1:nrow(subjecttesttrain)

xtesttraintwo <- cbind(id=id1, xtesttraintwo)
ytesttrain_activity_labels <- cbind(id=id2, ytesttrain_activity_labels)
subjecttesttrain <- cbind(id=id3, subjecttesttrain)


#Join The Three Data sets Together
subtotal <- merge(subjecttesttrain,ytesttrain_activity_labels,by="id")
fulltesttrain <- merge(subtotal,xtesttraintwo,by="id")



#4 Appropriately labels the data set with descriptive variable names. 

# The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. 
# These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. 
# Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. 
# Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) 
# using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
# Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). 
# Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm 
#(tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
# Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing 
# fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

names(fulltesttrain) <-gsub("^t", "time_", names(fulltesttrain))
names(fulltesttrain) <-gsub("^f", "frequency_", names(fulltesttrain))
names(fulltesttrain) <-gsub("gyro", "_gyroscope_", names(fulltesttrain))
names(fulltesttrain) <-gsub("acc", "_accelerometer_", names(fulltesttrain))
names(fulltesttrain) <-gsub("mag", "_magnitude_", names(fulltesttrain))
names(fulltesttrain) <-gsub("jerk", "jerk_signal_", names(fulltesttrain))
names(fulltesttrain) <-gsub("bodybody", "body", names(fulltesttrain))
names(fulltesttrain) <-gsub("-", "", names(fulltesttrain))



#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

testtraintable <- data.table(fulltesttrain)
testtraintable <- testtraintable[, lapply(.SD, mean), by = list(activity, subject)]
write.table(testtraintable, file = "tidydata.txt", append = FALSE, quote = TRUE, sep = ",",col.names = TRUE, row.name=FALSE)



# final output contains the following columns
names(testtraintable)
##  [1] "activity"                                                  
##  [2] "subject"                                                   
##  [3] "id"                                                        
##  [4] "time_body_accelerometer_mean()x"                           
##  [5] "time_body_accelerometer_mean()y"                           
##  [6] "time_body_accelerometer_mean()z"                           
##  [7] "time_body_accelerometer_std()x"                            
##  [8] "time_body_accelerometer_std()y"                            
##  [9] "time_body_accelerometer_std()z"                            
## [10] "time_gravity_accelerometer_mean()x"                        
## [11] "time_gravity_accelerometer_mean()y"                        
## [12] "time_gravity_accelerometer_mean()z"                        
## [13] "time_gravity_accelerometer_std()x"                         
## [14] "time_gravity_accelerometer_std()y"                         
## [15] "time_gravity_accelerometer_std()z"                         
## [16] "time_body_accelerometer_jerk_signal_mean()x"               
## [17] "time_body_accelerometer_jerk_signal_mean()y"               
## [18] "time_body_accelerometer_jerk_signal_mean()z"               
## [19] "time_body_accelerometer_jerk_signal_std()x"                
## [20] "time_body_accelerometer_jerk_signal_std()y"                
## [21] "time_body_accelerometer_jerk_signal_std()z"                
## [22] "time_body_gyroscope_mean()x"                               
## [23] "time_body_gyroscope_mean()y"                               
## [24] "time_body_gyroscope_mean()z"                               
## [25] "time_body_gyroscope_std()x"                                
## [26] "time_body_gyroscope_std()y"                                
## [27] "time_body_gyroscope_std()z"                                
## [28] "time_body_gyroscope_jerk_signal_mean()x"                   
## [29] "time_body_gyroscope_jerk_signal_mean()y"                   
## [30] "time_body_gyroscope_jerk_signal_mean()z"                   
## [31] "time_body_gyroscope_jerk_signal_std()x"                    
## [32] "time_body_gyroscope_jerk_signal_std()y"                    
## [33] "time_body_gyroscope_jerk_signal_std()z"                    
## [34] "time_body_accelerometer__magnitude_mean()"                 
## [35] "time_body_accelerometer__magnitude_std()"                  
## [36] "time_gravity_accelerometer__magnitude_mean()"              
## [37] "time_gravity_accelerometer__magnitude_std()"               
## [38] "time_body_accelerometer_jerk_signal__magnitude_mean()"     
## [39] "time_body_accelerometer_jerk_signal__magnitude_std()"      
## [40] "time_body_gyroscope__magnitude_mean()"                     
## [41] "time_body_gyroscope__magnitude_std()"                      
## [42] "time_body_gyroscope_jerk_signal__magnitude_mean()"         
## [43] "time_body_gyroscope_jerk_signal__magnitude_std()"          
## [44] "frequency_body_accelerometer_mean()x"                      
## [45] "frequency_body_accelerometer_mean()y"                      
## [46] "frequency_body_accelerometer_mean()z"                      
## [47] "frequency_body_accelerometer_std()x"                       
## [48] "frequency_body_accelerometer_std()y"                       
## [49] "frequency_body_accelerometer_std()z"                       
## [50] "frequency_body_accelerometer_jerk_signal_mean()x"          
## [51] "frequency_body_accelerometer_jerk_signal_mean()y"          
## [52] "frequency_body_accelerometer_jerk_signal_mean()z"          
## [53] "frequency_body_accelerometer_jerk_signal_std()x"           
## [54] "frequency_body_accelerometer_jerk_signal_std()y"           
## [55] "frequency_body_accelerometer_jerk_signal_std()z"           
## [56] "frequency_body_gyroscope_mean()x"                          
## [57] "frequency_body_gyroscope_mean()y"                          
## [58] "frequency_body_gyroscope_mean()z"                          
## [59] "frequency_body_gyroscope_std()x"                           
## [60] "frequency_body_gyroscope_std()y"                           
## [61] "frequency_body_gyroscope_std()z"                           
## [62] "frequency_body_accelerometer__magnitude_mean()"            
## [63] "frequency_body_accelerometer__magnitude_std()"             
## [64] "frequency_body_accelerometer_jerk_signal__magnitude_mean()"
## [65] "frequency_body_accelerometer_jerk_signal__magnitude_std()" 
## [66] "frequency_body_gyroscope__magnitude_mean()"                
## [67] "frequency_body_gyroscope__magnitude_std()"                 
## [68] "frequency_body_gyroscope_jerk_signal__magnitude_mean()"    
## [69] "frequency_body_gyroscope_jerk_signal__magnitude_std()"