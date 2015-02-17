
#load the tidy, sqldf and dplyr packages that will be used 
#library(tidyr)
library(sqldf)
#library(dplyr)
library(data.table)

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
