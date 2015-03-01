#====================================================================================
#Peer Assessments Course Project
# You should create one R script called run_analysis.R that does the following
# File: run_analysis.R
# Author: Blandine Meillon
# Date: 01 Mar 2015
#====================================================================================

#0.Reading the files to be used
if (!require("data.table")) {
  install.packages("data.table")
}

if (!require("reshape2")) {
  install.packages("reshape2")
}

require("data.table")
require("reshape2")

# get the activity labels
activityLabel <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

# get data column names
features <- read.table("./UCI HAR Dataset/features.txt")[,2]

# getting only the measurements on the mean and standard deviation for each measurement.
extract_features <- grepl("mean|std", features)

# Loading and processing testData & yTest data.
testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
yTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")

names(testData) = features

#2. Extract only the measurements on the mean and standard deviation for each measurement.
testData = testData[,extract_features]

# Load activity labels
yTest[,2] = activityLabel[yTest[,1]]
names(yTest) = c("Activity_ID", "Activity_Label")
names(testSubject) = "subject"

# Bind data
dtTest <- cbind(as.data.table(testSubject), yTest, testData)

# Load and process trainData & yTrain data.
trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")
yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")

trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt")

names(trainData) = features

# Extract only the measurements on the mean and standard deviation for each measurement.
trainData = trainData[,extract_features]

# getting activity data
yTrain[,2] = activityLabel[yTrain[,1]]
names(yTrain) = c("Activity_ID", "Activity_Label")
names(trainSubject) = "subject"

#1. Merges the training and the test sets to create one data set.
# Concatenate data
dtTrain <- cbind(as.data.table(trainSubject), yTrain, trainData)

#Merging all to get one data
data = rbind(dtTest, dtTrain)

#4. Appropriately labels the data set with descriptive variable names. 
idLabel   = c("subject", "Activity_ID", "Activity_Label")
dataLabel = setdiff(colnames(data), idLabel)
mdata      = melt(data, id = idLabel, measure.vars = dataLabel)

# Apply mean function to dataset using dcast function
tidyData   = dcast(mdata, subject + Activity_Label ~ variable, mean)

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
write.table(tidyData, file = "./tidyData.txt")