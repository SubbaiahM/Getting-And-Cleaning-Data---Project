
#Prep

# The data for this projects needs to be downloaded and saved 
# in your working directory before performing/running the following R script
# The link to the dataset is given in the course project

# setwd("C:/Users/wa82/Documents/Malleswari/Coursera/Getting and Cleaning Data")

# install data.table and dplyr packages for this project.

# Read the "Training" and "test" data from the dataset 
# to the following variables
library(data.table)

featuresTrainingData <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
activityTrainingData <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
subjectTrainingData <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
featuresTestData <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
activityTestData <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
subjectTestData <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)

# 1. Merges the training and the test sets to create one data set.

   # The data between Traning and Test will be concatenated by rows 
   # and saved in the following variables

featureData <- rbind(featuresTrainingData, featuresTestData)
activityData <- rbind(activityTrainingData, activityTestData)
subjectData <- rbind(subjectTrainingData, subjectTestData)

   # Set the column Names
   
   # Read the feature names from the text file in the dataset
featureNM <- read.table("UCI HAR Dataset/features.txt")

colnames(featureData) <- t(featureNM[2])
colnames(activityData) <- c("Activity")
colnames(subjectData) <- c("Subject")

   # Now MERGE all the Data
MergeData <- cbind(featureData,activityData, subjectData)


# 2. Extracts only the measurements on the mean and 
#    standard deviation for each measurement.

   # To get only the columns with MEAN and STD
OnlyMeanSTD <- grep(".*Mean.*|.*Std.*", names(MergeData), ignore.case=TRUE)

   # Select the "Activity" and "Subject" columns
SelectedColumns <- c(OnlyMeanSTD, 562, 563)
DataOut <- MergeData[,SelectedColumns]

str(DataOut)


# 3. Uses descriptive activity names to name the activities in the data set

   # the below command shows that the Activity column is Num which needs to be
   # converted to char to display the Activiry Names
DataOut$Activity

   # Read the feature names from the text file in the dataset
activityLabelsNM <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

DataOut$Activity <- as.character(DataOut$Activity)

for (i in 1:6){
  DataOut$Activity[DataOut$Activity == i] <- as.character(activityLabelsNM[i,2])
}

   # this shows some discriptive names
head(DataOut$Activity, 30)

DataOut$Activity <- as.factor(DataOut$Activity)

# 4. Appropriately labels the data set with descriptive variable names.

   # looking at the names in the DataOut we can update some of the following codes
names(DataOut)

   # t with "Time"
   # Freq with "Frequency"
   # f with "Frequency"
   # Acc with "Accelerometer"
   # gyro with "Gyroscope"
   # mag with "Magnitude"
   # BodyBody "Body"
   # angle with "Angel"
   # gravity with "Gravity"
   # tbody with "TimeBody"
names(DataOut)<-gsub("^t", "Time", names(DataOut))
names(DataOut)<-gsub("Freq", "Frequency", names(DataOut))
names(DataOut)<-gsub("^f", "Frequency", names(DataOut))
names(DataOut)<-gsub("Acc", "Accelerometer", names(DataOut))
names(DataOut)<-gsub("Gyro", "Gyroscope", names(DataOut))
names(DataOut)<-gsub("Mag", "Magnitude", names(DataOut))
names(DataOut)<-gsub("BodyBody", "Body", names(DataOut))
names(DataOut)<-gsub("angle", "Angle", names(DataOut))
names(DataOut)<-gsub("gravity", "Gravity", names(DataOut))
names(DataOut)<-gsub("tBody", "TimeBody", names(DataOut))

# 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.

library(dplyr)  # for aggregate function

DataOut$Subject <- as.factor(DataOut$Subject)
DataOut <- data.table(DataOut)
FinaltidyData <- aggregate(. ~Subject + Activity, DataOut, mean)
FinaltidyData <- FinaltidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(FinaltidyData, file = "TidyData.txt", row.names = FALSE)

# End

