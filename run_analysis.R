## Reading Metadata
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
featureNames <- read.table("UCI HAR Dataset/features.txt")


##Read test data 
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt")
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt")
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt")

## Read training data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
featureTrain <- read.table("UCI HAR Dataset/train/X_train.txt")
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt")

## Merge the training and test data sets into one data set
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featureTrain, featuresTest)

## Naming the columns
colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "subject"

## The data in features, subject, activity are now merged and 
##stored in completeData
completeData <- cbind(features,activity,subject)

## PART 2 Extracts only the measurements on the mean and 
##standard deviation for each measurement. 

##Extract the column indices that have either mean or std in them.
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE
)

##Add activity and subject columns to the list
requiredColumns <- c(columnsWithMeanSTD,562,563)
dim(completeData)

## create extractedData with the selected columns in requiredColumns
extractedData <- completeData[,requiredColumns]
dim(extractedData)

##PART 3 Uses descriptive activity names to name the activities in the data set
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)

##PART 4 Appropriately labels the data set with descriptive variable names.
## Replacing Acc  with Accelerometer
names(extractedData) <- gsub("Acc", "Accelerometer", names(extractedData))
## Replacing Gyro with Gyroscope
names(extractedData) <- gsub("Gyro", "Gyroscope", names(extractedData))
##Replacing BodyBody with Body
names(extractedData) <- gsub("BodyBody", "Body", names(extractedData))
##Replacing Mag with Magnitude
names(extractedData) <- gsub("Mag", "Magnitude", names(extractedData))
##Replacing Character f with Frequency
names(extractedData) <- gsub("^f", "Frequency", names(extractedData))
names(extractedData) <- gsub("^t", "Time", names(extractedData))
names(extractedData) <- gsub("tBody", "TimeBody", names(extractedData))
names(extractedData) <- gsub("-mean()", "Mean", names(extractedData))
names(extractedData) <- gsub("-std()", "STD", names(extractedData))
names(extractedData) <- gsub("-freq()", "Frequency", names(extractedData))
names(extractedData) <- gsub("angle", "Angle", names(extractedData))
names(extractedData) <- gsub("gravity", "Gravity", names(extractedData))

##PART 5 From the data set in step 4, creates a second, 
##independent tidy data set with the average 
##of each variable for each activity and each subject.
extractedData$subject <- as.factor(extractedData$subject)
extractedData <- data.table(extractedData)

tidyData <- aggregate(. ~subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$subject, tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
