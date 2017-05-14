library(dplyr)

# Link to download and file
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

# Checks if the download has already been performed
if (!file.exists(zipFile)) {
     download.file(zipUrl, zipFile, mode = "wb")
     print("download OK")
}

# Checks if the directory already exists, then extract the data
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
     unzip(zipFile)
     print("unzip OK")
}

# Loading Data
trainValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainlabels <- read.table(file.path(dataPath, "train", "Y_train.txt"))
trainSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))

testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testlabels <- read.table(file.path(dataPath, "test", "Y_test.txt"))
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))

# Loading measurements and labels
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
labels <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(labels) <- c("id", "labels")

# 1. Merges the training and the test sets to create one data set ###
# Adding Variables
sensores <- rbind(
     cbind(trainSubjects, trainValues, trainlabels),
     cbind(testSubjects, testValues, testlabels)
)

# Removing old variables
rm(trainSubjects, trainValues, trainlabels, testSubjects, testValues, testlabels)

# Nomeando variaveis
colnames(sensores) <- c("subject", features[, 2], "activity")

# 2. Extracts only the measurements on the mean and standard deviation for each measurement ###
# de subject ate activity, extraia somente as variaveis com mean e std
filtercolumns <- grepl("subject|activity|mean|std", colnames(sensores))

# Extracts only the filtered variables
sensores <- sensores[, filtercolumns]

# Get only the column names
columns_sensores <- colnames(sensores)

# 3. Uses descriptive activity names to name the activities in the data set ###
sensores$activity <- factor(sensores$activity, 
                  levels = labels[, 1], labels = labels[, 2])

# 4. Appropriately labels the data set with descriptive variable names ###
# Remove special characters
columns_sensores <- gsub("[\\(\\)-]", "", columns_sensores)

# expand abbreviations and clean up names
columns_sensores <- gsub("^f", "frequencyDomain", columns_sensores)
columns_sensores <- gsub("^t", "timeDomain", columns_sensores)
columns_sensores <- gsub("Acc", "Accelerometer", columns_sensores)
columns_sensores <- gsub("Gyro", "Gyroscope", columns_sensores)
columns_sensores <- gsub("Mag", "Magnitude", columns_sensores)
columns_sensores <- gsub("Freq", "Frequency", columns_sensores)
columns_sensores <- gsub("mean", "Mean", columns_sensores)
columns_sensores <- gsub("std", "StandardDeviation", columns_sensores)
columns_sensores <- gsub("BodyBody", "Body", columns_sensores)
colnames(sensores) <- columns_sensores

# 5. From the data set in step 4, 
# creates a second, independent tidy data set with the average of each variable for each 
# activity and each subject ###
sensoresMeans <- sensores %>% 
     group_by(subject, activity) %>%
     summarise_each(funs(mean))

# tidy data
write.table(sensoresMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

print("process OK")