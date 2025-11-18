#Getting and Cleaning Data Course Project
#########Preparing the datasets

#Loading libraries needed
library(dplyr)
library (data.table)

#Download the zip file from the URL and save it in a temporary location
projecturl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- tempfile (fileext=".zip")
download.file(projecturl, destfile=destfile, mode="wb")

#Unzip the file and extract the file to a temporary directory
unzip(destfile, exdir = tempdir())
data_dir <- file.path(tempdir(), "UCI HAR Dataset")

#Read data using the read.table command and adding column names
Features <- read.table(file.path(data_dir, "features.txt"), col.names = c("feature_id", "feature_name"))
Activity_labels <- read.table(file.path(data_dir, "activity_labels.txt"), col.names = c("activity_id", "activity_type"))

Subject_train <- read.table(file.path(data_dir, "train", "subject_train.txt"), col.names = "subject")
X_train <- read.table(file.path(data_dir, "train", "X_train.txt"), col.names = Features$feature_name, check.names = FALSE)
Y_train <- read.table(file.path(data_dir, "train", "y_train.txt"), col.names = "activity_id")

Subject_test <- read.table(file.path(data_dir, "test", "subject_test.txt"), col.names = "subject")
X_test <- read.table(file.path(data_dir, "test", "X_test.txt"), col.names = Features$feature_name, check.names = FALSE)
Y_test <- read.table(file.path(data_dir, "test", "y_test.txt"), col.names = "activity_id")

###############ASSIGNMENT STEPS###################################

# Step 1. Merge the training and test sets to create one data set
Train_data <- cbind(Subject_train, Y_train, X_train)
Test_data <- cbind(Subject_test, Y_test, X_test)
Final_data <- rbind(Train_data, Test_data)

# Step 2. Extract only the the measurements on the mean and standard deviation for each measurement
Final_mean_std <- Final_data %>%
  select(subject, activity_id, contains("mean()"), contains("std()"))

# STEP 3. Uses descriptive activity names to name the activities in the dataset
Final_mean_std$activity_id <- Activity_labels[Final_mean_std$activity_id, 2]


# STEP 4. Appropriately labels the data set with descriptive variable names
names(Final_mean_std)[2] <- "activity"
names(Final_mean_std) <- gsub("Acc", "Accelerometer", names(Final_mean_std))
names(Final_mean_std) <- gsub("Gyro", "Gyroscope", names(Final_mean_std))
names(Final_mean_std) <- gsub("BodyBody", "Body", names(Final_mean_std))
names(Final_mean_std) <- gsub("Mag", "Magnitude", names(Final_mean_std))
names(Final_mean_std) <- gsub("^t", "Time", names(Final_mean_std))
names(Final_mean_std) <- gsub("^f", "Frequency", names(Final_mean_std))
names(Final_mean_std) <- gsub("tBody", "TimeBody", names(Final_mean_std))
names(Final_mean_std) <- gsub("-mean\\(\\)", "Mean", names(Final_mean_std), ignore.case = TRUE)
names(Final_mean_std) <- gsub("-std\\(\\)", "STD", names(Final_mean_std), ignore.case = TRUE)
names(Final_mean_std) <- gsub("-freq\\(\\)", "Frequency", names(Final_mean_std), ignore.case = TRUE)
names(Final_mean_std) <- gsub("angle", "Angle", names(Final_mean_std))
names(Final_mean_std) <- gsub("gravity", "Gravity", names(Final_mean_std))

#STEP 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
TidyData <- Final_mean_std %>%
  group_by(subject, activity) %>%
  summarise(across(.cols = where(is.numeric), mean), .groups = "drop")

# Write to a text file
write.table(TidyData, "tidy_data.txt", row.names = FALSE)

# Check the tidy dataset 
print(head(TidyData))
cat("Number of columns in tidy dataset:", ncol(TidyData), "\n")