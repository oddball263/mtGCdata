library(plyr)
library(dplyr)
library(data.table)

# - Getting and Cleaning Data Course Project
# - 4/3/2016

#1. -- Download raw data and related files
file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(file_url, "UCI_HA_Dataset.zip", method = "curl")

# 1. -- Import results data - x_test/train.txt ---------
x_train <- fread("./UCI_HAR_Dataset/train/x_train.txt")
x_test <- fread("./UCI_HAR_Dataset/test/x_test.txt")
features <- fread("./UCI_HAR_Dataset/features.txt")
# Name variables w origina names
names(x_train) <- features[ , V2]
names(x_test) <- features[ , V2]
# head(x_test) # check

#  -- Import subject data - subject_test/train.txt ---------
subject_train <- fread("./UCI_HAR_Dataset/train/subject_train.txt")
subject_test <- fread("./UCI_HAR_Dataset/test/subject_test.txt")
# Name variable 
names(subject_train) <- 'Subject'
names(subject_test) <- 'Subject'
# Add varialble 'Subject' to main table x_test and x_train
x_test$Subject <- subject_test[, Subject]
x_train$Subject <- subject_train[, Subject]

# -- Import Activity data - y_test/train.txt ---------
y_train <- fread("./UCI_HAR_Dataset/train/y_train.txt")
y_test <- fread("./UCI_HAR_Dataset/test/y_test.txt")
# Name variable 
names(y_train) <- 'Activity'
names(y_test) <- 'Activity'
# Add varialble 'Activity' to main table x_test and x_train
x_test$Activity <- y_test[, Activity]
x_train$Activity <- y_train[, Activity]


# 2. -- Select out only Mean and SD vars ---------
filter_by_mean_sd <- fread("./UCI_HAR_Dataset/mean_sd_features.txt")
mean_sd_vars <- filter_by_mean_sd[,V1]
test_data_mean_sd <- subset(x_test,  select=mean_sd_vars)
train_data_mean_sd <- subset(x_train,  select=mean_sd_vars)


# 3. -- Merge test and train datasets ---------
mergedData = rbind(test_data_mean_sd, train_data_mean_sd)


# 4. -- Create column fo Activity Descriptions and set ---------
mergedData$ActivityDescription <- "" #add a col
mergedData[mergedData$Activity ==  5, 69]  <- "STANDING"
mergedData[mergedData$Activity ==  1, 69]  <- "WALKING"
mergedData[mergedData$Activity ==  2, 69]  <- "WALKING_UPSTAIRS"
mergedData[mergedData$Activity ==  3, 69]  <- "WALKING_DOWNSTAIRS"
mergedData[mergedData$Activity ==  4, 69]  <- "SITTING"
mergedData[mergedData$Activity ==  6, 69]  <- "LAYING"


# 5. -- Revise variable names ---------
cols <- colnames(mergedData)
cols_rev<- gsub("^t", "timed", cols)
cols_rev<- gsub("^f", "freq", cols_rev)
cols_rev<- gsub("-mean()", "Mean", cols_rev)
cols_rev<- gsub("-std()", "StndDev", cols_rev)
cols_rev<- gsub("\\()", "", cols_rev)
cols_rev<- gsub("\\)", "", cols_rev)
cols_rev<- gsub("-", "", cols_rev)
cols_rev<- gsub("Acceleration", "Acc", cols_rev)
colnames(mergedData) <- cols_rev


# 6. --Create tidy dataset grouped by Subject and Activity ---------
groupedData <- mergedData %>%  group_by(Subject, Activity,ActivityDescription) %>% summarise_each(funs(mean))

# 7. -- Write file: "tidy_groupedData.txt" ---------------------------
write.table(groupedData,"tidy_groupedData.txt", sep="\t", row.names=F)

