#Merge the training and test dataset
trainData <- read.table("./data/train/X_train.txt")
dim(trainData) # [1] 7352  561
head(trainData,1)
trainLabel <- read.table("./data/train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./data/train/subject_train.txt")
testData <- read.table("./data/test/X_test.txt")
dim(testData) # [1] 2947  561
testLabel <- read.table("./data/test/y_test.txt")
testSubject <- read.table("./data/test/subject_test.txt")
joinedData <- rbind(trainData, testData)
dim(joinedData) #[1] 10299   561
joinedLabel <- rbind(trainLabel, testLabel)
dim(joinedLabel) #[1] 10299     1
joinedSubject <- rbind(trainSubject, testSubject)
dim(joinedSubject) #[1] 10299     1

# Step2. Extracts only the measurements on the mean and standard 
# deviation for each measurement. 
features <- read.table("./data/features.txt", col.names = c("featureId", "featureLabels"))
dim(features)  # 561*2
activities <- read.table("./data/activity_labels.txt", col.names=c("activityId", "activityLabel"))
dim(activities)
includedFeatures <- grep("-mean\\(\\)|-std\\(\\)", features$featureLabel)
length(includedFeatures)  # 66
selectedData <- joinedData[,includedFeatures] 
dim(selectedData) #[1] 10299    66
##Cleaning the colNames
names(selectedData) <- gsub("\\(\\)", "", features[includedFeatures, 2]) # remove "()"
names(selectedData) <- gsub("mean","Mean", names(selectedData)) # capitalize M in mean
names(selectedData) <- gsub("std", "Std", names(selectedData)) # capitalize S in std
names(selectedData) <- gsub("-", "", names(selectedData)) # removing "-"

#Uses descriptive activity names to name the activities in the data set
activity <- read.table("./data/activity_labels.txt")
activity[,2] <- tolower(gsub("_", "", activity[,2]))
activityLabel <- activity[joinedLabel[,1], 2]
joinedLabel[,1] <- activityLabel
names(joinedLabel) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity 
# names. 

names(joinedSubject) <- "subject"
cleanedData <- cbind(joinedSubject, joinedLabel, selectedData)
dim(cleanedData) # 10299*68
write.table(cleanedData, "merged_data.txt") # write out the 1st dataset


# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
library(data.table)
#calculatedData<- cleanedData[, lapply(.SD, mean), by=c("subject", "activity")]
subjectLen <- length(table(joinedSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
        for(j in 1:activityLen) {
                result[row, 1] <- sort(unique(joinedSubject)[, 1])[i]
                result[row, 2] <- activity[j, 2]
                bool1 <- i == cleanedData$subject
                bool2 <- activity[j, 2] == cleanedData$activity
                result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
                row <- row + 1
        }
}
head(result)
write.table(result, "data_with_means.txt", row.names=FALSE) 
