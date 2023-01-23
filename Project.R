#Merges the training and the test sets to create one data set.
features <- read.table ("features.txt")
subject_test <- read.table ("test/subject_test.txt")
y_test <- read.table ("test/y_test.txt")
X_test <- read.table ("test/X_test.txt")
colnames (X_test) <- features [,2]
database_test <- cbind (subject_test, y_test, X_test)

subject_train <- read.table ("train/subject_train.txt")
y_train <- read.table ("train/y_train.txt")
X_train <- read.table ("train/X_train.txt")
colnames (X_train) <- features [,2]
database_train <- cbind (subject_train, y_train, X_train)
database <- rbind (database_train, database_test)

#Extracts only the measurements on the mean and standard deviation for each measurement. 
vet <- grepl ("mean", colnames (database)) | grepl ("std", colnames (database)) 
vet [c(1,2)] <- TRUE
database <- database [,vet]

#Uses descriptive activity names to name the activities in the data set
activity_labels  <- read.table ("activity_labels.txt")
subst <- function (x) {
  activity_labels[grepl (x, activity_labels[,1]),2]
}
Labl <- sapply (as.list (database[,2]), subst)
database[,2] <- Labl

#Appropriately labels the data set with descriptive variable names. 
colnames (database) [c(1,2)] <- c("id", "activity")
View (database)
names (database) <- gsub ("-","",names (database))
names (database) <- tolower (names(database))
names (database) <- gsub ( "\\()" ,"",names (database))

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
database1 <- summarize_all(group_by (database, id, activity), mean)
View (database1)
write.table(database1, "Database.txt")
