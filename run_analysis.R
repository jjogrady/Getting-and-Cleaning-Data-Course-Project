#Extract the Data
filesPath <- "C:/Users/i55802/datasciencecoursera/data/UCI HAR Dataset"
#Subject Data
dataSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))
#Activity Data
dataActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))
#Data Files
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

###Merges the training and the test sets to create one data set.
#Rowbind SubjectTrain data and SubjectTest data to 'Subject' variable
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "Subject")
#Rowbind ActivityTrain data and Activity Test data to 'ActivityNum' variable
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "ActivityNum")
#RowBind dataTrain and dataTest files
dataTable <- rbind(dataTrain, dataTest)
#Labelling the Feature and Activity files
#Rename data according to features in file
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("FeatureNum", "FeatureName"))
colnames(dataTable) <- dataFeatures$FeatureName
#Rename data according to activities in file
ActivityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(ActivityLabels, names(ActivityLabels), c("ActivityNum","ActivityName"))
#Merge columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

###Extracts only the measurements on the mean and standard deviation for each 
###measurement.
#Pull the Mean and Std from the Feature file
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$FeatureName,value=TRUE)
#Adding Subject and Activity Num Variables to Mean and Std Measurements
dataFeaturesMeanStd <- union(c("Subject","ActivityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd)

###Uses descriptive activity names to name the activities in the data set.
#Merge Activity Names and Numbers
dataTable <- merge(ActivityLabels, dataTable , by="ActivityNum", all.x=TRUE)
dataTable$ActivityName <- as.character(dataTable$ActivityName)
##Update dataTable with variable means sorted by Subject and Activity Names
dataTable$ActivityName <- as.character(dataTable$ActivityName)
dataAggr<- aggregate(. ~ Subject - ActivityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,Subject,ActivityName))

###Appropriately labels the data set with descriptive variable names.
#Set Appropriate Descriptive Labels for Each variable
names(dataTable)<-gsub("std()", "STD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "(Time)", names(dataTable))
names(dataTable)<-gsub("^f", "(Frequency)", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

###From the data set in step 4, creates a second, independent tidy data set 
###with the average of each variable for each activity and each subject.
#Create tidy data
melt_data <- melt(dataTable, id=c("Subject","ActivityName"))
tidy_data <- dcast(melt_data, Subject+ActivityName ~ variable, mean)
#Create tidy data text file
write.table(tidy_data, "TidyData.txt", row.name=FALSE)


