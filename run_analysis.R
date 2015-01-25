##### 1. Merges the training and the test sets to create one data set.#####
## Download the zipped file from the link and place in working directory##

if(!file.exists("./data")){dir.create("./data")}
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

##unzip the downloaded file##

unzip(zipfile="./data/Dataset.zip",exdir="./data")

files= list.files("./data/UCI HAR Dataset",recursive=TRUE)

##List the files in directory ##
files

##Read the test and train datasets##
testdata<-read.table("./data/UCI HAR Dataset/test/X_test.txt")
traindata<-read.table("./data/UCI HAR Dataset/train/X_train.txt")

## Concatenate the test and train datasets into a variable called mergeddataset##

mergeddataset<-rbind(testdata, traindata)

##Read the features file##
features<-read.table("./data/UCI HAR Dataset/features.txt")

##Name the columns of mergeddataset##
names(mergeddataset)<-features$V2

##Read the test and train subjects##
testsubject<-read.table("./data/UCI HAR Dataset/test/subject_test.txt")
head(testsubject)

trainsubject<-read.table("./data/UCI HAR Dataset/train/subject_train.txt")
head(trainsubject)

## Concatenate the test and train subject datasets into a variable called mergedsubject##

mergedsubject<-rbind(testsubject,trainsubject)

##Name the column of mergedsubject column subjects##

names(mergedsubject)<-c("subjects")
head(mergedsubject)

##Read the test and train activities##

testactivitylabels<-read.table("./data/UCI HAR Dataset/test/y_test.txt")
head(testactivitylabels)

trainactivitylabels<-read.table("./data/UCI HAR Dataset/train/y_train.txt")
head(trainactivitylabels)

##Concatenate the test and train activitities into a variable called mergedactivitylabels##

mergedactivitylabels<-rbind(testactivitylabels,trainactivitylabels)

##Name the column of mergedactivitylabels column activities##

names(mergedactivitylabels)<-c("activities")
head(mergedactivitylabels)

##Merge all the columns to get the data1##

data1<-cbind(mergeddataset,mergedsubject, mergedactivitylabels)

####2) Extracts only the measurements on the mean and standard deviation for each measurement.########

##Get the columns of the features with names mean and std##
m<-grep("mean\\(|std\\(", features[,2], value=F)

### Get the column names that have features with mean and std##
selectedColumns<- features[,2][m]

##Combined the columns with names with mean and std and add the sugjects and activities columns##

selectedNames<-c(as.character(selectedColumns), "subjects", "activities")

##Subset the data1 with the columns selected in selectNames##

data2<-subset(data1,select=selectedNames)

#####3)Uses descriptive activity names to name the activities in the data set#####

##Read the activity labels file##

activities <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

##Modify the labels to lower case##
activities[, 2] <- tolower(as.character(activities[, 2]))

##Replace the underscore##
activities[, 2] <- gsub("_", "", activities[, 2])

##Replace all activities with proper labels##
data2[,68]<-activities[data2[,68],2]


#####4)Appropriately labels the data set with descriptive activity names#####

names(data2)<-gsub("\\(|\\)","",names(data2))
names(data2)<-gsub("_","",names(data2))
names(data2)<-tolower(names(data2))
names(data2)<-gsub("Acc", "Accelerometer", names(data2))
names(data2)<-gsub("Gyro", "Gyroscope", names(data2))
names(data2)<-gsub("Mag", "Magnitude",names(data2))
names(data2)<-gsub("^t", "time", names(data2))
names(data2)<-gsub("^f", "frequency", names(data2))


#####5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. #####

##Calculate the average of each variable for each activity and each subject based on the data set##
data<-aggregate(. ~subjects + activities, data2, mean)

##Order according to subjects and then activities##
data<-data[order(data$subjects,data$activities),]

##Output the tidy dataset as file tidydata.txt##
write.table(data, file = "./data/tidydata.txt", sep="\t", row.names=FALSE)

