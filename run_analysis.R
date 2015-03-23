run_analysis <- function(){
	# Retrieve the files
	if(!file.exists("./proj")){dir.create("./proj")}
	url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	download.file(url,destfile="./proj/RawData.zip",method="curl")
	unzip(zipfile="./proj/RawData.zip",exdir="./data")

	# get the features and activities
	f.path<-file.path("./proj","UCI HAR Dataset")
	activity.names <- read.table(file.path(f.path,"activity_labels.txt"), header=F, col.names =c("Activity","Activity Name"))
	features.names <- read.table(file.path(f.path,"features.txt"),header=F)
	
	# read the test files
	ftest.path<-file.path("./proj/UCI HAR Dataset", "test")
	X_test<-read.table(file.path(ftest.path,"X_test.txt"),header=F)
	Y_test<-read.table(file.path(ftest.path,"y_test.txt"),header=F)
	Sub_test<-read.table(file.path(ftest.path,"subject_test.txt"),header=F)
	
	# read the train files
	ftrain.path<- file.path("./proj/UCI HAR Dataset","train")
	X_train<-read.table(file.path(ftrain.path,"X_train.txt"), header=F)
	Y_train<-read.table(file.path(ftrain.path,"y_train.txt"), header=F)
	Sub_train<- read.table(file.path(ftrain.path,"subject_train.txt"), header=F)

	#1.Merges the training and the test sets to create one data set.
	X.data<-rbind(X_train,X_test)
	names(X.data)<-features.names$V2
	Y.data<-rbind(Y_train,Y_test)
	names(Y.data)<-c("Activity")
	sub.data<-rbind(Sub_train,Sub_test)
	names(sub.data)<-c("Subject")
	total.data<- cbind(X.data,Y.data,sub.data)

	#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
	total.data1<-total.data[,grep("mean|std|Subject|Activity", names(total.data))]
	
	#3.Uses descriptive activity names to name the activities in the data set
	library(plyr)
	total.data2<- join(total.data1,activity.names, by="Activity")

	#4.Appropriately labels the data set with descriptive variable names.
	names(total.data2)<-gsub("^t", "time", names(total.data2))
	names(total.data2)<-gsub("^f", "frequency", names(total.data2))
	names(total.data2)<-gsub("Acc", "Accelerometer", names(total.data2))
	names(total.data2)<-gsub("Gyro", "Gyroscope", names(total.data2))
	names(total.data2)<-gsub("Mag", "Magnitude", names(total.data2))
	names(total.data2)<-gsub("BodyBody", "Body", names(total.data2))

	#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
	library(data.table)
	final.data<- data.table(total.data2)
	mean.final.data = ddply(final.data, c("Subject","Activity","Activity.Name"), numcolwise(mean))
	write.table(mean.final.data, file = "meanfinaldata.txt",row.name=FALSE)	
}