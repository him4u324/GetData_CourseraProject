# downloading the data and unzipping the files

setwd("C:\\Users\\Himanshu\\Documents\\DSP and tools\\Coursera classes\\Geeting and Cleaning data\\GetData_CourseraProject\\coursera_project")
install.packages("downloader")
library(downloader)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download(url, dest="dataset.zip", mode="wb")
unzip ("dataset.zip")




setwd("C:\\Users\\Himanshu\\Documents\\DSP and tools\\Coursera classes\\Geeting and Cleaning data\\GetData_CourseraProject\\coursera_project\\UCI HAR Dataset")

      #reading all files
activity_labels <- read.table("activity_labels.txt", stringsAsFactors = FALSE)
features <- read.table("features.txt", strip.white=TRUE, stringsAsFactors=FALSE)

      #reading files in "train" sub folder
subject_train <- read.table("./train/subject_train.txt", col.names = "subject")
X_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt", col.names = "activity")


      #reading files in "test" sub folder
subject_test <- read.table("./test/subject_test.txt", col.names = "subject")
X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt", col.names = "activity")


      #getting the headers from data frame - "features"'s 2nd column
colnames(X_train) <- features[,2]
colnames(X_test) <- features[,2]


  ########## 3.  Uses descriptive activity names to name the activities in the data set

      # replacing the values of y_train/test with activity_labels
a<- as.character(activity_labels[,2])
b<- as.numeric (activity_labels[,1])

      #y_train
for (i in 1:6){
  y_train[,1][which(y_train[,1]==b[i])] <- a[i]
}
      #y_test
for (i in 1:6){
  y_test[,1][which(y_test[,1]==b[i])] <- a[i]
}


  ##########  1.  Merges the training and the test sets to create one data set. 
dataset <- rbind(cbind(subject_train, y_train,  X_train),
                       cbind(subject_test, y_test, X_test ))


  ##########  2.  Extracts only the measurements on the mean and standard deviation for each measurement. 

    # only retain features of mean and standard deviation
features_mean_std <- features[grep("mean\\(\\)|std\\(\\)", features$V2), ]
    # select only the means and standard deviations from data
dataset_mean_std <- dataset[, c("subject", "activity", features_mean_std$V2)]



  ##########  4.  Appropriately labels the data set with descriptive variable names

good.colnames <- c("subject", "activity", features_mean_std$V2)
good.colnames <- tolower(gsub("[^[:alpha:]]", "", good.colnames))
tidy_colnames <- good.colnames

pattern <- c ('mean','std', 'acc','gyrojerk','gyro','mag','^t','^f')
replacement <- c('_mean.','_std.', "Acceleration","AngularAcceleration","AngularSpeed", "Magnitude","Time.","Freq.")

for (i in 1:length(pattern)){
  tidy_colnames <- gsub(pattern[i],replacement[i], tidy_colnames)
}
colnames(dataset_mean_std) <- tidy_colnames

  ##########  6.  From the data set in step 4, creates a second,
  #               independent tidy data set with the average of 
  #               each variable for each activity and each subject


library(plyr)

tidy_dataset <- ddply(dataset_mean_std, c("subject","activity"), numcolwise(mean))

