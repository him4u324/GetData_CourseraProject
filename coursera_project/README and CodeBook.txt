##### Project assignment:- 

Instructions for project The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
   Description of the DATA



#### Variables explained:-

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix �t� to denote time) were captured at a constant rate of 50 Hz. and the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) � both using a low pass Butterworth filter.

The body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

A Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the �f� to indicate frequency domain signals).



##### Description of abbreviations of measurements

leading t or f is based on time or frequency measurements.
Body = related to body movement.
Gravity = acceleration of gravity
Acc = acceleration
GyroJerk = Angular Acceleration
Gyro = Angular Speed
Mag = magnitude of movement



##### Only Mean and Std are considered from original set of measurements

mean and SD are calculated for each subject for each activity for each mean and SD measurements.
The units given are g�s for the accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec for the corresponding jerks.
These signals were used to estimate variables of the feature vector for each pattern:
�-XYZ� is used to denote 3-axial signals in the X, Y and Z directions. They total 33 measurements including the 3 dimensions - the X,Y, and Z axes.

Time.bodyAcceleration-XYZ
Time.GravityAcceleration-XYZ
Time.bodyAccelerationjerk-XYZ
Time.bodyAngularSpeed-XYZ
Time.bodyAngularAcceleration-XYZ
Time.bodyAccelerationMagnitude
Time.gravityAccelerationMagnitude
Time.bodyAccelerationjerkMagnitude
Time.bodyAngularSpeedMagnitude
Time.bodyAngularAccelerationMagnitude
Freq.bodyAcceleration-XYZ
Freq.bodyAccelerationjerk-XYZ
Freq.bodyAngularSpeed-XYZ
Freq.bodyAccelerationMagnitude
Freq.bodybodyAccelerationjerkMagnitude
Freq.bodybodyAngularSpeedMagnitude
Freq.bodybodyAngularAccelerationMagnitude

The set of variables that were estimated from these signals are:

mean(): Mean value
std(): Standard deviation
Data Set Information:
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.




######## 
	Data will be downloaded from the link mentioned in the question and unzipped into 'UCI HAR Dataset'.

#used files for project
	Files in folder �UCI HAR Dataset� that will be used are:

*SUBJECT FILES
	test/subject_test.txt
	train/subject_train.txt
*ACTIVITY FILES
	test/X_test.txt
	train/X_train.txt
*DATA FILES
	test/y_test.txt
	train/y_train.txt

features.txt - Names of column variables in the dataTable

activity_labels.txt - Links the class labels with their activity name.




####### All steps are mentioned in run_analysis.R

############	FINAL OUPUT	############
	The final output file is "tidy.txt" which is set of variables for each activity and each subject.
	10299 instances are split into 180 groups (30 subjects and 6 activities).
	66 mean and standard deviation features are averaged for each group.
	The resulting data table has 180 rows and 69 columns
		 :� 	33 Mean variables + 
			33 Standard deviation variables +
			1 Subject( 1 of of the 30 test subjects) + 
			ActivityName + ActivityNum . 
	The tidy data set�s first row is the header containing the names for each column.
