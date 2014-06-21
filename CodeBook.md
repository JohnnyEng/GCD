# CODEBOOK:  Human Activity Recognition Using Smartphones Dataset
========================================================


# Source of the original data:

The original data is available  at the following site: 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws


# Tidy data set

The provided data set (tidydata.txt) is a cleaned up version of the original study data. The data frame consists of 68 columns (variables) and 180 rows.


# The variables of the data frame are as follows:

ID: An identifier of the subjects who carried out the experiment, ranges between 1 and 30

activity: each subject performed several activities while wearing the smartphone: standing, sitting, laying, walking, walking down, walking up

other variables are a combination of the following  : 

Acc: Signals from the accelerometer 3-axial raw signals. The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'

Gyr¬: Signals from the gyroscope 3-axial raw signals. Units for the angular velocity vector measured by the gyroscope are radians/second. 

XYZ: is used to denote 3-axial signals in the X, Y and Z directions.

t: The prefix t denotes the time domain signal which was captured at a constant rate of 50 Hz, subsequently filtered with a corner frequency of 20Hz to remove noise.
Body/gravity: The acceleration signal is separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using a filter with a corner frequency of 0.3 Hz. 

Jerk/Mag: body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ).  Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

f: Fast Fourier Transformation of signals ('f' indicates frequency domain signals). 

mean: Mean value

std: Standard deviation


# complete list of variable names: 

"ID" 
"activity"  
"tBodyAcc-mean-X"  
"tBodyAcc-mean-Y"
"tBodyAcc-mean-Z"  
"tBodyAcc-std-X" 
"tBodyAcc-std-Y"   
"tBodyAcc-std-Z" 
"tGravityAcc-mean-X"   
"tGravityAcc-mean-Y"  
"tGravityAcc-mean-Z"    
"tGravityAcc-std-X"   
"tGravityAcc-std-Y"    
"tGravityAcc-std-Z"   
"tBodyAccJerk-mean-X"   
"tBodyAccJerk-mean-Y" 
"tBodyAccJerk-mean-Z"  
 "tBodyAccJerk-std-X"  
"tBodyAccJerk-std-Y"    
"tBodyAccJerk-std-Z"  
"tBodyGyro-mean-X"
"tBodyGyro-mean-Y"    
"tBodyGyro-mean-Z"    
"tBodyGyro-std-X"
"tBodyGyro-std-Y" 
"tBodyGyro-std-Z"
"tBodyGyroJerk-mean-X"
"tBodyGyroJerk-mean-Y"
"tBodyGyroJerk-mean-Z"  
"tBodyGyroJerk-std-X" 
"tBodyGyroJerk-std-Y"   
"tBodyGyroJerk-std-Z" 
"tBodyAccMag-mean" 
"tBodyAccMag-std"
"tGravityAccMag-mean"  
"tGravityAccMag-std"  
"tBodyAccJerkMag-mean"  
"tBodyAccJerkMag-std" 
"tBodyGyroMag-mean"
"tBodyGyroMag-std"    
"tBodyGyroJerkMag-mean"
"tBodyGyroJerkMag-std"
"fBodyAcc-mean-X"  
"fBodyAcc-mean-Y"
"fBodyAcc-mean-Z" 
"fBodyAcc-std-X" 
"fBodyAcc-std-Y"   
"fBodyAcc-std-Z" 
"fBodyAccJerk-mean-X"   
"fBodyAccJerk-mean-Y" 
"fBodyAccJerk-mean-Z"   
"fBodyAccJerk-std-X"  
"fBodyAccJerk-std-Y"    
"fBodyAccJerk-std-Z"  
"fBodyGyro-mean-X" 
"fBodyGyro-mean-Y"    
"fBodyGyro-mean-Z" 
"fBodyGyro-std-X"
"fBodyGyro-std-Y"  
"fBodyGyro-std-Z"
"fBodyAccMag-mean"
 "fBodyAccMag-std"
"fBodyBodyAccJerkMag-mean"   
"fBodyBodyAccJerkMag-std"  
"fBodyBodyGyroMag-mean"
"fBodyBodyGyroMag-std"
"fBodyBodyGyroJerkMag-mean" 
"fBodyBodyGyroJerkMag-std" 
