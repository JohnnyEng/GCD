README:  Human Activity Recognition Using Smartphones Dataset



OBJECTIVE OF THE COURSE PROJECT:

The objective of this assignment was to create a tidy data set from the original data set by:
1.  Merging the training and the test sets to create one data set.
2.	Extracting only the measurements on the mean and standard deviation for each measurement. 
3.	Using descriptive activity names to name the activities in the data set
4.	Naming the data set with descriptive activity names. 
5.	Creating a tidy data set with the average of each variable for each activity and each subject. 


FILES:

The github repo (https://github.com/JohnnyEng/GCD) contains the following files:
- tidydata.txt: The tidy data set is a data frame that can be loaded into R with: read.table("tidydata.txt")
- run_analysis.R: The code to obtain the tidy data set. To run the code in R, make sure the original files (see list below) are located in your working directory.
- CodeBook.md: information about variables in the tidy data set
- README.txt: explanation of the script


SOURCE MATERIAL:

The following files from the original data set were used in this code:
X_train.txt: Training set.
y_train.txt: Training labels.
X_test.txt: Test set.
y_test.txt: Test labels.
subject_test.txt: List of study subjects for test set.
subject_train.txt: List of study subjects for training set.
features.txt: list of variables used in the original study
activity_labels.txt provided the information for the activity names


EXPLANATION OF THE SCRIPT:

The first part of the code (step 1-4) combines the various data sets to obtain one data set that contains the complete data ("data"), a data frame with 10299 rows and 563 columns
The following files were combined by row-binding or column-binding:
Y_test.txt
Y_train.txt
X_test.txt
X_train.txt

An additional column (ID) was created using the files: subject_train.txt and subject_test.txt.
The file "features.txt" was used to create the column names. 
The factors 1-6 were replaced by the corresponding activity descriptions

The second part of the code (step 5) extracts the measurements on the mean and standard deviation of the data set. Not included were the mean frequencies and the angle gravity means.
The resulting data frame is named "datasub" (10299 rows, 68 columns).

The third part (step 6) splits the data frame according to ID and activity and subsequently calculates the average of each variable for each activity and each subject. The resulting matrix is converted back into a data frame named (almosttidydata" with 180 rows and 66 columns. 

The last part of the code (step 7) cleans up the column names by removing "()" and splits the rownames of the almosttidydata set into 2 separate columns: ID and activity. The result is a data frame with 180 rows and 68 columns.
