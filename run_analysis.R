# Getting and Cleaning Data - Assignment


# 1.) PREPARE Y_data (activities) to form a vector named "activity"

# 1.a) combine the Y data (files: Y_test.txt and Y_train.txt)

Y_test <- read.table("Y_test.txt") 	
Y_train <- read.table("Y_train.txt")	
Y_data <- rbind(Y_test, Y_train)

# 1.b) assign column name "activity"
colnames(Y_data) <- c("activity")

# 1.c) replace factors 1-6 with activities (standing, walking etc.)
activity5 <- sub("6", "lying", Y_data$activity) 
activity4 <-sub("5", "standing", activity5)	
activity3 <-sub("4", "sitting", activity4)
activity2 <-sub("3", "walking downwards", activity3)
activity1 <-sub("2", "walking upwards", activity2)
activity <-sub("1", "walking", activity1)



# 2.) PREPARE X_data to form a data frame named "X_data"

# 2. a) combine the X data (files: X_test.txt and X_train.txt)
X_test <- read.table("X_test.txt")
X_train <- read.table("X_train.txt")
X_data <- rbind(X_test, X_train)

# 2. b) assign column names
features <- read.table("features.txt")
colnames(X_data) <- features$V2



# 3.)  PREPARE ID data to form a vectpr named "ID"
IDtest <- read.table("subject_test.txt")
IDtrain <- read.table("subject_train.txt")
ID <- c(IDtest$V1, IDtrain$V1)



# 4.) COMBINE X data, Y data (activity) & ID to form one single data frame named "data"
data <- cbind(ID, activity, X_data)



# 5.) EXTRACT MEAN AND STD for a data subset named "datasub"

# 5.a) create a vector that returns the indices for columns with "mean(" or "std",
# add +2 to the indices to account for 2 additional columns in "data" as compared to the columns in "features"
# this vector is then used to subset the data 
mean <- grep ("[Mm]ean\\(", features$V2)
std <- grep ("[Ss]td", features$V2)
meanstd <- sort(c(mean, std)+2)

# 5.b) create final data frame named "datasub" by subsetting only the desired columns
datasub <- data[,c(1, 2, meanstd)]
	


# 6.) CALCULATE AVERAGE FOR EACH ACTIVITY AND EACH SUBJECT

# 6.a) split into ID and activity
f1 <- datasub$ID
f2 <- datasub$activity
s <- split(datasub, list(f1,f2), drop = TRUE)

# 6.b) calculate the means , returns a matrix
vec <- colnames(datasub)[3:68]
a <- sapply (s, function(s) colMeans(s[vec]))

# 6.c) swap rows and columns and convert matrix to data frame
almosttidydata <- as.data.frame(t(a))



# 7.) MAKE THE DATA LOOK PRETTY

# 7.a) remove "()" in the column names
tidynames1 <- sub(")", "", names(almosttidydata))
tidynames2 <- sub("\\(", "",  tidynames1)
names(almosttidydata) <- tidynames2

# 7.b) split the rownames into ID and motion	
splitNames <- strsplit(rownames(almosttidydata), "\\.")
library(plyr)
df <- ldply(splitNames)
colnames(df) <- c("ID", "activity")
tidydata <- cbind(df, almosttidydata)
rownames(tidydata) <- NULL
