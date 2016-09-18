##This code runs in R version 3.2.4 Revised (2016-03-16 r70336)
## to display the details of partial results source it with Echo in R-studio environment

##////////////////////////////////////////////////////////////////////
## Step "0": Check if the target data folder exist in the working directory. If not 
##           download the zip file containing the project data and uncompress it 
##///////////////////////////////////////////////////////////////////
##
if (!file.exists("./UCI HAR Dataset")){
    
    fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl,destfile = "./dataset_getting_cleaning_project.zip")
    
    ##
    ## unzip the file in the working directory
    ##
    unzip("./dataset_getting_cleaning_project.zip")
    
}
## 
## after unzip the folder named "UCI HAR Dataset" will be reated in your current directory
##
## the list of files in this folder can be obtained running the following command
##
project_files <- list.files("./UCI HAR Dataset", recursive = TRUE)
project_files
##
## The file "README.txt" contains the detailed description of the data set
##
## for the purpose of this project the data will be gotten from the following list of flies:
##
## "features_info.txt": Shows information about the variables used on the feature vector.
## "features.txt": List of all features.
## "activity_labels.txt": Links the class labels with their activity name.
## "train/X_train.txt": Training set.
## "train/y_train.txt": Training labels.
## "train/subject_train.txt": Training subjects
## "test/X_test.txt": Test set.
## "test/y_test.txt": Test labels.
## "test/subject_test.txt": Test subjects
##
##////////////////////////////////////////////////////////////////////////////////////
## Step "1": read the data files and check the table dimentions and merge the data sets
##////////////////////////////////////////////////////////////////////////////////////
##

print("step 1 start")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
dim(X_train)
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
dim(X_test)
## observe that the two data sets have the same number of colons (561), the number of 
## rows for the training set (number of observations = 7352) is about twice and half 
## the number of rows of the test set (2947). This ratio is usual when working with
## machine learning algorithms
## as the number of colons is the same, no further operation will be required to merge 
## these data sets into a new table called "X_data"
X_data <- rbind(X_train, X_test)

## read the feature names

features <-read.table("./UCI HAR Dataset/features.txt", header = FALSE) 
str(features)
## observe that this table contains 561 rows which match with the number of colons of the
## above "X_data". The second colon of "features" data.frame contains the feature names

## add the names to the "X_data" frame

X_data_names <- as.character(features$V2)
names(X_data) <- X_data_names

## reading and merging activity labels

train_labels <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
str(train_labels)
str(test_labels)
## observe that both tables have only one colon (So, they match), and the number of rows
## match with the number of training respectively test observations (7352 & 2947)

## now these label data sets can be merged and set the colon name "activity_label"
## pay attention to keep the same order: train on top and test at the bottom

 X_label <- rbind(train_labels,test_labels)
 names(X_label) <- "activity_label"
 

## reading and merging subject data sets

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
str(subject_train)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
str(subject_test)
## These data sets have the same dimensions as the previous one, so you could proceed to 
## merge them and set the colon name "subject"

X_subject <- rbind(subject_train,subject_test)
names(X_subject) <- "subject"

## now you can merge the all these colons to get a "merged_data" table

merged_data <- cbind(X_subject, X_label, X_data)
print("step 1 end")

##////////////////////////////////////////////////////////////////////////////////////
## Step "2": Extracts only the measurements on the mean and standard deviation for each 
##          measurement
##////////////////////////////////////////////////////////////////////////////////////
##
## to get the sub-set you can use the regular expression as follows and check the resilts
##
print("step 2 starts")
subset_feature_names <- X_data_names[grep("mean\\(\\)|std\\(\\)",X_data_names )]
subset_feature_names

## add "suject" and "activity_label", and select the colons in the merged_data frame

subset_feature_names <- c("subject", "activity_label",subset_feature_names)

subset_merged_data <- subset(merged_data,select = subset_feature_names)

print("step 2 end")
##////////////////////////////////////////////////////////////////////////////////////
## Step "3": Uses descriptive activity names to name the activities in the data set 
##////////////////////////////////////////////////////////////////////////////////////
##
## read the activity lablels table to get the descriptive name of each activity
##
print("step 3 start")

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)

str(activity_labels)
## observe that the activity descriptive names are stored a factor object in 
## the second colon of the "activity_labels" data frame 
##
## replace the "int" values of the colon "activity_label" in the "subset_merged_data" 
## data frame by their descriptive names from "activity_labels" table, and check
## the results
##

head(subset_merged_data$activity_label)
tail(subset_merged_data$activity_label)

for (index in 1:length(activity_labels$V2)) {
    
    logical_activity <-  subset_merged_data$activity_label ==index
    subset_merged_data$activity_label[which(logical_activity)] <- as.character(activity_labels$V2[index])
    
    
}
head(subset_merged_data$activity_label)
tail(subset_merged_data$activity_label)
activity_labels
str(subset_merged_data$activity_label)
print("step 3 end")
##////////////////////////////////////////////////////////////////////////////////////
## Step "4": Appropriately labels the data set with descriptive variable names 
##////////////////////////////////////////////////////////////////////////////////////
##
## the names of the features will be completed as follows":
##
## the ptefix "t" is replaced by "time" (check the regular expression and the result of the
## after substitution)
print("step 4 start")

subset_merged_data_names <- names(subset_merged_data)
subset_merged_data_names[grep("^t",names(subset_merged_data))]

names(subset_merged_data) <- gsub("^t","time", names(subset_merged_data))

subset_merged_data_names <- names(subset_merged_data)
subset_merged_data_names[grep("^t",names(subset_merged_data))]

## the prefix "f" is replaced by "frequency" (check the regular expression and the result of the
## after substitution)

subset_merged_data_names <- names(subset_merged_data)
subset_merged_data_names[grep("^f",names(subset_merged_data))]

names(subset_merged_data) <- gsub("^f","frequency", names(subset_merged_data))

subset_merged_data_names <- names(subset_merged_data)
subset_merged_data_names[grep("^f",names(subset_merged_data))]


## the "Acc" character chain is replaced by "Acceleration"(check the regular expression and the result of the
## after substitution)

subset_merged_data_names <- names(subset_merged_data)
subset_merged_data_names[grep("Acc",names(subset_merged_data))]

names(subset_merged_data) <- gsub("Acc","Acceleration", names(subset_merged_data))

subset_merged_data_names <- names(subset_merged_data)
subset_merged_data_names[grep("Acc",names(subset_merged_data))]

## the "Gyro" character chain is replaced by "AngularVelocity"

subset_merged_data_names <- names(subset_merged_data)
subset_merged_data_names[grep("Gyro",names(subset_merged_data))]

names(subset_merged_data) <- gsub("Gyro","AngularVelocity", names(subset_merged_data))

subset_merged_data_names <- names(subset_merged_data)
subset_merged_data_names[grep("Angular",names(subset_merged_data))]

## and finally the "Mag" character chain is replaced by "Magnitude"

subset_merged_data_names <- names(subset_merged_data)
subset_merged_data_names[grep("Mag",names(subset_merged_data))]

names(subset_merged_data) <- gsub("Mag","Magnitude", names(subset_merged_data))

subset_merged_data_names <- names(subset_merged_data)
subset_merged_data_names[grep("Mag",names(subset_merged_data))]

## check the global result of names transtormation

names(subset_merged_data)

print("step 4 end")
##////////////////////////////////////////////////////////////////////////////////////
## Step "5": From the data set in step 4, creates a second, independent tidy data set 
##           with the average of each variable for each activity and each subject 
##////////////////////////////////////////////////////////////////////////////////////
##

print("step 5 start")
tidy_data <- aggregate(. ~subject + activity_label, subset_merged_data, mean)
tidy_data <- tidy_data[order(tidy_data$subject,tidy_data$activity_label),]
##write.csv(tidy_data, file = "tidy_data.csv")
write.table(tidy_data, file = "tidy_data.txt", row.names = FALSE ) 
print("step 5 end")

