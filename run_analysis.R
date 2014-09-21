#This script is extracting files of all the relevant data and merging it accordingly to get a clean & tidy data.

#Checking if reshape2 package is installed, if not we install it here

if (!("reshape2" %in% rownames(installed.packages())) ) {
  print("Please install required package \"reshape2\" before proceeding")
} 
else {
  #Reading the required packages from the library
  library(reshape2)
  
  #Reading the activity file and labelling the column names
  activity_labels <- read.table("./activity_labels.txt",col.names=c("activity_id","activity_name"),header=TRUE)
  features <- read.table("features.txt",header=TRUE)
  
  #Reading the column names and assigning it to the column names of testdata
  feature_names <- features[,2]
  testdata <- read.table("./test/X_test.txt",header=TRUE)
  colnames(testdata) <- feature_names
  #Reading the train data and assigning it to the column names of traindata
  traindata <- read.table("./train/X_train.txt",header=TRUE)
  colnames(traindata) <- feature_names
  
  #Reading the required subject and activity columns and assigning them column names accordingly
  test_subject_id <- read.table("./test/subject_test.txt",header=TRUE)
  colnames(test_subject_id) <- "subject_id"
  test_activity_id <- read.table("./test/y_test.txt",header=TRUE)
  colnames(test_activity_id) <- "activity_id"
  train_subject_id <- read.table("./train/subject_train.txt",header=TRUE)
  colnames(train_subject_id) <- "subject_id"
  train_activity_id <- read.table("./train/y_train.txt",header=TRUE)
  colnames(train_activity_id) <- "activity_id"
  
  #Combining all the test subject and activity ids
  test_data <- cbind(test_subject_id , test_activity_id , testdata)
  
  #Combining all the train subject and activity ids
  train_data <- cbind(train_subject_id , train_activity_id , traindata)
  #Combining all the rows of train and test data
  all_data <- rbind(train_data,test_data)
  
  #Searching for the pattern mean and matching it
  mean_col_idx <- grep("mean",names(all_data),ignore.case=TRUE)
  #Subsetting all the mean patterns to mean_col_names
  mean_col_names <- names(all_data)[mean_col_idx]
  #Searching for the pattern standard deviation 'std' and matching it
  std_col_idx <- grep("std",names(all_data),ignore.case=TRUE)
  #Subsetting all the std patterns to std_col_names
  std_col_names <- names(all_data)[std_col_idx]
  
  #Storing all the columns with mean and std patterns in meanstddata
  meanstddata <-all_data[,c("subject_id","activity_id",mean_col_names,std_col_names)]
  
  #Merging all the database with activity containing mean/std
  descrnames <- merge(activity_labels,meanstddata,by.x="activity_id",by.y="activity_id",all=TRUE)
  #Melt the dataset with appropriate descriptive values
  data_melt <- melt(descrnames,id=c("activity_id","activity_name","subject_id"))
  
  #Cast the melted database according to the mean value of each variable
  mean_data <- dcast(data_melt,activity_id + activity_name + subject_id ~ variable,mean)
  #Writing the table containing the tidy data
  write.table(mean_data,"./tidy_movement_data.txt")
  
}

