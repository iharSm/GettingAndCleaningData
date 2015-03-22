library(dplyr)

## load all the datasets for editing
subj_tr <- read.table("UCI HAR Dataset/train/subject_train.txt")
subj_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_tr <- read.table("UCI HAR Dataset/train/X_train.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_tr <- read.table("UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
features <- read.table("UCI HAR Dataset/features.txt")
activity_lbl <- read.table("UCI HAR Dataset/activity_labels.txt")

#main function that takes parameters: subject - dataframe with a column of
#"subjects" - people who participated in the experiment X - dataframe of
#parameters measured in the experiment activity_lbl - key value pairs of
#activity and its description. y - dataframe of keys of activities

dataset <- function(subject, x, y, activity_lbl){
  # create table of subjects with keys of types of activites
  n<- cbind(subject, y)
  #change colum names to meaningful values
  colnames(n)<- c("subject", "labels")
  colnames(x) <- features[["V2"]]
  colnames(activity_lbl) <- c("lbl", "activity")
  
  #combine tables of subjects with experimental measurements
  n1<-cbind(n, x)
  #replace activity keys with their descriptions
  n2 <- merge(activity_lbl, n1, by.y = "labels", by.x = "lbl", all = TRUE)
  
  #create a list of column names that we want to keep (ie those that contain
  #mean(), std(), activity and subject in their names)
  columns <- grepl( "activity" , names( n2 ) ) |
    grepl( "subject" , names( n2 ) ) |
    grepl( "mean()" , names( n2 ) ) |
    grepl( "std()" , names( n2 ) )
  #removing all unnecessary columns
  n3 <- n2[, columns]
  n3
}

# execute dataset function on both traning and testing datasets and then combine
# them in one
test <- dataset(subject = subj_test, x=X_test, y = y_test, activity_lbl)
train <- dataset(subject = subj_tr, x=X_tr, y = y_tr, activity_lbl)
combined <- rbind(test, train)

#calculate mean for each activity and subject and store the resulting table into
#result.txt
combined %>% group_by(activity, subject) %>% 
  summarise_each(funs(mean)) %>% 
  write.table( file = "result.txt", row.name=FALSE)




