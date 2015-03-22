library(dplyr) 
subj_tr <- read.table("UCI HAR Dataset/train/subject_train.txt")
subj_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_tr <- read.table("UCI HAR Dataset/train/X_train.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_tr <- read.table("UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
features <- read.table("UCI HAR Dataset/features.txt")
activity_lbl <- read.table("UCI HAR Dataset/activity_labels.txt")

dataset <- function(subject, x, y, activity_lbl){
  n<- cbind(subject, y)
  colnames(n)<- c("subject", "labels")
  colnames(x) <- features[["V2"]]
  colnames(activity_lbl) <- c("lbl", "activity")
  n1<-cbind(n, x)
  n2 <- merge(activity_lbl, n1, by.y = "labels", by.x = "lbl", all = TRUE)
  ##colnames(n2)<- c("subject", "labels", "activity")
  columns <- grepl( "activity" , names( n2 ) ) | grepl( "labels" , names( n2 ) ) | grepl( "subject" , names( n2 ) ) | grepl( "mean()" , names( n2 ) ) | grepl( "std()" , names( n2 ) )
  n3 <- n2[, columns]
  n3
}

test <- dataset(subject = subj_test, x=X_test, y = y_test, activity_lbl)
train <- dataset(subject = subj_tr, x=X_tr, y = y_tr, activity_lbl)
combined <- rbind(test, train)

combined %>% group_by(activity, subject) %>% summarise_each(funs(mean)) %>% write.table( file = "result.txt", row.name=FALSE)




