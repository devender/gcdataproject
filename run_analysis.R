#' R Script to download and produce a tidy data set


if("futile.logger" %in% rownames(installed.packages()) == FALSE) {
    install.packages("futile.logger")
}
if("data.table" %in% rownames(installed.packages()) == FALSE) {
    install.packages("data.table")
}
if("dplyr" %in% rownames(installed.packages()) == FALSE) {
    install.packages("dplyr")
}
if("knitr" %in% rownames(installed.packages()) == FALSE) {
    install.packages("knitr")
}
library(futile.logger)
library(data.table)
library(dplyr)

runScript <- function() {

    if(!file.exists("data")) { 
        flog.info("creating data dir");dir.create("data")
    }else{
        flog.info("data dir already exists");
    }
    
    if(!file.exists("data/dataset.zip")){ 
        flog.info("downloading dataset")
        fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl,"data/dataset.zip",method="curl") 
    }else{
        flog.info("dataset already downloaded")
    }
    
    if(!file.exists("data/UCI HAR Dataset")){
        flog.info("extracting dataset")
        unzip(zipfile = "data/dataset.zip", exdir="data")
    }else{
        flog.info("dataset already extracted")
    }
    
    #########################################################
    # Reading both the test and train activity and combine #
    #########################################################
    flog.info("reading activity file for test activity")
    test_activity <- read.table("data/UCI HAR Dataset/test/Y_test.txt")
    
    flog.info("reading activity file for train activity")
    train_activity <- read.table("data/UCI HAR Dataset/train/Y_train.txt")
    
    flog.info("combine test & train activity")
    all_activities <- rbind(test_activity,train_activity)
    names(all_activities)<-c("activity")
    
    flog.info("reading activity labels")
    activity_labels <- read.table("data/UCI HAR Dataset/activity_labels.txt")

    flog.info("apply activity labels")
    all_activities$activity <- factor(all_activities$activity, levels=activity_labels[,1], labels = activity_labels[,2])
    #View(all_activities)
    
    #########################################################
    # Reading both the test and train subject and combine   #
    #########################################################
    flog.info("reading file for test subject")
    test_subjects <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
    
    flog.info("reading files for train subject")
    train_subjects <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
    
    flog.info("combine test & train subject")
    subjects<-rbind(test_subjects,train_subjects)
    names(subjects)<-c("subject")
    #View(subjects)
    
    #########################################################
    # Reading both the test and train features and combine  #
    #########################################################
    flog.info("reading files for test features")
    test_features <- read.table("data/UCI HAR Dataset/test/X_test.txt")
    
    flog.info("reading files for train features")
    train_features <- read.table("data/UCI HAR Dataset/train/X_train.txt")
    
    flog.info("reading all features names")
    features_names<-fread("data/UCI HAR Dataset/features.txt")
    
    flog.info("combine test & train features")
    features<-rbind(test_features,train_features)
    names(features)<-features_names$V2
    #drop unnecessary columns
    #print(grep(x=colnames(features), "mean\\(\\)|std\\(\\)", value=TRUE))
    features<-subset(features, select=grep(x=colnames(features), "mean\\(\\)|std\\(\\)"))
    
    Data<-cbind(cbind(subjects,all_activities),features)
    names(Data)<-gsub("^t", "time", names(Data))
    names(Data)<-gsub("^f", "frequency", names(Data))
    names(Data)<-gsub("Acc", "Accelerometer", names(Data))
    names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
    names(Data)<-gsub("Mag", "Magnitude", names(Data))
    names(Data)<-gsub("BodyBody", "Body", names(Data))
    names(Data)<-gsub("-std\\(\\)", "StandardDeviation", names(Data))
    names(Data)<-gsub("-mean\\(\\)", "Mean", names(Data))
    
    Data2<-Data %>% group_by(subject,activity) %>% summarise_each(funs(mean))
    write.table(Data2,"data/UCI HAR Dataset/grouped_by_subject_and_activity.csv")
    View(Data2)
    Data2
    #rm(list=ls())
    #gc()
}