
# read activity data sets for subjects & join with activity label data
read.subject.activity.table<-function(type = "test", dir = "data/UCI HAR Dataset"){
        activity_labels_df<-read.table(paste0(c(dir, "activity_labels.txt"), collapse = "/"), col.names = c("activity.id", "activity.label"), colClasses = c("numeric", "character"))
        
        # read subject information and join to activity reference table
        subject_table<-read.table(paste0(c(dir, type, sprintf(fmt = "subject_%s.txt", type)), collapse = "/"), col.names = c("subject.id"), colClasses = c("numeric"))
        y_table<-read.table(paste0(c(dir, type, sprintf(fmt = "y_%s.txt", type)), collapse = "/"), col.names = c("activity.id"), colClasses = c("numeric"))
        subject_activity_table<-cbind(subject_table, y_table)
        
        subject_activity_table<-merge(subject_activity_table, activity_labels_df, by.x  = "activity.id", by.y = "activity.id")
        return(subject_activity_table[, c(2,3)])
}

# read test/train dataset
read.HAR.table<-function(type = "test", dir = "data/UCI HAR Dataset"){
        # pretty read the features for the dataset
        features<-read.table(sep = " ", paste0(c(dir, "features.txt"), collapse = "/"), stringsAsFactors = FALSE, col.names = c("index", "measures"))
        
        x_table<-read.table(paste0(c(dir, type, sprintf(fmt= "X_%s.txt", type)), collapse = "/"), stringsAsFactors = FALSE)
        names(x_table)<-features$measures        
        return(x_table)
}

# filter test/train dataset for the mean()/std() measures
filter.mean.std.cols<-function(t){
        measures<-names(t)
        
        mean.measures<-grep("-mean\\(\\)", measures.names)
        std.measures<-grep("-std\\(\\)", measures.names)
        
        filter.cols<-measures[c(mean.measures, std.measures)]
        mean.std.table<-t[, filter.cols]
        
        return(t[, filter.cols])
}

# combine the subject activity information with the actual readings
join.subject.HAR.tables<-function(HAR.table, subject.activity.table){
        library(data.table)
        full_table<-cbind(subject.activity.table, HAR.table)
        t<-data.table(full_table)
        return (t)
}

# summarize the information & calculate mean for all measures by (subject.id, activity.label)
summarize.HAR.dataset<-function(HAR.DT){
        HAR.tidy.mean<-HAR.DT[, lapply(.SD, mean), by=.(subject.id, activity.label)]
        HAR.tidy.mean<-HAR.tidy.mean[order(HAR.tidy.mean$subject.id, HAR.tidy.mean$activity.label), ]
        return(HAR.tidy.mean)
}

# write the final summarized table to txt file
write.HAR.table<-function(t, destfile = "data/HAR Tidy Dataset.txt"){
        write.table(HAR.tidy.mean, destfile, row.names = FALSE)
}

# read & create "test" dataset
s<-read.subject.activity.table("test")
t<-read.HAR.table("test")
t<-filter.mean.std.cols(t)
t.test<-join.subject.HAR.tables(t, s)

# read & create "train" dataset
s<-read.subject.activity.table("train")
t<-read.HAR.table("train")
t<-filter.mean.std.cols(t)
t.train<-join.subject.HAR.tables(t, s)

# combine test and train data
t.final<-rbind(t.test, t.train)

# sumarize and write final data to txt file
t.summary<-summarize.HAR.dataset(t.final)
write.HAR.table(t.summary)
