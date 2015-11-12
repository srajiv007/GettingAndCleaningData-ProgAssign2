features = NULL
x_csv_lines = NULL
subject_activity_matrix = NULL


read.HAR.dataset<-function(type = "test", dir = "data/UCI HAR Dataset"){
        # pretty read the features for the dataset
        features<<-read.table(sep = " ", paste0(c(dir, "features.txt"), collapse = "/"), stringsAsFactors = FALSE, col.names = c("index", "measures"))
        activity_labels<-read.table(paste0(c(dir, "activity_labels.txt"), collapse = "/"), col.names = c("activity.id", "activity.label"), colClasses = c("numeric", "character"))
        
        # y_test_table : type of activity performed
        # activity_labels : description of each matrix
        # Merge both the tables, to get the activity type for each row of test data
        y_table<-read.table(paste0(c(dir, type, sprintf(fmt = "y_%s.txt", type)), collapse = "/"), col.names = c("activity.id"), colClasses = c("numeric"))
        y_activity_table<-merge(y_table, activity_labels, by.x  = "activity.id", by.y = "activity.id", colClasses = c("numeric", "character"))
        subject_table<-read.table(paste0(c(dir, type, sprintf(fmt = "subject_%s.txt", type)), collapse = "/"), col.names = c("subject.id"), colClasses = c("numeric"))
        
        subject_activity_matrix<<-cbind(as.matrix(subject_table), as.matrix(y_activity_table))

        # read.csv is unable to split the data uniformly
        # so...read the lines and replace spaces with "," and read the data into line vector
        x_lines<-readLines(paste0(c(dir, type, sprintf(fmt= "X_%s.txt", type)), collapse = "/"), skipNul = TRUE)
        
        # replace all spaces with "," : generates a vector of replaced lines
        x_csv_lines<<-gsub(trimws(x_lines), pattern = " +", replacement = ",")
}



create.HAR.matrix<-function(type, sample.size = -1){
        # create matrix to bind all the CSV rows into
        HAR.matrix<-matrix(nrow = 0, ncol = length(features$measures))
        colnames(HAR.matrix)<-features$measures
        
        # read the sample.size (for testing)
        sample.size<-if(sample.size==-1) length(x_csv_lines) else sample.size 
        
        # sample data from initial dataset for faster read
        x_csv_lines<<-sample(x = x_csv_lines, size = sample.size)
        
        # iterate over the line vectors and bind them to the global data matrix
        for(x_line in x_csv_lines){
                # split each vector with sep = "," and bind them to the global HAR.matrix
                line_vector<-as.numeric(unlist(strsplit(x_line, split = ",", fixed = TRUE)))
                m<-matrix(data = line_vector, byrow = FALSE, ncol = length(features$measures))
                HAR.matrix<-rbind(HAR.matrix, m)
        }
        
        HAR.matrix<-cbind(subject_activity_matrix[1:sample.size, ], HAR.matrix)
        
        # Bind the dataset type to the combined dataset
        #HAR.matrix<-cbind(type, HAR.matrix)
        #colnames(HAR.matrix)[1]<-"sample.type"        
        return(HAR.matrix)
}

get.mean.std.measures<-function(){
        measures.names<-features$measures
        
        # select the columns with mean and stddev measures
        mean.measures<-grep("-mean\\(\\)", measures.names)
        std.measures<-grep("-std\\(\\)", measures.names)
        
        return(measures.names[c(mean.measures, std.measures)])
}

create.tidy.df <- function(m){
        
        mean.std.measures<-get.mean.std.measures()
        
        # filter the matrix with the data.type, activity.label and mean/std measures
        filter.names<-c("subject.id", "activity.label",  mean.std.measures)
        
        HAR.matrix.tidy<-m[, filter.names]
        HAR.tidy.df<-data.frame(HAR.matrix.tidy, stringsAsFactors = FALSE)
        names(HAR.tidy.df)<-filter.names
        
        for(col_index in 1:length(c(mean.measures, std.measures))){
                HAR.tidy.df[[col_index+2]]<-as.numeric(HAR.tidy.df[[col_index+2]])
        }
        
        HAR.tidy.df$subject.id<-as.numeric(HAR.tidy.df$subject.id)
        return(HAR.tidy.df)        
}

# read & create matrix for test dataset
read.HAR.dataset(type = "test")
HAR.matrix.test<-create.HAR.matrix("test")

# read & create matrix for train dataset
read.HAR.dataset(type = "train")
HAR.matrix.train<-create.HAR.matrix("train")

# combine both test and train datasets
HAR.matrix<-rbind(HAR.matrix.test, HAR.matrix.train)

# tidy the matrix & create new data frame
HAR.tidy.df<-create.tidy.df(HAR.matrix)

library(data.table)
HAR.tidy.DT<-data.table(HAR.tidy.df)
HAR.tidy.mean<-HAR.tidy.DT[, lapply(.SD, mean), by=.(subject.id, activity.label)]
HAR.tidy.mean<-HAR.tidy.mean[order(HAR.tidy.mean$subject.id), ]
write.table(HAR.tidy.mean, file = "data/HAR Tidy Dataset (TXT ver).txt", row.names = FALSE)
write.csv(HAR.tidy.mean, file = "data/HAR Tidy Dataset (CSV ver).csv", row.names = FALSE)


