## nead at least reshape2 for script, dplyr for checks
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile = "UCI.zip", method = "curl")
unzip("UCI.zip")
setwd("./UCI HAR Dataset")
testSubs <- read.table("./test/subject_test.txt")
testActs <- read.table("./test/y_test.txt")
testMeas <- read.table("./test/X_test.txt")
test <- cbind(testSubs, testActs, testMeas)
trSubs <- read.table("./train/subject_train.txt")
trActs <- read.table("./train/y_train.txt")
trMeas <- read.table("./train/X_train.txt")
tr <- cbind(trSubs, trActs, trMeas)
big <- rbind(test,tr)
features <- read.table("features.txt")
features$V2 <- as.character(features$V2)
good <- grep("mean\\(\\)|std\\(\\)", features$V2)
good2 <- c(1,2,good + 2)
slimmed <- big[,good2]
acts <- read.table("activity_labels.txt")
tidy <- merge(acts, slimmed, by.x = "V1", by.y = "V1.1", all = TRUE)
tidy <- tidy[,2:69]
##remove first column, which is the the number code from V1 of acts
## since we have the name of the acts in the second column already
names(tidy) <- c("activity", "subject", features$V2[good]) 
##merge puts the by.x/by.y identifying column (activity) leftmost
tidy <- tidy[,c(2,1,3:68)]  ##move subject column to leftmost
names(tidy) <- sub("^t", "Time",names(tidy))
names(tidy) <- sub("^f", "Freq",names(tidy))
names(tidy) <- gsub("\\(\\)|-", "",names(tidy))
names(tidy) <- sub("mean", "Mean",names(tidy))
names(tidy) <- sub("std", "Std",names(tidy))
library(reshape2)
melted <- melt(tidy, id.vars = c("subject", "activity"))
output <- dcast(melted, subject + activity ~ variable, mean)
write.table(output, "output.txt", row.names = FALSE)

## optional partial check of averaging - true means avg in output correct
## check <- filter(tidy, subject == 1, activity == "LAYING")
## abs(mean(check$TimeBodyAccMeanX) - output[1,3]) < .0001
## would use == but in some cells of output the average is off by the
## tiniest amount
