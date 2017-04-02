## nead at least reshape2 for script, dplyr for checks
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
actsnamed <- merge(acts, slimmed, by.x = "V1", by.y = "V1.1", all = TRUE)
actsnamed <- actsnamed[,2:69]
##remove first column, which is the the number code from V1 of acts
## since we have the name of the acts in the second column already
names(actsnamed) <- c("activity", "subject", features$V2[good]) 
##merge puts the by.x/by.y identifying column (activity) leftmost
actsnamed <- actsnamed[,c(2,1,3:68)]  ##move subject column to leftmost
names(actsnamed) <- sub("^t", "Time",names(actsnamed))
names(actsnamed) <- sub("^f", "Freq",names(actsnamed))
names(actsnamed) <- gsub("\\(\\)|-", "",names(actsnamed))
names(actsnamed) <- sub("mean", "Mean",names(actsnamed))
names(actsnamed) <- sub("std", "Std",names(actsnamed))
library(reshape2)
melted <- melt(actsnamed, id.vars = c("subject", "activity"))
output <- dcast(melted, subject + activity ~ variable, mean)
write.table(output, "output.txt", row.names = FALSE)
data <- read.table("output.txt", header = TRUE)

check <- filter(actsnamed, subject == 1, activity == "LAYING")
mean(check$TimeBodyAccMeanX) == output[1,3] ##true means avg in output correct
sum(!(output$activity == data$activity)) ## zero if table read back in has right activities
newoutput <- output[,c(1,3:68)]
newdata <- data[,c(1,3:68)]
sum(colSums(newoutput - newdata > .0001)) ## should be zero if all numbers match