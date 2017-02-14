##GETTING AND CLEANING DATA COURSE ASSIGMENT##

##1. Merges the training and the test sets to create one data set.

setwd("~/Desktop/Coursera/UCI HAR Dataset/test")
xtest <- read.table("X_test.txt")
ytest <- read.table("y_test.txt")
subjtest <- read.table("subject_test.txt")
test <- cbind(xtest, ytest, subjtest)

setwd("~/Desktop/Coursera/UCI HAR Dataset/train")
xtrain <- read.table("X_train.txt")
ytrain <- read.table("y_train.txt")
subjtrain <- read.table("subject_train.txt")
train <- cbind(xtrain, ytrain, subjtrain)

dta <- rbind(test, train)


##2. Extracts only the measurements on the mean and standard deviation for each measurement."

setwd("~/Desktop/Coursera/UCI HAR Dataset")

feats <- read.table("features.txt")
meanstd <- grep(".mean()|.std()", feats$V2)
dta <- dta[ , meanstd]



##3. Uses descriptive activity names to name the activities in the data set

activity <- rbind(ytest, ytrain)
activity <- as.factor(activity[,1])

levels(activity) <- c("walking", "walking_upstairs", "walking_downstairs", "sitting",
                  "standing", "laying")


subj <- rbind(subjtest, subjtrain)
dta <- cbind(dta, activity, subj)


##4. Appropriately labels the data set with descriptive variable names.

cnames <- feats[meanstd, 2]
cnames <- as.character(cnames)

colnames(dta) <- cnames
colnames(dta)[80] <- "activity"
colnames(dta)[81] <- "subject"

##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

meanactivity <- list()

for(i in 1:79){
        meanactivity[i] <- list(tapply(dta[,i], dta$activity, mean))      

}

meanactivity

meansubj <- list()

for(i in 1:79){
        meansubj[i] <- list(tapply(dta[,i], dta$subject, mean))
}

meansubj

mna <- data.frame(meanactivity)
colnames(mna) <- cnames

mns <- data.frame(meansubj)
colnames(mns) <-cnames

tidymean <- rbind(mna, mns)

write.table(tidymean, file = "/Users/noahgarcia/Desktop/Coursera/tidymean.txt", row.name = FALSE)
