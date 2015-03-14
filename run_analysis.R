# 1. creating one data set
dataTrain <- read.table("./train/X_train.txt")
labelTrain <- read.table("./train/y_train.txt")
subjectTrain <- read.table("./train/subject_train.txt")
dataTest <- read.table("./test/X_test.txt")
labelTest <- read.table("./test/y_test.txt")
subjectTest <- read.table("./test/subject_test.txt")

dim(dataTrain) # 7352x561
dim(labelTrain) # 7352x1
dim(subjectTrain) #7352x1
dim(dataTest) #2947x561
dim(labelTest) #2947x1
dim(subjectTest) #2947x1

data <- rbind(dataTrain, dataTest)
label <- rbind(labelTrain, labelTest)
subject <- rbind(subjectTrain, subjectTest)

features<- read.table("features.txt", stringsAsFactors=FALSE)
colnames(data) <- c(features[,2])

# 2. extracting mean and standard deviation for each var
any(is.na(data))
indices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
ms <- data[,indices]


# 3. Uses descriptive activity names to name the activities in the data set
activities <- read.table("./activity_labels.txt")
activities <-c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
data$activity <- c(rep(0,10299))
ldata <- cbind(label, data)
colnames(ldata)[1] <- "label"
for (i in 1:10299) {
  act <- ldata[i,"label"]
  activ <- activities[act]
  ldata[i,"activity"] <- activ
}
ldata <- ldata[,c(1,563,2:562)]

# adding subjects
lsdata <- cbind(ldata, subject)
lsdata <- lsdata[,c(564,1:563)]

write.table(lsdata, "merged_data.txt")

# 5. data set with the average of each variable of each subject
ds <- data.frame()
for (i in 1:30){
  char <- lsdata[which(lsdata$subject == i),4:length(lsdata)]
  ds <- rbind(ds, colMeans(char))
}
colnames(ds) <- c(features[,2])
subjects <- c(1:30)
ssd <- cbind(subjects, ds)
write.table(ssd, "data_with_means.txt")
