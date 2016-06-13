packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

path <- getwd()

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"

if (!file.exists(path)) {
  download.file(url, file.path(path, f))
  unzip('Dataset.zip')
}

DatasetPath <- file.path(path, "UCI HAR Dataset")
list.files(DatasetPath, recursive=TRUE)

dtSubjectTrain <- fread(file.path(DatasetPath, "train", "subject_train.txt"))
dtSubjectTest  <- fread(file.path(DatasetPath, "test" , "subject_test.txt" ))

dtActivityTrain <- fread(file.path(DatasetPath, "train", "Y_train.txt"))
dtActivityTest  <- fread(file.path(DatasetPath, "test" , "Y_test.txt" ))

fileToDataTable <- function (f) {
  df <- read.table(f)
  dt <- data.table(df)
}

dtTrain <- fread(file.path(DatasetPath, "train", "X_train.txt"))
dtTest  <- fread(file.path(DatasetPath, "test" , "X_test.txt" ))


dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

setkey(dt, subject, activityNum)

dtFeatures <- fread(file.path(DatasetPath, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)

select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]

dtActivityNames <- fread(file.path(DatasetPath, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)
setkey(dt, subject, activityNum, activityName)

dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)


index.in.feature <- function (regex) {
  grepl(regex, dt$feature)
}

n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(index.in.feature("^t"), index.in.feature("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))

x <- matrix(c(index.in.feature("Acc"), index.in.feature("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))

x <- matrix(c(index.in.feature("BodyAcc"), index.in.feature("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))

x <- matrix(c(index.in.feature("mean()"), index.in.feature("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))


dt$featJerk <- factor(index.in.feature("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(index.in.feature("Mag"), labels=c(NA, "Magnitude"))

n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(index.in.feature("-X"), index.in.feature("-Y"), index.in.feature("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])

setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

f <- file.path(path, "tidy_dataset.txt")
write.table(dtTidy, f, quote=FALSE, sep="\t", row.names=FALSE)


