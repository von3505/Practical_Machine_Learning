# Practical Machine Learning Project:
# by Yvonne Low

#Download input files
training_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testing_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
dest_training <- "/Users/Yvonne/Documents/Yvonne Low/Data Science Course/Practical Machine Learning/data/pml-training.csv"
dest_testing <-  "/Users/Yvonne/Documents/Yvonne Low/Data Science Course/Practical Machine Learning/data/pml-testing.csv"
download.file(url=training_url, destfile=dest_training, method="curl")
download.file(url=testing_url, destfile=dest_testing, method="curl")

#Treat empty values as NA.
df_training <- read.csv(dest_training, na.strings=c("NA",""), header=TRUE)
colnames_train <- colnames(df_training)
df_testing <- read.csv(dest_testing, na.strings=c("NA",""), header=TRUE)
colnames_test <- colnames(df_testing)

# 1 useful step to verify that the column names (excluding classe and problem_id) are identical in the training and test set.
all.equal(colnames_train[1:length(colnames_train)-1], colnames_test[1:length(colnames_train)-1])

# Count the number of non-NAs in each col.
nonNAs <- function(x) {
  as.vector(apply(x, 2, function(x) length(which(!is.na(x)))))
}

# Build vector of missing data or NA columns to drop.
colcnts <- nonNAs(df_training)
drops <- c()
for (cnt in 1:length(colcnts)) {
  if (colcnts[cnt] < nrow(df_training)) {
    drops <- c(drops, colnames_train[cnt])
  }
}

# Drop NA columns
df_training <- df_training[,!(names(df_training) %in% drops)]
df_testing <- df_testing[,!(names(df_testing) %in% drops)]

# Drop identifier columns in the first 7 columns
df_training <- df_training[,8:length(colnames(df_training))]
df_testing <- df_testing[,8:length(colnames(df_testing))]

# Show remaining columns.
colnames(df_training)

# Check for covariates
nsv <- nearZeroVar(df_training, saveMetrics=TRUE)
nsv

# Split the training data again into training and cross validation
set.seed(102)
inTrain <- createDataPartition(y = df_training$classe, p = 0.6, list = FALSE)
training <- df_training[inTrain, ]
crossval <- df_training[-inTrain, ]

# Fit a model to predict the classe using everything else as a predictor
model <- randomForest(classe ~ ., data = training)

# Crossvalidate the model using the remaining 40% of data
predictCrossVal <- predict(model, crossval)
confusionMatrix(crossval$classe, predictCrossVal)

# Predict the classes of the original Testing data
predictTest <- predict(model, df_testing)
predictTest
