# Exam

#########################################################################################
# READ FIRST
# 1) set working directory
# 2) load the data 
# load("exam_workspace.R")
#########################################################################################
list.of.packages <- c("randomForest", "rfUtilities", "caret", "tree", "rpart")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
} else {
  print(paste("Packages already installed" ))
}


library(randomForest)
library(caret)
library(rfUtilities)
library(rpart)
library(tree)

source("helper_functions.R") # Import helper functions

set.seed(1)
DEBUG = TRUE
project_path <- getwd()
load_raw_data("data")
# generate_density_plot("density_plots", "data")

if (DEBUG == TRUE) {
  setwd(paste(project_path, "corrected_data", sep = "/"))
  temp = list.files(pattern="*_out.csv")
  list2env(
    lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
           read.table, header=TRUE), envir = .GlobalEnv
  )  
} else {
  build_peaks("data", "peax", "corrected_data")
}

peaks <- get_peaks("*_out.csv")
peaks.data <- get_peaks_data(peaks)
peaks.n_clust <- get_nclust(peaks.data)
peaks.kmeans <- kmeans(peaks.data, peaks.n_clust)
plot(x = peaks.data[,2], y = peaks.data[,3], col=peaks.kmeans$cluster, pch=20, 
     xlab = "t", ylab = "r", main = "K-means on peak data")

# Build: matrix, training matrix
peaks.labels <- peaks_labels
peaks.matrix <- build_matrix(peaks, peaks.kmeans$cluster, peaks.n_clust)
training <- build_training_matrix(peaks.matrix, peaks_labels)

# SPLIT DATA INTO TRAINING AND TEST DATA (75%)
train_split <- floor(0.75 * nrow(training))
train_ind <- sample(seq_len(nrow(training)), size = train_split)
rf.train <- training[train_ind, ]
rf.test <- training[-train_ind, ]
rf.model <- randomForest(candy ~ ., data = rf.train, ntree = 500)
rf.model
getTree(rf.model, k = 81)
plot(rf.model)

# 5-FOLD CROSS-VALIDATION
rf.num <- 5
fold.trainControl <- trainControl(method = "cv", number = rf.num, classProbs = TRUE, savePredictions = TRUE, allowParallel = TRUE)
fold.model <- train(candy ~ ., data = training, trControl = fold.trainControl, method = "rf", tuneLength = 10, metric = "Accuracy")
fold.model
plot(fold.model)

# REPORT MEAN, ACCURACY, SENSITIVITY AND SPECIFICITY
predicted <- predict(fold.model, rf.test)
sensitivity(rf.test$candy, predicted)
specificity(rf.test$candy, predicted)
accuracy(rf.test$candy, predicted)

# 5 PEAKS USING Gini index
gini.peaks <- build_gini_index(fold.model)

# PLOT THE DECISION TREE
#########################################################################################
# VERSION 1
# tree.formula <- paste("candy ~ . +",
#                      paste(gini.peaks$peaks, collapse = "+"),
#                      sep = "")
# tree.model <- tree(tree.formula, training)
# summary(tree.model)
# plot(tree.model)
# text(tree.model, pretty = 0)
#########################################################################################

# PLOT VERSION II - BETTER
tree.formula <- paste("candy ~ ",
                      paste(gini.peaks$peaks, collapse = "+"),
                      sep = "")

tree.model2 <- rpart(tree.formula,
             data=training,
             method="class",
             control=rpart.control(minsplit = 2, cp = 0)
             )
summary(tree.model2)
plot_dt()
save_dt()

#########################################################################################
# UNLABELED DATA
load_raw_data("unlabelled_candy_raw")
if (DEBUG == TRUE) {
  setwd(paste(project_path, "corrected_data", sep = "/"))
  temp = list.files(pattern="*_out2.csv")
  list2env(
    lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
           read.table, header=TRUE), envir = .GlobalEnv
  )  
} else {
  build_peaks("data", "peax", "corrected_data", "_out2.csv")
}

unlabelled <- get_peaks("*_out2.csv")
unlabelled.data <- get_peaks_data(unlabelled)
unlabelled.n_clust <- peaks.n_clust
unlabelled.kmeans <- kmeans(unlabelled.data, unlabelled.n_clust)
unlabelled.matrix <- build_matrix(unlabelled, unlabelled.kmeans$cluster, unlabelled.n_clust)
ulabelled.the_matrix <- build_training_matrix(unlabelled.matrix)

the_result <- predict_result(tree.model2, ulabelled.the_matrix)
