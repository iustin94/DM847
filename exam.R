# Exam

#########################################################################################
# READ FIRST
# 1) set working directory
# 2) load the data 
# load("exam_workspace.R")
#########################################################################################
install.packages("randomForest")
install.packages("rfUtilities")
install.packages("caret")
install.packages("tree")
install.packages("rpart")

library(randomForest)
library(caret)
library(rfUtilities)
library(rpart)
library(tree)

setwd("corrected_data/")
temp = list.files(pattern="BD18_*")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.table, header=TRUE), envir = .GlobalEnv
)

real <- NULL
for(name in gsub(".csv", "", temp)) {
  real <- rbind(real, get(name))
}
real <- as.data.frame(real, stringsAsFactors = TRUE)

real.data <- cbind(real$peak_name, real$t, real$r)
colnames(real.data) <- c("peak_name", "t", "r")

# DATA STRUCTURE FOR K-MEANS
real.num_cluster <- (nrow(real.data))*sum(apply(real.data,2,var))
for (i in 75:85) real.num_cluster[i - 74] <- sum(kmeans(real.data, centers=i)$withinss)

# BEST FIT X clusters
plot_kmeans_analysis(75, 85, real.num_cluster)

real_nclust <- 81
real.kmeans <- kmeans(real.data, real_nclust)

plot(x=real.data[,2], y=real.data[,3], col=real.kmeans$cluster, pch=20)

# BUILD MATRIX
real.labels <- NULL
real.labels <- read.table("labels.txt", header = TRUE, sep = "\t")
real.matrix <- build_matrix(real, real.kmeans$cluster, real_nclust)

# BUILD TRAINING MATRIX
set.seed(42)
training <- as.data.frame(t(real.matrix))
rf.candies <- as.factor(real.labels$candy)
training <- cbind(training, candy=(as.factor(real.labels$candy)))

# SPLIT DATA INTO TRAINING AND TEST DATA (75%)
train_split <- floor(0.75 * nrow(training))
train_ind <- sample(seq_len(nrow(training)), size = train_split)
rf.train <- training[train_ind, ]
rf.test <- training[-train_ind, ]
rf.model <- randomForest(candy ~ ., data = rf.train, ntree = 500)
rf.model
plot(rf.model)

# 5-FOLD CROSS-VALIDATION
rf.num <- 5
fold.trainControl <- trainControl(method = "cv", number = rf.num, classProbs = TRUE)
fold.model <- train(candy ~ ., data = training, trControl = fold.trainControl, method = "rf")
fold.model
plot(fold.model)

# REPORT MEAN, ACCURACY, SENSITIVITY AND SPECIFICITY
predicted <- predict(fold.model, rf.test)
sensitivity(rf.test$candy, predicted)
specificity(rf.test$candy, predicted)
accuracy(rf.test$candy, predicted)

# 5 PEAKS USING Gini index
gini.imporance <- as.data.frame(importance(rf.model))
sort_indexes <- order(gini.imporance, decreasing = T)[1:5]
gini.peaks <- NULL
gini.peaks <- cbind(peaks = lapply(sort_indexes, function(i) paste("V", i, sep = "")))
gini.peaks <- cbind(gini.peaks, values = gini.imporance$MeanDecreaseGini[sort_indexes])
gini.peaks <- as.data.frame(gini.peaks)

# PLOT THE DECISION TREE
tree.formula <- paste("candy ~ . +",
                      paste(gini.peaks$peaks, collapse = "+"),
                      sep = "")
tree.model <- tree(tree.formula, training)
summary(tree.model)
plot(tree.model)
text(tree.model, pretty = 0)

# PLOT VERSION II
tree.model2 <- rpart(tree.formula,
             data=training,
             method="class",
             control=rpart.control(minsplit=2, cp=0)
             )
summary(tree.model2)
plot(tree.model2)
text(tree.model2)

tmp <- as.data.frame(predict(tree.model2, rf.test))
tmp
#########################################################################################
# HELPER FUNCTIONS
#########################################################################################
plot_kmeans_analysis <- function(from, to, data) {
  plot(from:to, data, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",
       main="Assessing the Optimal Number of Clusters with the Elbow Method",
       pch=20, cex=2, panel.first = grid())
}

build_matrix <- function (p_data, p_clusters, p_num_clusters) {
  
  matrixData <- NULL
  matrixData <- cbind(p_data)
  matrixData <- matrixData[,-c(5,6,7)]
  matrixData <- cbind(matrixData, cluster=p_clusters)
  matrixData <- as.data.frame(matrixData)
  
  headers <- strsplit(as.character(unique(matrixData$measurement_name)), " ")
  matrix <- matrix(0, nrow = p_num_clusters, ncol = length(headers))
  colnames(matrix) <- headers
  
  for(i in 1:nrow(matrixData)) {
    row <- matrixData[i,]
    index <- which(headers == row$measurement_name)
    old_row <- matrix[row$cluster,]
    old_row[index] = 1
    matrix[row$cluster,] = as.numeric(old_row)
  }
  
  return(matrix)
}