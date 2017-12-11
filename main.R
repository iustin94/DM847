# Exam

#########################################################################################
# READ FIRST
# 1) set working directory
# 2) load the data 
# load("workspace.R")
# continue from line 95
#########################################################################################

#########################################################################################
# 1.1 - load in data files each into their own data frame
#########################################################################################
setwd("data/")
temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*_ims.csv$", "", temp))), read.csv), 
  envir = .GlobalEnv
)
categories <- read.table("categories.txt", header=TRUE, sep=",")

#########################################################################################
# 1.2 - create a density plot for the IMS data, also saving it to PNG
#########################################################################################
setwd("../density_plots/")
names <- categories[,1]
for (i in names){
  input <- paste("../data/", i, "_ims.csv", sep="")
  input <- read.table(input,header=TRUE, sep=",")
  input <- input[-(1:131),-(3)]
  png(paste("Density Plot", i, ".png", sep=""), width = 1000, height=600)
  cols = rev(colorRampPalette(c("red", "blue", "yellow"))(13))
  filled.contour(as.matrix(input[,3:length(input[1,])]),
                 col=cols,plot.title = title(xlab="tr", ylab="1/k0"))
  dev.off()
}

#########################################################################################
# 1.3 - calls PEAX for each file, saves them as results_file
#########################################################################################
setwd("../peax/")
for (i in names){
  input <- paste("../data/", i, "_ims.csv", sep="")
  output <- paste("../peaks_output/peaks_", substr(i, 14, 16), ".csv", sep="")
  system(paste("peax.exe", input, output, sep=" "))
}

# inputs the peak result files back into R
setwd("../peaks_output")
temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
  read.table, header=TRUE), envir = .GlobalEnv
)

#########################################################################################
# 1.4 - Peak alignment
#########################################################################################
par(mfrow=c(1,2))
citrus <- NULL
original <- NULL

citrus <- rbind(peaks_34, peaks_51)
citrus <- rbind(citrus, peaks_38)
original <- rbind(peaks_26, peaks_41)
original <- rbind(original, peaks_44)

# VISUALIZATION
plot(x=citrus$t, y=citrus$r)
plot(x=original$t, y=original$r)

# DATA STRUCTURE FOR K-MEANS
citrus.data <- cbind(citrus$peak_name, citrus$t, citrus$r)
original.data <- cbind(original$peak_name, original$t, original$r)
colnames(citrus.data) <- c("peak_name", "t", "r")
colnames(original.data) <- c("peak_name", "t", "r")

citrus.num_cluster <- (nrow(citrus.data))*sum(apply(citrus.data,2,var))
for (i in 75:85) citrus.num_cluster[i - 74] <- sum(kmeans(citrus.data, centers=i)$withinss)

original.num_cluster <- (nrow(original.data))*sum(apply(original.data,2,var))
for (i in 75:85) original.num_cluster[i - 74] <- sum(kmeans(original.data, centers=i)$withinss)

# BEST FIT IS 81 and 84 CLUSTERS
plot_kmeans_analysis(75, 85, citrus.num_cluster)
plot_kmeans_analysis(75, 85, original.num_cluster)

nclust <- 81
citrus.kmeans <- kmeans(citrus.data, nclust)
original.kmeans <- kmeans(original.data, nclust)

plot(x=citrus.data[,2], y=citrus.data[,3], col=original.kmeans$cluster, pch=20)
plot(x=original.data[,2], y=original.data[,3], col=original.kmeans$cluster, pch=20)

# TO DO: JUSTIN DO IT!
# CANBERA DISTANCE
# canbera <- dist(d, method = "canberra", diag = TRUE)
# canbera

# BUILD MATRIX
citrus.matrix <- build_matrix(citrus, citrus.kmeans$cluster)
original.matrix <- build_matrix(original, original.kmeans$cluster)

# FINAL MATRIX
dummy.matrix <- cbind(citrus.matrix, original.matrix)

#########################################################################################
# HELPER FUNCTIONS
#########################################################################################
plot_kmeans_analysis <- function(from, to, data) {
  plot(from:to, data, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",
       main="Assessing the Optimal Number of Clusters with the Elbow Method",
       pch=20, cex=2, panel.first = grid())
}

build_matrix <- function (p_data, p_clusters) {
  
  matrixData <- NULL
  matrixData <- cbind(p_data)
  matrixData <- matrixData[,-c(5,6,7)]
  matrixData <- cbind(matrixData, cluster=p_clusters)
  matrixData <- as.data.frame(matrixData)
  
  headers <- strsplit(as.character(unique(matrixData$measurement_name)), " ")
  matrix <- matrix(0, nrow = nclust, ncol = length(headers))
  colnames(matrix) <- headers
  
  for(i in 1:nrow(matrixData)) {
    row <- matrixData[i,]
    index <- which(headers == row$measurement_name)
    old_row <- matrix[row$cluster,]
    old_row[index] = 1
    matrix[row$cluster,] = old_row
  }
  
  return(matrix)
}
