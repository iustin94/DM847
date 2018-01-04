
#########################################################################################
# HELPER FUNCTIONS LOAD FIRST
#########################################################################################
load_raw_data <- function(directory) {
  print("LOADING DATA ...")
  setwd(paste(project_path, "data", sep = "/"))
  temp = list.files(pattern="*.csv")
  list2env(
    lapply(setNames(temp, make.names(gsub("*_ims.csv$", "", temp))), read.csv), 
    envir = .GlobalEnv
  )
  assign("peaks_labels", read.table("labels.txt", header = TRUE, sep = "\t"), envir = .GlobalEnv)
}

generate_density_plot <- function(output_dir, data_dir) {
  setwd(paste(project_path, output_dir, sep = "/"))
  names <- peaks_labels$file
  for (i in names){
    input <- paste("../", data_dir, "/", i, sep = "")
    print(paste("Generating density plot for", i, sep = " "))
    input <- read.table(input,header=TRUE, sep=",")
    input <- input[-(1:131),-(3)]
    png(paste(i, ".png", sep=""), width = 1000, height=600)
    cols = rev(colorRampPalette(c("red", "blue", "yellow"))(13))
    filled.contour(as.matrix(input[,3:length(input[1,])]),
                   col=cols,plot.title = title(xlab="tr", ylab="1/k0"))
    dev.off()
  }
}

# RUN TEST
build_peaks <- function(data_dir, peax_dir, out_dir, p_pattern) {
  setwd(paste(project_path, peax_dir, sep = "/"))
  print("Running peax")
  for (i in names){
    input <- paste("../", data_dir, "/", i, "_ims.csv", sep="")
    output <- paste("../", out_dir, "/", i, p_pattern, sep="")
    print(output)
    system(paste("peax.exe", input, output, sep=" "))
  }
  
  # inputs the peak result files back into R
  setwd(paste(project_path, out_dir, sep = "/"))
  temp = list.files(pattern="*.csv")
  list2env(
    lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
           read.table, header=TRUE), envir = .GlobalEnv
  )
}

get_peaks <- function(p_pattern) {
  tmp_peaks <- NULL
  temp = list.files(pattern=p_pattern)
  for(name in gsub(".csv", "", temp)) {
    tmp_peaks <- rbind(tmp_peaks, get(name))
  }
  tmp_peaks <- as.data.frame(tmp_peaks, stringsAsFactors = TRUE)
  
  return(tmp_peaks)
}

get_peaks_data <- function(p_peaks) {
  # tmp <- cbind(p_peaks$peak_name, p_peaks$t, p_peaks$r)
  tmp <- cbind(p_peaks$t, p_peaks$r)
  colnames(tmp) <- c("t", "r")
  
  return(tmp)
}

get_nclust <- function(p_data) {
  tmp_start <- 79
  tmp_end <- 85
  clusters <- (nrow(p_data))*sum(apply(p_data,2,var))
  for (i in tmp_start:tmp_end) clusters[i - tmp_start] <- sum(kmeans(p_data, centers=i)$withinss)
  
  plot_kmeans_analysis(tmp_start + 1, tmp_end, clusters)
  return(which.max(clusters) + tmp_start)
}

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

build_training_matrix <- function(p_matrix, p_labels = NULL) {
  tmp <- as.data.frame(t(p_matrix))
  if (is.null(p_labels)) {
    tmp <- cbind(tmp, candy=(c("")))
  } else {
    tmp <- cbind(tmp, candy=(as.factor(p_labels$candy)))  
  }
  return(tmp)
}

build_gini_index <- function(p_model) {
  tmp_importance <- varImp(p_model, scale = FALSE, useModel = TRUE, nonpara = TRUE)
  sort_indexes <- order(tmp_importance$importance, decreasing = T)[1:5]
  tmp <- NULL
  tmp <- cbind(peaks = lapply(sort_indexes, function(i) paste("V", i, sep = "")))
  tmp <- cbind(tmp, values = tmp_importance$importance[sort_indexes,])
  tmp <- as.data.frame(tmp)
  
  return(tmp)
}

plot_dt <- function() {
  setwd(project_path)
  plot(tree.model2, main = paste("Decision tree for", peaks.n_clust, "cluster", collapse = " "))
  text(tree.model2)
}

save_dt <- function() {
  setwd(project_path)
  png(paste("tree_img.png", sep=""), width = 1000, height=600)
  plot_dt()
  dev.off()
}

predict_result <- function(p_tree, p_test) {
  the_result <- as.data.frame(predict(p_tree, p_test))
  tmp <- get_result(the_result)
  View(tmp)
  write.csv(tmp, "result.csv", row.names = FALSE)
  return(tmp)
}

get_result <- function(p_data) {
  setwd(paste(project_path))
  headers <- colnames(p_data)
  patients <- rownames(p_data)
  tmp <- NULL
  for (i in 1:nrow(p_data)) {
    if (p_data[i,][1] > p_data[i,][2]) {
      tmp <- rbind(tmp, c(patients[i], headers[1]))
    } else {
      tmp <- rbind(tmp, c(patients[i], headers[2]))
    }
  }
  colnames(tmp) <- c("file", "candy")
  return(as.data.frame(tmp))
}

