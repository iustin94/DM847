# Exam

#########################################################################################
# READ FIRST
# 1) set working directory
# 2) load the data 
# load.image("workspace.R")
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
citrus.data <- cbind(citrus$t, citrus$r)
original.data <- cbind(original$t, original$r)

citrus.num_cluster <- (nrow(citrus.data))*sum(apply(citrus.data,2,var))
for (i in 75:85) citrus.num_cluster[i - 74] <- sum(kmeans(citrus.data, centers=i)$withinss)

original.num_cluster <- (nrow(original.data))*sum(apply(original.data,2,var))
for (i in 75:85) original.num_cluster[i - 74] <- sum(kmeans(original.data, centers=i)$withinss)

# BEST FIT IS 81 and 84 CLUSTERS
plot_kmeans_analysis(75, 85, citrus.kmeans)
plot_kmeans_analysis(75, 85, original.kmeans)

nclust <- 81
citrus.kmeans <- kmeans(citrus.data, nclust)
original.kmeans <- kmeans(original.data, nclust)

plot(citrus.data,col=original.kmeans$cluster, pch=20)
plot(original.data,col=original.kmeans$cluster, pch=20)

hist(citrus.data, xlim = c(1,133))

# BUILD MATRIX
dd1 <- NULL
dd1 <- cbind(dd1, d)
dd1 <- cbind(dd1, km.out.d$cluster)
colnames(dd1) <- c("r", "t", "cluster_num")
heatmap(dd1)

# TO DO: JUSTIN DO IT!
# CANBERA DISTANCE
# canbera <- dist(d, method = "canberra", diag = TRUE)
# canbera

#########################################################################################
# HELPER FUNCTIONS
#########################################################################################
plot_kmeans_analysis <- function(from, to, data) {
  plot(from:to, data, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",
       main="Assessing the Optimal Number of Clusters with the Elbow Method",
       pch=20, cex=2, panel.first = grid())
}
