# DM847

### Overview

Our task was to implement a Random Forest algorithm on the samples measured at Odense University Hospital. We've chosen R language for this project.

### Workflow

1. Install all necessary libraries
2. Load all packages
3. Load raw measured data
4. Run PEAX on data to generate peaks of use our premeasured data (DEBUG has to be TRUE)
5. Load the peaks and calculate the best cluster number
6. Apply k-means on the dataset
7. Build training matrix
8. Split data in ratio 75%-25% for train and test
9. Apply randomForest
10. Run 5-fold cross validation
11. Report mean, accuracy, sensitiviry and specificity
12. Apply gini index and pick top 5 peaks
13. Extract decision tree with the best peaks
14. Load and build training matrix for unlabeled data
15. Apply the decision tree on the unlabeled data set

### How to run

1. Open the exam.R
2. Load exam_workspace.R (if you want to see the best best result)
3. Run the script (everything is automated)

### Current status & decision tree

With the current solution by average we 0 or 3 missmatches the most. 

![Decision tree](https://github.com/iustin94/DM847/blob/master/tree_img.png "Decision tree")

### Other

Quick link to [exam](http://www.imada.sdu.dk/~jbaumbac/download/teaching/ws17-18/DM847/project/bioinformatics_intro_class_project.pdf).
