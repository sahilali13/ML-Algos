# K Nearest Neighbour

# Importing the dataset
dataset <- read.csv("Data/Social_Network_Ads_Full.csv")
dataset <- dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased <- factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] <- scale(training_set[-3])
test_set[-3] <- scale(test_set[-3])

# Fitting classifier to the Training set and Predicting
library(class)

y_pred <- knn(train = training_set[, -3],
  test = test_set[, -3],
  cl = training_set[, 3],
  k = 5,
#   prob = TRUE
)

y_pred
# Making the Confusion Matrix
cm <- table(test_set[, 3], y_pred)
cm
# Visualising the Training set results
library(ElemStatLearn)
set <- training_set
x1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
x2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
y_grid <- knn(train = training_set[, -3],
  test = grid_set,
  cl = training_set[, 3],
  k = 5,
)
plot(set[, -3],
     main = "K-NN Classifier (Training set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "dodgerblue", "salmon"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "dodgerblue3", "salmon3"))

# Visualising the Test set results
library(ElemStatLearn)
set <- test_set
x1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
x2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
y_grid <- knn(train = training_set[, -3],
  test = grid_set,
  cl = training_set[, 3],
  k = 5,
)

plot(set[, -3],
     main = "K-NN Classifier (Test set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "dodgerblue", "salmon"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "dodgerblue3", "salmon3"))