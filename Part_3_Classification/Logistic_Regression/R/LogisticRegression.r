# Logistic Regression Classification

# Importing the dataset

dataset <- read.csv("Data/Social_Network_Ads_Full.csv")
dataset <- dataset[, 3:5]
View(dataset)

# Splitting the dataset

# install.packages("caTools")
library(caTools)

set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

View(training_set)
View(test_set)

# Feature scaling

training_set[, 1:2] <- scale(training_set[, 1:2])
test_set[, 1:2] <- scale(test_set[, 1:2])

View(training_set)
View(test_set)

# Training

classifier <- glm(formula = Purchased ~ .,
                  family = binomial,
                  data = training_set)

summary(classifier)

# Prediction

prob_pred <- predict.glm(object = classifier,
                         type = "response",
                         newdata = test_set[-3])

View(prob_pred)

y_pred <- ifelse(prob_pred > 0.5, 1, 0)

View(y_pred)

# Evaluation

## Confusion Matrix

cm <- table(test_set[, 3], y_pred)

View(cm)

# Visualization

## Training Set

library(ElemStatLearn)
set <- training_set
x1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
x2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
prob_set <- predict(classifier, type = "response", newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = "Logistic Regression (Training set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "dodgerblue", "salmon"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "dodgerblue3", "salmon3"))

## Test Set

# library(ElemStatLearn)
set <- test_set
x1 <- seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
x2 <- seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
prob_set <- predict(classifier, type = "response", newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = "Logistic Regression (Test set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "dodgerblue", "salmon"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "dodgerblue3", "salmon3"))