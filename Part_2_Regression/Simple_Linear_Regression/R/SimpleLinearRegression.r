# Simple Linear Regression

# Importing the dataset

dataset <- read.csv("Data/Experience_Salary.csv")

# Splitting the dataset

# install.packages("caTools")

# Feature scaling

training_set[, 1] <- scale(training_set[, 1])
test_set[, 2] <- scale(test_set[, 2])

# Training

regressor <- lm(formula = Salary ~ YearsExperience,
                data = training_set)

# Prediction

y_pred <- predict(regressor, newdata = test_set)

# Visualizing the training set

# install.packages("ggplot2")

library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = "red") +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = "blue") +
  ggtitle("Salary vs Experience (Training Set)") +
  xlab("Years of Experience") +
  ylab("Salary")


# Visualizing the test set

ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = "red") +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = "blue") +
  ggtitle("Salary vs Experience (Test Set)") +
  xlab("Years of Experience") +
  ylab("Salary")
