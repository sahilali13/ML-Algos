# Data Preprocessing

# Importing the dataset

dataset <- read.csv("Part_1_Data_Preprocessing/Data.csv")

# Taking care of the missing data

dataset$Age <- ifelse(is.na(dataset$Age),
                      ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                      dataset$Age)

dataset$Salary <- ifelse(is.na(dataset$Salary),
                         ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)), # nolint: line_length_linter.
                         dataset$Salary)

# Encoding the categorical data

dataset$Country <- factor(dataset$Country, levels = c("France", "Spain", "Germany"), labels = c(1, 2, 3)) # nolint: line_length_linter.

dataset$Purchased <- factor(dataset$Purchased, levels = c("Yes", "No"), labels = c(1, 0)) # nolint: line_length_linter.

# Splitting the dataset

# install.packages("caTools") # nolint: commented_code_linter.
library(caTools)

set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Feature scaling

training_set[, 2:3] <- scale(training_set[, 2:3])
test_set[, 2:3] <- scale(test_set[, 2:3])

# Training

regressor <- lm(formula = Salary ~ YearsExperience,
                data = training_set)

# Prediction

y_pred <- predict(regressor, newdata = test_set)

# Visualizing the training set

# install.packages("ggplot2") # nolint: commented_code_linter.

library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = "red") +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)), # nolint: line_length_linter.
            colour = "blue") +
  ggtitle("Salary vs Experience (Training Set)") +
  xlab("Years of Experience") +
  ylab("Salary")


# Visualizing the test set

ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = "red") +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)), # nolint: line_length_linter.
            colour = "blue") +
  ggtitle("Salary vs Experience (Test Set)") +
  xlab("Years of Experience") +
  ylab("Salary")
