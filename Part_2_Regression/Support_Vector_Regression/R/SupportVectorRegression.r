# Data Preprocessing

# Importing the dataset

dataset <- read.csv("Part_2_Regression/Support_Vector_Regression/Position_Salaries.csv") # nolint: line_length_linter.
dataset <- dataset[2:3]
# Training

library(e1071)
regressor <- svm(formula = Salary ~ .,
                 data = dataset,
                 type = "eps-regression")

# Prediction

y_pred <- predict(regressor, data.frame(Level = 6.5))

# View(y_pred) # nolint: commented_code_linter.

# Visualizing

# install.packages("ggplot2") # nolint: commented_code_linter.

library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)), # nolint: line_length_linter.
            colour = "blue") +
  ggtitle("Salary vs Levels") +
  xlab("Level") +
  ylab("Salary")
