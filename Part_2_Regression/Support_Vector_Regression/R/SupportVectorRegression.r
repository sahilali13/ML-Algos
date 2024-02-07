# Support Vector Regression

# Importing the dataset

dataset <- read.csv("Data/Position_Salaries.csv")
dataset <- dataset[2:3]
# Training
# install.packages("e1071")
library(e1071)
regressor <- svm(formula = Salary ~ .,
                 data = dataset,
                 type = "eps-regression")

# Prediction

y_pred <- predict(regressor, data.frame(Level = 6.5))

# View(y_pred)

# Visualizing

# install.packages("ggplot2")

library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = "blue") +
  ggtitle("Salary vs Levels") +
  xlab("Level") +
  ylab("Salary")
