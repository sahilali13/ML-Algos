# Decision Tree Regression

# Importing the dataset

dataset <- read.csv("Part_2_Regression/Decision_Tree_Regression/Position_Salaries.csv")
dataset <- dataset[2:3]

# View(dataset)

# Training

# install.packages("rpart")
library(rpart)
regressor <- rpart(formula = Salary ~ .,
                   data = dataset,
                   control = rpart.control(minsplit = 1))

# Prediction

y_pred <- predict(regressor, data.frame(Level = 6.5))

# View(y_pred)

# Visualizing

# install.packages("ggplot2")

library(ggplot2)
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = "blue") +
  ggtitle("Salary vs Levels") +
  xlab("Level") +
  ylab("Salary")
