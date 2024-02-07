# Polynomial Linear Regression

# Importing the dataset

dataset <- read.csv("Data/Position_Salaries.csv")

dataset <- dataset[2:3]

# Creating Linear Regression

lr <- lm(formula = Salary ~ .,
         data = dataset)

# summary(lr)

# Creating Polynomial Linear Regression

dataset$level2 <- dataset$Level ^ 2
dataset$level3 <- dataset$Level ^ 3
dataset$level4 <- dataset$Level ^ 4
plr <- lm(formula = Salary ~ .,
          data = dataset)

# summary(plr)

# Visualizing Linear Regression

library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x = dataset$Level, y = predict(object = lr, newdata = dataset)),
            colour = "blue") +
  ggtitle("Linear Regression") +
  xlab("Level") +
  ylab("Salary")

# Visualizing Polynomial Linear Regression
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x = dataset$Level, y = predict(object = plr, newdata = dataset)),
            colour = "blue") +
  ggtitle("polynomial Linear Regression") +
  xlab("Level") +
  ylab("Salary")

# Predicting Linear Regression
y_pred <- predict(object = lr, newdata = data.frame(Level = 6.5))

# Predicting Polynomial Linear Regression
y_pred <- predict(object = plr,
                  newdata = data.frame(Level = 6.5,
                                       level2 = 6.5 ^ 2,
                                       level3 = 6.5 ^ 3,
                                       level4 = 6.5 ^ 4))