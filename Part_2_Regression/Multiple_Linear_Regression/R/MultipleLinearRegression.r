# Multiple Linear Regression

# Importing the dataset

dataset <- read.csv("Part_2_Regression/Multiple_Linear_Regression/50_Startups.csv")

# Taking care of the missing data

dataset$R.D.Spend <- ifelse(is.na(dataset$R.D.Spend),
                            ave(dataset$R.D.Spend, FUN = function(x) mean(x, na.rm = TRUE)),
                            dataset$R.D.Spend)

dataset$Administration <- ifelse(is.na(dataset$Administration),
                                 ave(dataset$Administration, FUN = function(x) mean(x, na.rm = TRUE)),
                                 dataset$Administration)

dataset$Marketing.Spend <- ifelse(is.na(dataset$Marketing.Spend),
                                  ave(dataset$Marketing.Spend, FUN = function(x) mean(x, na.rm = TRUE)),
                                  dataset$Marketing.Spend)

# Encoding the categorical data

dataset$State <- factor(dataset$State, levels = c("New York", "California", "Florida"), labels = c(1, 2, 3))

# Splitting the dataset

# install.packages("caTools")
library(caTools)

set.seed(123)
split <- sample.split(dataset$Profit, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Fitting MLR on the training set

regressor <- lm(formula = Profit ~ .,
                data = training_set)

# summary(regressor)

# Predicting the results

y_pred <- predict(regressor, newdata = test_set)