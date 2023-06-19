# 14. Develop a model to predict whether a given car gets high or low 
#     gas mileage based on the Auto data set.

# Load and look the basic stats.
Auto <- ISLR2::Auto
names(Auto)


# (a) Create a binary variable, mpg01, that contains a 1 if mpg contains
#     a value above its median, and a 0 if mpg contains a value below
#     its median.
vec <- rep(0, 392)
vec[Auto$mpg > median(Auto$mpg)] = 1

#     Create a single data set containing both mpg01 and the other 
#     Auto variables
Auto <- cbind(Auto, mpg01 = vec)
Auto$mpg01 <- as.factor(Auto$mpg01)

# (b) Explore the data graphically in order to investigate the association
#     between mpg01 and the other features.

require(caret)
featurePlot(x = Auto[, c("displacement", "horsepower",
                         "weight", "acceleration")], 
            y = as.factor(Auto$mpg01),
            plot = "density", 
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(2, 2), 
            auto.key = list(columns = 2))


featurePlot(x = Auto[, c("mpg", "displacement", "horsepower",
                         "weight", "acceleration")], 
            y = Auto$mpg01,
            plot = "ellipse",
            auto.key = list(columns = 2))

featurePlot(x = Auto[, c("displacement", "horsepower",
                                "weight", "acceleration")], 
                   y = Auto$mpg01,
                   plot = "box",
                   scales = list(y = list(relation = "free"),
                                 x = list(rot = 90)),
                   layout = c(2, 2))

featurePlot(x = Auto[, c("mpg", "displacement", "horsepower",
                         "weight", "acceleration")], 
            y = Auto$mpg01,
            plot = "pairs",
            auto.key = list(columns = 2))

# We find that Displacement, Horsepower and Weight are the best 
# predictors for classification.

# (c) Split the data into a training set and a test set.
set.seed(123)
Auto_index <- sample(nrow(Auto), 294)
Auto_train <- Auto[Auto_index, ]
Auto_test <- Auto[-Auto_index, ]

# (d) Perform LDA on the training data in order to predict mpg01
#     using the variables that seemed most associated with mpg01 in
#     (b). What is the test error of the model obtained?

require(MASS)
lda_Auto_train = lda(mpg01 ~ displacement + horsepower + weight,
                     data = Auto_train)

# LDA Training Data Prediction
lda_Auto_train.pred <- predict(lda_Auto_train, Auto_train)$class
# LDA Test Data Prediction
lda_Auto_test.pred <- predict(lda_Auto_train, Auto_test)$class

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}

# LDA Training Class Error: 0.102
calc_class_err(actual = Auto_train$mpg01, predicted = lda_Auto_train.pred)
# LDA Test Class Error: 0.133
calc_class_err(actual = Auto_test$mpg01, predicted = lda_Auto_test.pred)

# (e) Perform QDA on the training data in order to predict mpg01
#     using the variables that seemed most associated with mpg01 in
#     (b). What is the test error of the model obtained?

qda_Auto_train <- qda(mpg01 ~ displacement + horsepower + weight,
                      data = Auto_train)

# QDA Training Data Prediction
qda_Auto_train.pred <- predict(qda_Auto_train, Auto_train)$class
# QDA Test Data Prediction
qda_Auto_test.pred <- predict(qda_Auto_train, Auto_test)$class

# QDA Training Class Error: 0.105
calc_class_err(actual = Auto_train$mpg01, predicted = qda_Auto_train.pred)
# QDA Test Class Error:  0.102
calc_class_err(actual = Auto_test$mpg01, predicted = qda_Auto_test.pred)

# (f) Perform logistic regression on the training data in order to predict
#     mpg01 using the variables that seemed most associated with
#     mpg01 in (b). What is the test error of the model obtained?

logit_Auto_train <- glm(mpg01 ~ displacement + horsepower + weight,
                         data = Auto_train, family = binomial)

# Logit Training Data Prediction
logit_Auto_train.prob <- predict(logit_Auto_train, type = "response")
logit_Auto_train.pred <- rep(0, 294)
logit_Auto_train.pred[logit_Auto_train.prob > .5] = 1

# Logit Test Data Prediction
logit_Auto_test.prob <- predict(logit_Auto_train, newdata = Auto_test, type = "response")
logit_Auto_test.pred <- rep(0, 98)
logit_Auto_test.pred[logit_Auto_test.prob > .5] = 1

# Logit Training Class Error: 0.095
calc_class_err(actual = Auto_train$mpg01, predicted = logit_Auto_train.pred)
# Logit Test Class Error:  0.122
calc_class_err(actual = Auto_test$mpg01, predicted = logit_Auto_test.pred)

# (g) Perform naive Bayes on the training data in order to predict
#     mpg01 using the variables that seemed most associated with mpg01
#     in (b). What is the test error of the model obtained?

require(e1071)
nb_Auto_train <- naiveBayes(mpg01 ~ displacement + horsepower + weight,
                            data = Auto_train)

# NB Training Data Prediction
nb_Auto_train.pred <- predict(nb_Auto_train, Auto_train)
# NB Test Data Prediction
nb_Auto_test.pred <- predict(nb_Auto_train, Auto_test)

# NB Training Class Error: 0.105
calc_class_err(actual = Auto_train$mpg01, predicted = nb_Auto_train.pred)
# NB Test Class Error:  0.122
calc_class_err(actual = Auto_test$mpg01, predicted = nb_Auto_test.pred)

# (h) Perform KNN on the training data, with several values of K, in
#     order to predict mpg01. Use only the variables that seemed most
#     associated with mpg01 in (b). What test errors do you obtain?
#     Which value of K seems to perform the best on this data set?

library(class)
# training data
X_Auto_train = Auto_train[, c("displacement", "weight", "horsepower")]
y_Auto_train = Auto_train$mpg01

# testing data
X_Auto_test = Auto_test[, c("displacement", "horsepower", "weight")]
y_Auto_test = Auto_test$mpg01

# KNN Test Class Error for k = 1:  0.520, scaled: 0.153
calc_class_err(actual = y_Auto_test,
               predicted = knn(train = scale(X_Auto_train), 
                               test  = scale(X_Auto_test),
                               cl    = y_Auto_train,
                               k     = 1))

# KNN Test Class Error for k = 3:  0.520, scaled: 0.153
calc_class_err(actual = y_Auto_test,
               predicted = knn(train = scale(X_Auto_train), 
                               test  = scale(X_Auto_test), 
                               cl    = y_Auto_train,
                               k     = 3))

calc_class_err(actual = y_Auto_test,
               predicted = knn(train = scale(X_Auto_train), 
                               test  = scale(X_Auto_test), 
                               cl    = y_Auto_train,
                               k     = 10))
