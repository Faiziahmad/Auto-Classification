library(ISLR2)
head(Auto)
attach(Auto)
require(caret)
# Create a new variable mpg01

Auto <- ISLR2::Auto
names(Auto)
dim(Auto)
summary(Auto)
attach(Auto)
set.seed(9457)

Auto_index = sample(nrow(Auto),294)
Auto_train = Auto[Auto_index, ]
Auto_test = Auto[-Auto_index, ]


set.seed(40)
df_index <- sample(nrow(Auto_df),100)
df_train <-  df[df_index, ]
df_test <- df[-df_index, ]

#Creating binary Variable of mpg

mpg01 <- ifelse(mpg > median(mpg), 1, 0)


auto_data = lm(Drug ~ ., data= df_train)

auto_df_train_pred <-  predict(drug_nb, df_train)

auto_test_pred <-  predict(drug_nb, df_test)


nb_train_error <- calc_class_err(actual = mpg, predicted = auto_df_train_pred)

nb_test_error <- calc_class_err(actual = mpg, predicted = auto_df_test_pred)

mpg01 <- ifelse(mpg > median(mpg), 1, 0)

# Create a data frame containing mpg01 and the other Auto variables
Auto_df <- data.frame(mpg01 = mpg01, 
                      cylinders = cylinders, 
                      displacement = displacement, 
                      horsepower =horsepower, 
                      weight = weight, 
                      acceleration = acceleration, 
                      year = year, 
                      origin = origin, 
                      name = name)
Auto_df

ggplot(mpg01 ~ ., data = Auto_df)


featurePlot(x=Auto[, c("displacement","horsepower","weight","acceleration")],
            y=as.factor(mpg01),
            plot="box",
            scales= list(y=list(relation="free"),
                         x=list(rot=90)),
            layout=c(2,2))
            
featurePlot(x=Auto[, c("displacement","horsepower","weight","acceleration")],
            y=as.factor(mpg01),
            plot="density",
            scales= list(y=list(relation="free"),
                         x=list(rot=90)),
            layout=c(2,2))

featurePlot(x=Auto[, c("displacement","horsepower","weight","acceleration")],
            y=as.factor(mpg01),
            plot="pairs",
            scales= list(y=list(relation="free"),
                         x=list(rot=90)),
            auto.key=list(columns=2))

#WE FIND DISPLACEMENT HORSEPOWER AND WEIGHT ARE THE BEST PREDICTOR FOR CLASSIFICATION


#SPLIT DATA INTO TRAINING SET TEST SET


set.seed(123)
Auto_index = sample(nrow(Auto),294)
Auto_train = Auto[Auto_index, ]
Auto_test = Auto[-Auto_index, ]

# performing lda over the data 
require(MASS)
detach(Auto)
attach(Auto)
Auto__train_lda <-  lda(mpg01 ~ displacement + horsepower + weight, data =Auto_train)

#keep changing k value for to check error