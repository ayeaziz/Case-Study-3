setwd("C:/Users/ayeaz/Downloads/titanic")
library(randomForest)


###read in the test and train datasets
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)


###Before combining the two datasets together for cleaning, add a "TRUE" column for the train set and "FALSE" column for the test set

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

###Add "Survived" column to the test set
titanic.test$Survived <- NA

###Combine the two datasets
titanic.full <- rbind(titanic.train, titanic.test)

###cleaning the data

#Clean missing values of Embark
titanic.full[titanic.full$Embarked=='',"Embarked"] <- 'S'

#Clean missing values of Fare
#Use a Linear Regression Model to predict the missing values

upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter <- titanic.full$Fare < upper.whisker
titanic.full[outlier.filter,]

fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(
  formula = fare.equation,
  data = titanic.full[outlier.filter,]
)

fare.row <- titanic.full[
  is.na(titanic.full$Fare),
  c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
]
fare.prediction <- predict(fare.model, newdata = fare.row)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.prediction

#Clean missing values of Age
#Use a Linear Regressions Model to predict the missing values
upper.whisker.age <- boxplot.stats(titanic.full$Age)$stats[5]
outlier.filter.age <- titanic.full$Age < upper.whisker.age
titanic.full[outlier.filter.age,]

age.equation = "Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked"
age.model <- lm(
  formula = age.equation,
  data = titanic.full[outlier.filter.age,]
)

age.row <- titanic.full[
  is.na(titanic.full$Age),
  c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked")
]
age.prediction <- predict(age.model, newdata = age.row)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.prediction

#Categorical Casting
titanic.full$Pclass <- as.ordered(titanic.full$Pclass)
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

#Split dataset back out into train and test
titanic.train <- titanic.full[titanic.full$IsTrainSet == TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet == FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)



#Build a predictive model for Survived
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)

PassengerId <- titanic.test$PassengerId
PassengerName <- titanic.test$Name
PassengerAge  <- titanic.test$Age
PassengerGender <- titanic.test$Sex
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived
output.df$Name <- PassengerName
output.df$Age <- PassengerAge
output.df$Gender <- PassengerGender


write.csv(output.df, file = "Titanic Predictions.csv", row.names = FALSE)


#Save the model to RDS file
saveRDS(titanic.model, "model.rds")


