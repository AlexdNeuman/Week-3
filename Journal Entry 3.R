library(ROCR)
Titanic.train = read.csv(file="train.csv", stringsAsFactors = FALSE, header = TRUE)
Titanic.test = read.csv(file="test.csv", stringsAsFactors = FALSE, header = TRUE)
str(Titanic.train)
Titanic.train$InTrainSet = TRUE
Titanic.test$InTrainSet = FALSE
Titanic.test$Survived = NA
names(Titanic.train)
names(Titanic.test)
Titanic.complete = rbind(Titanic.train, Titanic.test)
Titanic.complete[Titanic.complete$Embarked == '', "Embarked"] = "S"
median.age = median(Titanic.complete$Age, na.rm = TRUE)
Titanic.complete[is.na(Titanic.complete["Age"]), "Age"] = median.age
table(is.na(Titanic.complete$Age))
median.fare = median(Titanic.complete$Fare, na.rm = TRUE)
Titanic.complete[is.na(Titanic.complete["Fare"]), "Fare"] = median.fare
Titanic.complete$Wealthy[Titanic.complete$Pclass > 2] = 1
Titanic.complete$Wealthy[Titanic.complete$Pclass < 3] = 0
as.numeric(Titanic.complete$Wealthy)
Titanic.complete$Sex = as.factor(Titanic.complete$Sex)
Titanic.complete$Embarked = as.factor(Titanic.complete$Embarked)
Titanic.complete$Familysize = Titanic.complete$SibSp + Titanic.complete$Parch
Titanic.complete$Survived = as.factor(Titanic.complete$Survived)
str(Titanic.complete)
Titanic.train.build = Titanic.complete[Titanic.complete$InTrainSet==TRUE,]
Titanic.test.build = Titanic.complete[Titanic.complete$InTrainSet==FALSE,]
model = glm(formula = Wealthy ~ Survived + Familysize + Sex + Age + Embarked, family = "binomial", data = Titanic.train.build)
summary(model)
model.revised = glm(formula = Wealthy ~ Survived + Age + Embarked, family = "binomial", data = Titanic.train.build)
summary(model.revised)
res = predict(model.revised, Titanic.train.build, type = "response")
table(Truevalue=Titanic.train.build$Wealthy, Predictedvalue=res>0.5)
Accuracy = (257+388)/(257+388+103+143)
ROCRPred = prediction(res, Titanic.train.build$Wealthy)
ROCRPref = performance(ROCRPred, "tpr", "fpr")
plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
