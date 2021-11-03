library(readr)
library(dplyr)
library(psych)

#Load the data-set. 
train <- read.csv("D:/R/titanic/train.csv",stringsAsFactors = TRUE)
View(train)

test <- read.csv("D:/R/titanic/test.csv")
View(test)

# Basic info about data

describe(train)
summary(train)

#Lets look at the summary statistics 

table(train$Survived)
table(train$Sex)
table(train$Pclass)
table(train$Cabin)


#lets view survived as a proportional value
# 0=no and 1=yes

prop.table(table(train$Survived))


#Find out those passenger detail who dies
#Dies based on Ticket Class.
Total_Dies <- filter(train,as.numeric(Survived)==0)

#1.Ticket Class = 1st.
Ticket_Class_1 <- filter(train,as.numeric(Survived)==0 & as.numeric(Pclass)==1)
#2.Ticket Class = 2nd.
Ticket_Class_2 <- filter(train,as.numeric(Survived)==0 & as.numeric(Pclass)==2)
#3.Ticket Class = 3rd.
Ticket_Class_3 <- filter(train,as.numeric(Survived)==0 & as.numeric(Pclass)==3)

#create a submission for this dies case

write.csv(Ticket_Class_1,file = "D:/R/titanic/Ticket_class_1.csv",row.names = FALSE)
write.csv(Ticket_Class_2,file = "D:/R/titanic/Ticket_class_2.csv",row.names = FALSE)
write.csv(Ticket_Class_3,file = "D:/R/titanic/Ticket_class_3.csv",row.names = FALSE)

####################################################################################

#Gender Class model development

summary(train$Sex)

#prop table, Sex vs Survived
prop.table(table(train$Sex,train$Survived)) 

#Submit considering all Women Survived

#1.sex = Male.
Male_survived <- filter(train,as.numeric(Survived)==1 & as.character(Sex)=="male")
#2.sex = Female.
Female_survived <- filter(train,as.numeric(Survived)==1 & as.character(Sex)=="female")


write.csv(Female_survived,file = "D:/R/titanic/WomenSurvived.csv",row.names = FALSE)
write.csv(Male_survived,file = "D:/R/titanic/menSurvived.csv",row.names = FALSE)

#####################################################################################

#############################################################################################
#Decision Trees
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit)
text(fit)


#Now we can try making a submission with this decision tree predction
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(Name=test$Name,PassengerId = test$PassengerId, Survived = Prediction,Sex = test$Sex,Pclass=test$Pclass,Fare=test$Fare)
write.csv(submit, file = "D:/R/titanic/DecisionTreePrediction.csv", row.names = FALSE)






