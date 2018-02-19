library("readr")
library(tidyr)
library(dplyr)
library(ggplot2)


train = read_csv("train.csv")


#How many survived 

table(train$Survived)

# %age of hoe many survided

prop.table(table(train$Survived))*100

## Everyone dies prediction

test = read_csv("test.csv")  #missing Survie list
test

str(test)

# set everyone dies in test data set 

test$Survived = rep(0,418)  #418 obsv in test data set 

test


#We need to submit a csv file with the PassengerId as well as our Survived predictions to Kaggle. 
#So let’s extract those two columns from the test dataframe, store them in a new container, and then send it to an output file:

submitt = data_frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submitt, file = "theyallperish.csv" , row.names = FALSE )



#The disaster was famous for saving “women and children first”, 
#so let’s take a look at the Sex and Age variables to see if any patterns are evident. 


table(train$Sex)
## above seems majority of the passengers were male

## Propotion of males vs female survived 

prop.table(table(train$Sex,train$Survived))*100   ## this gives us wrong op; this simply divides all the count by total rows 

# we want is the row-wise propotion, propotion of each gender that survived

prop.table(table(train$Sex,train$Survived),1)*100

# We now can see that the majority of females aboard survived, and a very low percentage of males did

# in our last prediction "allperished" we said they all died., so we can see some of them survived. 
#So lets change our prediction 
#PRediction
test$Survived = 0
test
#modify prediction (test data set) where females are there 

test$Survived[test$Sex == 'female'] = 1

table(test$Survived)


submitt2 = data_frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submitt2, file = "prediction1.csv" , row.names = FALSE )

## Now secondly it was menitoned that Childerens were also saved than Men ., lets check childens using AGE 
# Data Analysis
summary(train$Age)

## We can see 177 NA's in age ie., missing values 
# create a new column Child 

train$child = 0
train
train$child[train$Age <= 18] = 1

str(train)

## now we want to see the dtaa inclusing Femals, CHildren ans males And their survial propotion 

aggregate(Survived~child + Sex , data = train , FUN = sum)

# find the propotion 

aggregate(Survived~child + Sex , data = train , FUN = function(x) {sum(x)/length(x)}*100)


#Well, it still appears that if a passenger is female most survive, and if they were male most don’t, regardless of whether they were a child or not. 
#So we haven’t got anything to change our predictions on here. 
#Let’s take a look at a couple of other potentially interesting variables to see if we can 
#find anything more: the class that they were riding in, and what they paid for their ticket.


summary(train$Fare)
#Let’s bin the fares into less than $10, between $10 and $20, $20 to $30 and more than $30 and store it to a new variable:


train$fare2 = '30+'
train$fare2[train$Fare < 30 & train$Fare >=20 ] = '20-30'
train$fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$fare2[train$Fare < 10] <- '<10'

# include the ticket range and PassengeClass they are traveelling in 
aggregate(Survived~fare2 + Pclass +  Sex, data = train , FUN = function(x) {sum(x)/length(x)}*100)

#While the majority of males, regardless of class or fare still don’t do so well, 
#we notice that most of the class 3 women who paid more than $20 for their ticket actually also miss out on a lifeboat

#Prediction 

test$Survived = 0
test$Survived[test$Sex == 'female'] = 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >=20] = 0 
                                                                      # from aboev analysis very less   womens of class 3 
    
                                                                       # with higher ticket rates survived

submitt3 = data_frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submitt3, file = "prediction2.csv" , row.names = FALSE )


## WE WILL NOW AUTOMATE THIS PREDICTION PROCESS - USING MACHINE LEARNING DECIOSN TREE IMPLEMNETATION 
#http://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/

#the algorithm starts with all of the data at the root node (drawn at the top) and scans all of the variables for the best one to split on. 
# in our case if it takes root node and gives info about  Gender = Male the it divides it %age survival at root node level 
#  Not it creates 2 branches - If it is Male 1 and No 0 and their respective survial percentage  - EXACTLY SAME AS SUBMITT2 ANALYSIS 
# now we can go wit this till every passenger get classifed and get %proption of survived. 

## DECISION TREE DRAWBACKS- COMPUTATIONAL GREEDY 
#Decision trees do have some drawbacks though, they are greedy. They make the decision on the current node which appear to be the best at the time, 
#but are unable to change their minds as they grow new nodes. Perhaps a better, more pure, tree would have been grown if 
#the gender split occurred later? It is really hard to tell, there are a huge number of decisions that could be made, and 
#exploring every possible version of a tree is extremely computationally expensive. This is why the greedy algorithm is used.

library(rpart)
##Package - RECURSIVE Partitioning and Regression TREE

#STEP1 - lets take look at possible vairbales ; we looked at age, sex, fare , class but never looked into SibSp, Embareked orParch 
        # and remaing variables like ticketnbr , cabin nbr , name wont ghive any uselful info let's ignore them 

#rpart format is excaty same as aggregatefunction 

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train , method = "class")
train

#  since we are seeing discrete varibale "Survived" we use "CLASS" ; if we predicting continous variable liek "AGE" we use ANOVA
plot(fit)
text(fit) 
#NOT CLEAER lets install additional lib

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

rpart.plot(fit)

# NOW we can see the analysis is lot deeper than what we manually did. upto age, class, sex,fare only

## Prediction - 
# Childrent below 6.5 has godd chance of survial 

# try o be more accurate with Conrol part 

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))

rpart.plot(fit)

Prediction <- predict(fit, test, type = "class")

submit4 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "dtreePrediction.csv", row.names = FALSE)

## having 78% accuracy in Kaggle - we need to increase this level up.
