install.packages("C50") # we neeed to install C50 package
install.packages("tree")
library(C50)
library(tree)
fraud<-read.csv(file.choose()) # read fraud_data csv file
View(fraud)
str(fraud)
summary(fraud)

# factorise the Taxable Income (O/P Variable) into 2 levels

fraud$Taxable.Income<- cut(fraud$Taxable.Income,breaks = c(0,30000,100000),labels = c("Risky","Good"))
levels(fraud$Taxable.Income)
nlevels(fraud$Taxable.Income)
str(fraud)
table(fraud$Taxable.Income)
summary(fraud)
attach(fraud)

###### Splitting data into training and testing by Random (80/20 ratio)#####

install.packages("caTools")
library(caTools)
split<-sample.split(fraud$Taxable.Income,SplitRatio = 0.80)
split
table(split)
fraud_train<-subset(fraud,split==TRUE)
fraud_test<-subset(fraud,split==FALSE)

prop.table(table(fraud_train$Taxable.Income))
prop.table(table(fraud_test$Taxable.Income))

# Building Decision Tree model on training data 

fraudc5.0_train <- C5.0(fraud_train,fraud_train$Taxable.Income)
windows()
plot(fraudc5.0_train) # Tree graph

# Training accuracy
pred_train <- predict(fraudc5.0_train,fraud_train)
table(pred_train,fraud_train$Taxable.Income)
table(fraud_train$Taxable.Income)

mean(fraud_train$Taxable.Income==pred_train) # 100% Accuracy

# Testing Accuracy
predc5.0_test <- predict(fraudc5.0_train,newdata=fraud_test) # predicting on test data
table(predc5.0_test,fraud_test$Taxable.Income)
table(fraud_test$Taxable.Income)
mean(predc5.0_test==fraud_test$Taxable.Income) # 100% Accuracy

library(gmodels)
# Cross tablez
CrossTable(fraud_test$Taxable.Income,predc5.0_test)


install.packages("{party",dependencies = TRUE)
library(party)

fraud_tree<-ctree(Taxable.Income~. , data = fraud_train)
summary(fraud_tree)
plot(fraud_tree)

######### Splitting data into training and testing by Random (70/30 ratio)#####

split1<-sample.split(fraud$Taxable.Income,SplitRatio = 0.70)
split1
table(split1)
fraud_train1<-subset(fraud,split1==TRUE)
fraud_test1<-subset(fraud,split1==FALSE)

prop.table(table(fraud_train1$Taxable.Income))
prop.table(table(fraud_test1$Taxable.Income))

# Building Decision Tree model on training data 
fraudc5.0_train1 <- C5.0(fraud_train1,fraud_train1$Taxable.Income)
windows()
plot(fraudc5.0_train1) # Tree graph

# Training accuracy
pred_train1 <- predict(fraudc5.0_train1,fraud_train1)
table(pred_train1,fraud_train1$Taxable.Income)
table(fraud_train1$Taxable.Income)

mean(fraud_train1$Taxable.Income==pred_train1) # 100% Accuracy

# Testing Accuracy
predc5.0_test1 <- predict(fraudc5.0_train1,newdata=fraud_test1) # predicting on test data
table(predc5.0_test1,fraud_test1$Taxable.Income)
table(fraud_test1$Taxable.Income)
mean(predc5.0_test1==fraud_test1$Taxable.Income) # 100% Accuracy

library(gmodels)
# Cross tablez
CrossTable(fraud_test1$Taxable.Income,predc5.0_test1)

######### Splitting data into training and testing by Stratified (75/25 ratio)#####

split1<-sample.split(fraud$Taxable.Income,SplitRatio = 0.70)
split1
table(split1)
fraud_train2<-fraud[1:450, ] # 75% data for Train
fraud_test2<-fraud[451:600, ] # 25% data for Test

prop.table(table(fraud_train2$Taxable.Income)) # Risky = 22.66% , Good= 77.33%
prop.table(table(fraud_test2$Taxable.Income)) # Risky = 14.66% , Good= 85.33%

# Building Decision Tree model on training data 
fraudc5.0_train2 <- C5.0(fraud_train2,fraud_train2$Taxable.Income)
windows()
plot(fraudc5.0_train2) # Tree graph

# Training accuracy
pred_train2 <- predict(fraudc5.0_train2,fraud_train2)
table(pred_train2,fraud_train2$Taxable.Income)
table(fraud_train2$Taxable.Income)

mean(fraud_train2$Taxable.Income==pred_train2) # 100% Accuracy

# Testing Accuracy
predc5.0_test2 <- predict(fraudc5.0_train2,newdata=fraud_test2) # predicting on test data
table(predc5.0_test2,fraud_test2$Taxable.Income)
table(fraud_test2$Taxable.Income)
mean(predc5.0_test2==fraud_test2$Taxable.Income) # 100% Accuracy

library(gmodels)
# Cross tablez
CrossTable(fraud_test2$Taxable.Income,predc5.0_test2)


