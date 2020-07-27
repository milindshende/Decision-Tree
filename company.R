install.packages("C50") # we neeed to install C50 package
install.packages("tree")
library(C50)
library(tree)
company<-read.csv(file.choose()) # read company_data csv file
View(company)
str(company)
summary(company)
hist(company$Sales)

# factorise the Sales (O/P Variable) into 2 levels , based on summary of Sales variable.
# 3rd Quartile is 9.32 means almost 75% values covered. Hence Sales above 9.32 defined as High Sales.

company$Sales<- cut(company$Sales,breaks = c(-1,9.32,17),labels = c("Sale","High Sale"))
levels(company$Sales)
nlevels(company$Sales)
str(company)
table(company$Sales)
summary(company)
attach(company)

###### Splitting data into training and testing by Random (75/25 ratio)#####

install.packages("caTools")
library(caTools)
split<-sample.split(company$Sales,SplitRatio = 0.75)
split
table(split)
company_train<-subset(company,split==TRUE)
company_test<-subset(company,split==FALSE)

prop.table(table(company_train$Sales)) # Sale=75.33% , High Sale=24.66%
prop.table(table(company_test$Sales)) # Sale=75% , High Sale=25%

# Building Decision Tree model on training data 
companyc5.0_train <- C5.0(company_train[,-1],company_train$Sales)
windows()
plot(companyc5.0_train) # Tree graph

# Training accuracy
pred_train <- predict(companyc5.0_train,company_train)
table(pred_train,company_train$Sales)
table(company_train$Sales)

mean(company_train$Sales==pred_train) # 93.33% Accuracy

# Testing Accuracy
predc5.0_test <- predict(companyc5.0_train,newdata=company_test) # predicting on test data
table(predc5.0_test,company_test$Sales)
table(company_test$Sales)
mean(predc5.0_test==company_test$Sales) # 85% Accuracy

library(gmodels)
# Cross tablez
CrossTable(company_test$Sales,predc5.0_test)

### Plot Decision Tree to analyse the attributes/segments which contribute High Sale### 

install.packages("{party",dependencies = TRUE)
library(party)

company_tree1<-ctree(Sales~. , data = company_train)
summary(company_tree1)
plot(company_tree1)

######### Splitting data into training and testing by Random (60/40 ratio)#####

install.packages("caTools")
library(caTools)

split1<-sample.split(company$Sales,SplitRatio = 0.60)
split1
table(split1)

company_train1<-subset(company,split1==TRUE)
company_test1<-subset(company,split1==FALSE)

prop.table(table(company_train1$Sales)) # Sale=75.42% , High Sale=24.58%
prop.table(table(company_test1$Sales)) # Sale=75% , High Sale=25%

# Building Decision Tree model on training data 
companyc5.0_train1 <- C5.0(company_train1[,-1],company_train1$Sales)
windows()
plot(companyc5.0_train1) # Tree graph

# Training accuracy
pred_train1 <- predict(companyc5.0_train1,company_train1)
table(pred_train1,company_train1$Sales)
table(company_train1$Sales)

mean(company_train1$Sales==pred_train1) # 94.16% Accuracy

# Testing Accuracy
predc5.0_test1 <- predict(companyc5.0_train1,newdata=company_test1) # predicting on test data
table(predc5.0_test1,company_test1$Sales)
table(company_test1$Sales)
mean(predc5.0_test1==company_test1$Sales) # 80% Accuracy

library(gmodels)
# Cross tablez
CrossTable(company_test1$Sales,predc5.0_test1)

######### Splitting data into training and testing by Stratified (80/20 ratio)#####

install.packages("caTools")
library(caTools)

company_train2<-company[1:320, ] # 80% of data for Train
company_test2<-company[321:400,] # 20% of data for Test

prop.table(table(company_train2$Sales)) # Sales=75.94% High Sale=24.06%
prop.table(table(company_test2$Sales)) # Sales=72.5% High Sale=25.5%

# Building Decision Tree model on training data 
companyc5.0_train2 <- C5.0(company_train2[,-1],company_train2$Sales)
windows()
plot(companyc5.0_train2) # Tree graph

# Training accuracy
pred_train2 <- predict(companyc5.0_train2,company_train2)
table(pred_train2,company_train2$Sales)
table(company_train2$Sales)

mean(company_train2$Sales==pred_train2) # 95% Accuracy

# Testing Accuracy
predc5.0_test2 <- predict(companyc5.0_train2,newdata=company_test2) # predicting on test data
table(predc5.0_test2,company_test2$Sales)
table(company_test2$Sales)
mean(predc5.0_test2==company_test2$Sales) # 76.25% Accuracy

library(gmodels)
# Cross tablez
CrossTable(company_test2$Sales,predc5.0_test2)

########################################################

# On looking into the Above 3 models accuracy , first model (75/25 ratio) seems suitable
# on looking into its tree plot it is clear that ->

# if the shelve Location is good and price <=136
# then there is a probability of around 80% chance that the Sales will go high

# Also if the shelve Location is Bad/Medium and price is <=89 
# then there is a probability of around 60% chance that the Sales will go high

