rm(list=ls())

#####################
#### SECTION 1 ####
#####################


x<-matrix(c(1,0,1,3,3,1,0,11,2,1,1,7),nrow=4,ncol=3)

I<-c(1,1,1,1)

xcentered <- x - I %*% t(I) %*% x*(1/4)

#Display a confirmation that the mean of each column is 0
meanofcolumns<-colMeans(xcentered)
meanofcolumns   

#This next piece of code creates a vector of the stdevs of xcentered.
xsd<-apply(xcentered,2,sd)


#This next piece of code creates a matrix of the stdevs of xsd, with the stdevs going down
# at a diagonal from top left to bottom right.
xscale<-xcentered%*%diag(1/xsd)

#This next piece of code takes the matrix of xscale and creates a vector of the stdevs from each column.
xscalesd<-apply(xscale, 2, sd)
xscalesd

#covariance
sigma<-(t(xscale) %*% xscale)/(4-1)
sigma

#Checking calculation by displaying the results of the cov() function
cov(xscale)
eigen(sigma)

#Checking calculations using the scale, prcomp, functions.
x<-matrix(c(1,0,1,3,3,1,0,11,2,1,1,7),nrow=4,ncol=3)
x1=scale(x)

prc<-prcomp(x1)
prc

U<- prc$rotation[,-3]

Z <- x1 %*% U
Z

#####################
#### SECTION 2 ###### 
#####################
rm(list=ls())
#install.packages("ISLR")
#install.packages("pls")
library(pls)
library(ISLR)
Hitters = na.omit(Hitters)
set.seed(2)
pcr.fit1=pcr(Salary~., data=Hitters, scale=TRUE, validation = "CV")
summary(pcr.fit1)
validationplot(pcr.fit1,val.type="MSEP")
set.seed(1)

x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]


pcr.fit2=pcr(Salary~., data=Hitters, subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit2,val.type="MSEP")
pcr.pred=predict(pcr.fit2,x[test,],ncomp=7)

mean((pcr.pred-y.test)^2)

pcr.fit2=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit2)

set.seed (1)
pls.fit=plsr(Salary~., data=Hitters ,subset=train ,scale=TRUE ,validation ="CV")
summary(pls.fit)
validationplot(pls.fit ,val.type="MSEP")
pls.pred=predict (pls.fit ,x[test ,], ncomp =2)
mean((pls.pred -y.test)^2)
pls.fit=plsr(Salary~., data=Hitters ,scale=TRUE ,ncomp =2)
summary(pls.fit)


#####################
#### SECTION 3 ###### 
#####################
data1<- read.csv("https://raw.githubusercontent.com/Jonasyao/Machine-Learning-Specialization-University-of-Washington-/master/Regression/Assignment_four/kc_house_data.csv")
str(data1)
data2<-na.omit(data1)
data3<-data2[,-(1:2)]
data4<-scale(data3)
data5<-data.frame(data4)
str(data5)
set.seed(1)

#Create an 80/20 training set
train<-sample(1:nrow(data5), nrow(data5)*.80)
test<-(-train)
trainset <-data5[train,]
testset<-data5[test,]

#Using training set create a linear regression model
lm.fit <-lm(price~.,data= trainset)
summary(lm.fit)
alias(lm.fit)

#Remove sqft_living
trainset2 <-trainset[,-4]
testset2 <-testset[,-4]

lm.fit2<-lm(price~.,data=trainset2)
summary(lm.fit2)
alias(lm.fit2)

#Calculate the MSE and save for later comparison
lm.pred2 <- predict(lm.fit2, testset2)
mean((lm.pred2-testset2$price)^2)

set.seed(1)

#Create a PCR, but do not scale the data, it has already been scaled. 

pcr.fit3 = pcr(price~., data=trainset2, scale=FALSE,validation="CV")
validationplot(pcr.fit3,val.type="MSEP")
pcr.pred3 = predict(pcr.fit3, testset2, ncomp=11)
mean((pcr.pred3-testset2$price)^2)

#####################
#### SECTION 4 ###### 
#####################

set.seed(1)
pls.fit = plsr(price~., data = trainset2, scale = FALSE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit,val.type= "MSEP")
pls.pred = predict(pls.fit,testset2, ncomp=5)
mean((pls.pred-testset2$price)^2)

#The linear model preforms best and has the lowest MSE of .2871457











