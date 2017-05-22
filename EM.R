train=read.csv("C:\\Users\\shuzhang\\Downloads\\lab4-train.csv")
test=read.csv("C:\\Users\\shuzhang\\Downloads\\lab4-test.csv")

train1=train
train1[,1:4]=train1[,1:4]/max(train1[,1:4])
test1=test
test1[,1:4]=test1[,1:4]/max(test1[,1:4])

train2=train1
train2$Class=as.factor(train2$Class)
test2=test1
test2$Class=as.factor(test2$Class)

###################Randome forest########################################
library(randomForest)
train$Class=as.factor((train$Class))
test$Class=as.factor((test$Class))
###tune ntree
ntree=function(n){
  best.ntree=1
  accuracyTest=vector("numeric")
  for(i in 20:n){
    set.seed(100) 
    model <- randomForest(Class ~ ., data = train2,ntree=i)
    pred <- predict(model, newdata = test2[,1:4])
    table=table(pred, test2$Class)
    accuracy=sum(diag(table))/sum(table)
    #print(i)
    accuracyTest=c(accuracyTest,accuracy)
}
  best.ntree=which.max(accuracyTest)
  model <- randomForest(Class ~ ., data = train2,ntree=best.ntree)
  pred <- predict(model, newdata = test2[,1:4],type="class")
  table=table(pred, test2$Class)
  accuracy=sum(diag(table))/sum(table)
  list(best.ntree,pred,table,accuracy)
}
a=ntree(200)
predicted.rf=ntree(200)[[2]]
table.rf=ntree(200)[[3]]
accuracy.rf=ntree(200)[[4]]
####tune mtry
#set.seed(100) 
#mtry=tuneRF(train[,-5],train$Class,1, ntreeTry=83, stepFactor=2, improve=0.05,
##            trace=TRUE, plot=TRUE, doBest=FALSE)
#best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
#print(mtry)
#print(best.m)


###############################AdaBoost.M1 with decision stumps#####################################
#Import Library
#library(caret)
#Fitting model
#fitControl <- trainControl( method = "repeatedcv",
  #                        number = 4, repeats = 4)
#model.ab <- train(Class ~ ., data = train2, method = "ada",
 #             trControl = fitControl,verbose = FALSE)
#predicted.ab= predict(model.ab,test2[,1:4],type="response")
#table.ab=table(predicted,test2$Class)
#table.ab
#accuracy.ab=sum(diag(table.ab))/sum(table.ab)
#accuracy.ab
######################Adaboost.M1, with decision stamps. #######################################33
install.packages("adabag")
library("adabag")
set.seed(125)
model.ab <- boosting(Class ~ ., data = train2,control = rpart.control(maxdepth = 1,cp=0.005))

barplot(model.ab$imp[order(model.ab$imp, decreasing = TRUE)],
        ylim = c(0, 100), main = "Variables Relative Importance",
         col = "lightblue")
predicted.ab=predict(model.ab,test2[,1:4])
predicted.ab$class
table.ab=table(predicted.ab$class, test2[,5])
table.ab
accuracy.ab=sum(diag(table.ab))/sum(table.ab)
accuracy.ab

set.seed(125)
model.ab <- boosting(Class ~ ., data = train2,control = rpart.control(maxdepth = 1,cp=0.01))

barplot(model.ab$imp[order(model.ab$imp, decreasing = TRUE)],
        ylim = c(0, 100), main = "Variables Relative Importance",
        col = "lightblue")
predicted.ab=predict(model.ab,test2[,1:4])
predicted.ab$class
table.ab=table(predicted.ab$class, test2[,5])
table.ab
accuracy.ab=sum(diag(table.ab))/sum(table.ab)
accuracy.ab


set.seed(125)
model.ab <- boosting(Class ~ ., data = train2,control = rpart.control(maxdepth = 1,cp=0.05))

barplot(model.ab$imp[order(model.ab$imp, decreasing = TRUE)],
        ylim = c(0, 100), main = "Variables Relative Importance",
        col = "lightblue")
predicted.ab=predict(model.ab,test2[,1:4])
predicted.ab$class
table.ab=table(predicted.ab$class, test2[,5])
table.ab
accuracy.ab=sum(diag(table.ab))/sum(table.ab)
accuracy.ab



#################Neura network(NN) model##############################################################3
library(nnet)
# fit model model.ab$class


model.nn <- nnet(Class~., data=train2, size=4, decay=0.0001, maxit=6000)
# summarize the fit
summary(model.nn)
# make predictions
predicted.nn <- predict(model.nn , test2[,1:4], type="class")
# summarize accuracy
table.nn=table(predicted.nn, test2$Class)
table.nn
accucary.nn=sum(diag(table.nn))/sum(table.nn)
accucary.nn

model.nn <- nnet(Class~., data=train2, size=4, decay=0.001, maxit=6000)
# summarize the fit
summary(model.nn)
# make predictions
predicted.nn <- predict(model.nn , test2[,1:4], type="class")
# summarize accuracy
table.nn=table(predicted.nn, test2$Class)
table.nn
accucary.nn=sum(diag(table.nn))/sum(table.nn)
accucary.nn

model.nn <- nnet(Class~., data=train2, size=4, decay=0.005, maxit=6000)
# summarize the fit
summary(model.nn)
# make predictions
predicted.nn <- predict(model.nn , test2[,1:4], type="class")
# summarize accuracy
table.nn=table(predicted.nn, test2$Class)
table.nn
accucary.nn=sum(diag(table.nn))/sum(table.nn)
accucary.nn

#train.nnet=nnet(X_train, Y_train,Xcv.proc, Y_V,reg = 0.0001,learningRate=0.5,  h = 13, m=0.05, niteration=1000) 
#predicted_class_Test <- nnetPred(XT.proc, nnet.mnist)

#install.packages("neuralnet")
#library(neuralnet)
#train_NN=neuralnet(Class~R+F+M+T,data=train,err.fct="ce", linear.output=FALSE, likelihood=TRUE)


##############################Logistic regression##################################################
#Train the model using the training sets and check
#score

logistic <- glm(Class ~ ., data = train2,family=binomial("logit"))
anova(logistic)
summary(logistic)

head(train2)
model.lr <- glm(Class ~ ., data = train2[,-3],family=binomial("logit"))
summary(model.LR)
anova(model.LR)

#Predict Output
predicted.lr= predict(model.lr,test2[,c(1,2,4)],type="response")
predicted.lr <- ifelse(predicted.lr > 0.4,1,0)
table.lr=table(predicted.lr, test2$Class)
table.lr
accuracy.lr=sum(diag(table.lr))/sum(table.lr)
accuracy.lr



##############################KNN##################################################################


#Import Library
install.packages("class")
library(class)
#Fitting model
model.knn <-knn(train2[,-5],test2[,-5],train2$Class,k=5)
#install.packages("gmodels")
#library(gmodels)
#CrossTable(x=test2$Class,y=fit,prop.chisq = FALSE)
predicted.knn=model.knn
table.knn=table(predicted.knn, test2$Class)
table.knn
accuracy.knn=sum(diag(table.knn))/sum(table.knn)
accuracy.knn

#########################Naive Bayes(NB)##################################################
#Import Library
library(e1071)
#Fitting model
model.nb <-naiveBayes(Class ~ ., data = train2)
summary(model.nb)
#Predict Output
predicted.nb= predict(model.nb,test2[,1:4])

table.nb=table(predicted.nb, test2$Class)
table.nb
accuracy.nb=sum(diag(table.nb))/sum(table.nb)
accuracy.nb


###########################Decision Tree(DT)##############################################

#Import Library
library(rpart)
#grow tree
model.dt <- rpart(Class~., data = train2,method="class")
predicted.dt=predict(model.dt,test2[,1:4], type = 'class')
table.dt=table(predicted.dt, test2$Class)
table.dt
accuracy.dt=sum(diag(table.dt))/sum(table.dt)
accuracy.dt


 ########## an ensemble classifier using unweighted majority vote over the 5 models ##############   


class(predicted.lr)
class(predicted.knn)
class(predicted.dt)
class(predicted.nb)
class(predicted.nn)


#(predicted.lr+as.numepredicted.knn+predicted.dt+predicted.nb+predicted.nn)/nrow(test2$Class)

df=cbind(predicted.lr,as.numeric(levels(predicted.knn))[predicted.knn],as.numeric(levels(predicted.dt))[predicted.dt],as.numeric(levels(predicted.nb))[predicted.nb],as.numeric(predicted.nn))
colnames(df)=c("predicted.lr","predicted.knn","predicted.dt","predicted.nb","predicted.nn")
predicted.ensemble.uw=apply(df,1,function(x) names(which.max(table(x))))
predicted.ensemble.uw
table.ensemble.uw=table(predicted.ensemble.uw,test2$Class)
table.ensemble.uw
accucary.ensemble.uw=sum(diag(table.ensemble.uw))/sum(table.ensemble.uw)
accucary.ensemble.uw


########## an ensemble classifier using weighted majority vote over the 5 models####################
head(df)

weight1=c(1,3,5,7,9)
df1=df*weight1
predicted.ensemble.w=apply(df1,1,sum)
table(predicted.ensemble.w)
predicted.ensemble.w=ifelse(predicted.ensemble.w>8,1,0)
predicted.ensemble.w
table.ensemble.w=table(predicted.ensemble.w,test2$Class)
table.ensemble.w
accucary.ensemble.w=sum(diag(table.ensemble.w))/sum(table.ensemble.w)
accucary.ensemble.w


weight2=c(5,4,5,4,4)
df1=df*weight2
predicted.ensemble.w=apply(df1,1,sum)
table(predicted.ensemble.w)
predicted.ensemble.w=ifelse(predicted.ensemble.w>8,1,0)
predicted.ensemble.w
table.ensemble.w=table(predicted.ensemble.w,test2$Class)
table.ensemble.w
accucary.ensemble.w=sum(diag(table.ensemble.w))/sum(table.ensemble.w)
accucary.ensemble.w


weight3=c(accuracy.lr/(1-accuracy.lr),accuracy.knn/(1-accuracy.knn),accuracy.dt/(1-accuracy.dt),accuracy.nb/(1-accuracy.nb),accucary.nn/(1-accucary.nn))
df1=df*weight
predicted.ensemble.w=apply(df1,1,sum)
table(predicted.ensemble.w)
predicted.ensemble.w=ifelse(predicted.ensemble.w>8,1,0)
predicted.ensemble.w
table.ensemble.w=table(predicted.ensemble.w,test2$Class)
table.ensemble.w
accucary.ensemble.w=sum(diag(table.ensemble.w))/sum(table.ensemble.w)
accucary.ensemble.w

#######unweighted majority vote ensemble classifier over seven models##################################
class(predicted.rf)
class(predicted.ab$class)

df.7=cbind(predicted.lr,as.numeric(levels(predicted.knn))[predicted.knn],as.numeric(levels(predicted.dt))[predicted.dt],
           as.numeric(levels(predicted.nb))[predicted.nb],as.numeric(predicted.nn),
           as.numeric(levels(predicted.rf))[predicted.rf],as.numeric(predicted.ab$class))
colnames(df.7)=c("predicted.lr","predicted.knn","predicted.dt","predicted.nb","predicted.nn","predicted.rf","predicted.ab")
predicted.ensemble.uw=apply(df.7,1,function(x) names(which.max(table(x))))
predicted.ensemble.uw
table.ensemble.uw=table(predicted.ensemble.uw,test2$Class)
table.ensemble.uw
accucary.ensemble.uw=sum(diag(table.ensemble.uw))/sum(table.ensemble.uw)
accucary.ensemble.uw


#######weighted majority vote ensemble classifier over seven models##################################

weight4=c(1,3,5,7,9,11,13)
df1.7=df.7*weight4
predicted.ensemble.w=apply(df1.7,1,sum)
table(predicted.ensemble.w)
predicted.ensemble.w=ifelse(predicted.ensemble.w>8,1,0)
predicted.ensemble.w
table.ensemble.w=table(predicted.ensemble.w,test2$Class)
table.ensemble.w
accucary.ensemble.w=sum(diag(table.ensemble.w))/sum(table.ensemble.w)
accucary.ensemble.w


weight5=c(5,4,5,4,4,5,4)
df1.7=df.7*weight5
predicted.ensemble.w=apply(df1.7,1,sum)
table(predicted.ensemble.w)
predicted.ensemble.w=ifelse(predicted.ensemble.w>8,1,0)
predicted.ensemble.w
table.ensemble.w=table(predicted.ensemble.w,test2$Class)
table.ensemble.w
accucary.ensemble.w=sum(diag(table.ensemble.w))/sum(table.ensemble.w)
accucary.ensemble.w


weight6=c(accuracy.lr/(1-accuracy.lr),accuracy.knn/(1-accuracy.knn),accuracy.dt/(1-accuracy.dt),accuracy.nb/(1-accuracy.nb),
          accucary.nn/(1-accucary.nn),accuracy.rf/(1-accuracy.rf),accuracy.ab/(1-accuracy.ab))
df1.7=df.7*weight6
predicted.ensemble.w=apply(df1.7,1,sum)
table(predicted.ensemble.w)
predicted.ensemble.w=ifelse(predicted.ensemble.w>8,1,0)
predicted.ensemble.w
table.ensemble.w=table(predicted.ensemble.w,test2$Class)
table.ensemble.w
accucary.ensemble.w=sum(diag(table.ensemble.w))/sum(table.ensemble.w)
accucary.ensemble.w



