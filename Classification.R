#Install the following packages and their dependancies before running the program
library(RWeka)
library(party)
library(FSelector)
library(Rgraphviz)
library(caret)
library(jpeg)

#Laoad the dataset
LifeExpdata<-read.csv("./Data/Project1Dataset.csv")

divideDataset <-function(){
  set.seed(1234)
  indices<<-sample(1:nrow(LifeExpdata), size=0.2*nrow(LifeExpdata))
  test1 <<- LifeExpdata[indices,]
  train1 <<- LifeExpdata[-indices,]
  
  set.seed(222)
  indices<<-sample(1:nrow(LifeExpdata), size=0.2*nrow(LifeExpdata))
  test2<<-LifeExpdata[indices,]
  train2<<-LifeExpdata[-indices,]
  
  set.seed(44333)
  indices<<-sample(1:nrow(LifeExpdata), size=0.2*nrow(LifeExpdata))
  test3<<-LifeExpdata[indices,]
  train3<<-LifeExpdata[-indices,]
  
  set.seed(444)
  indices<<-sample(1:nrow(LifeExpdata), size=0.2*nrow(LifeExpdata))
  test4<<-LifeExpdata[indices,]
  train4<<-LifeExpdata[-indices,]
  
  set.seed(76576)
  indices<<-sample(1:nrow(LifeExpdata), size=0.2*nrow(LifeExpdata))
  test5<<-LifeExpdata[indices,]
  train5<<-LifeExpdata[-indices,]
}

myC45 <- function(trainDf,testDf,fileName){
  #Fit model 
  c45Fit <<- J48(trainDf$Continent~trainDf$Overall.Life+trainDf$Male.Life+trainDf$Female.Life, data=trainDf,control = Weka_control(R = TRUE))
  if(require("party", quietly = TRUE)) plot(c45Fit)
  #Summarize the fit to get the confusion matrix of C45
  summary(c45Fit)
  #Information Gain
  infoGainC45<<-information.gain(trainDf$Continent~trainDf$Overall.Life+trainDf$Male.Life+trainDf$Female.Life,data=trainDf)
  #To visualize in the form of Graph
  write_to_dot(c45Fit)
  ff <- tempfile()
  write_to_dot(c45Fit, ff)
  jpeg(file = paste(fileName,".jpeg"))
       plot(agread(ff))
  dev.off()
}

myC45Predict <- function(trainDf,testDf){
  predictions <- predict(J48(testDf$Continent~Overall.Life+Male.Life+testDf$Female.Life, data=testDf,control = Weka_control(R = TRUE)), testDf)
  conf_mat<<-table(predictions, testDf$Continent)
  #accuracy <- sum(diag(conf_mat))/sum(conf_mat)
  conf_mat_caret <- confusionMatrix(data = predictions, reference = testDf$Continent)
  accuracy <- conf_mat_caret$overall['Accuracy']
  precision <- conf_mat_caret$byClass[,'Pos Pred Value']  
  recall <- conf_mat_caret$byClass[,'Sensitivity']
  f_measure <- 2 * ((precision * recall) / (precision + recall))
  return(list(Accuracy=accuracy,Precision=precision,Recall=recall,FScore=f_measure))
}

myRipper <- function(trainDf,testDf){
  #Fit model 
  TrainData = trainDf[3:5]
  TrainClasses = trainDf$Continent
  jripFit <<- caret::train(trainDf[3:5],trainDf$Continent ,method = "JRip",preProcess = c("center", "scale"), trControl = trainControl(method = "cv"), 
                    tuneLength = 10)
  #if(require("party", quietly = TRUE)) plot(jripFit)
  #Summarize the fit to get the confusion matrix of C45
  #summary(jripFit)
}

myRipperPredict <- function(trainDf,testDf){
  predictions <- predict(jripFit, testDf)
  conf_mat<<-table(predictions, testDf$Continent)
  #accuracy <- sum(diag(conf_mat))/sum(conf_mat)
  conf_mat_caret <- confusionMatrix(data = predictions, reference = testDf$Continent)
  accuracy <- conf_mat_caret$overall['Accuracy']
  precision <- conf_mat_caret$byClass[,'Pos Pred Value']  
  recall <- conf_mat_caret$byClass[,'Sensitivity']
  f_measure <- 2 * ((precision * recall) / (precision + recall))
  return(list(Accuracy=accuracy,Precision=precision,Recall=recall,FScore=f_measure))
}

mySvm <- function(trainDf,testDf){
  #Fit model 
  svmFit <<- svm(trainDf$Continent~trainDf$Overall.Life+trainDf$Male.Life+trainDf$Female.Life, data=trainDf,control = Weka_control(R = TRUE))
  #if(require("party", quietly = TRUE)) plot(svmFit)
  #Summarize the fit to get the confusion matrix of C45
  summary(svmFit)
}

mySvmPredict <- function(trainDf,testDf){
  predictions <- predict(svm(testDf$Continent~testDf$Overall.Life+testDf$Male.Life+testDf$Female.Life, data=testDf,control = Weka_control(R = TRUE)), testDf,type="class")
  conf_mat<<-table(predictions, testDf$Continent)
  #accuracy <- sum(diag(conf_mat))/sum(conf_mat)
  conf_mat_caret <- confusionMatrix(data = predictions, reference = testDf$Continent)
  accuracy <- conf_mat_caret$overall['Accuracy']
  precision <- conf_mat_caret$byClass[,'Pos Pred Value']  
  recall <- conf_mat_caret$byClass[,'Sensitivity']
  f_measure <- 2 * ((precision * recall) / (precision + recall))
  return(list(Accuracy=accuracy,Precision=precision,Recall=recall,FScore=f_measure))
}

myKnn <- function(trainDf,testDf){
  #Fit model 
  knnFit <<- train(trainDf[3:5], trainDf$Continent,method = "knn",preProcess = c("center", "scale"), trControl = trainControl(method = "cv"), 
                   tuneLength = 10)
  #if(require("party", quietly = TRUE)) plot(knnFit)
  #Summarize the fit to get the confusion matrix of C45
  summary(knnFit)
}

myKnnPredict <- function(trainDf,testDf){
  predictions <- predict(knnFit, testDf)
  conf_mat<<-table(predictions, testDf$Continent)
  #accuracy <- sum(diag(conf_mat))/sum(conf_mat)
  conf_mat_caret <- confusionMatrix(data = predictions, reference = testDf$Continent)
  accuracy <- conf_mat_caret$overall['Accuracy']
  precision <- conf_mat_caret$byClass[,'Pos Pred Value']  
  recall <- conf_mat_caret$byClass[,'Sensitivity']
  f_measure <- 2 * ((precision * recall) / (precision + recall))
  return(list(Accuracy=accuracy,Precision=precision,Recall=recall,FScore=f_measure))
}


##Collect 5 different test and train samples in 80:20
divideDataset()

trainList = list(train1,train2,train3,train4,train5)
testList = list(test1,test2,test3,test4,test5)

##DecisionTree - C45 Algorithm
resultc45 <- list()
for (i in 1:5){
  myC45(trainList[[i]],testList[[i]],paste("C45_DT_",i))
  resultc45 = c(resultc45,Sample = myC45Predict(trainList[[i]],testList[[i]]))
}

##Decision Tree - Ripper Algorithm
resultRipper <- list()
for (i in 1:5){
  myRipper(trainList[[i]],testList[[i]])
  resultRipper = c(resultRipper,Sample = myRipperPredict(trainList[[i]],testList[[i]]))
}

##Support Vector Machine Model
resultSvm <- list()
for (i in 1:5){
  mySvm(trainList[[i]],testList[[i]])
  resultSvm = c(resultSvm,Sample = mySvmPredict(trainList[[i]],testList[[i]]))
}

##K Nearest Neighbor Model
resultKnn <- list()
for (i in 1:5){
  myKnn(trainList[[i]],testList[[i]])
  resultKnn = c(resultKnn,Sample = myKnnPredict(trainList[[i]],testList[[i]]))
}

##Performance Metrics
Accuracy_c45 <- mean(resultc45[[1]][[1]],resultc45[[5]][[1]],resultc45[[9]][[1]],resultc45[[13]][[1]],resultc45[[17]][[1]])
Precision_c45 <- mean(resultc45[[2]][[1]],resultc45[[6]][[1]],resultc45[[10]][[1]],resultc45[[14]][[1]],resultc45[[18]][[1]])
Recall_c45 <-mean(resultc45[[3]][[1]],resultc45[[7]][[1]],resultc45[[11]][[1]],resultc45[[15]][[1]],resultc45[[19]][[1]])
Fscore_c45 <-mean(resultc45[[4]][[1]],resultc45[[8]][[1]],resultc45[[12]][[1]],resultc45[[16]][[1]],resultc45[[20]][[1]])
c45_metrics = list(Accuracy = Accuracy_c45, Precision = Precision_c45, Recall = Recall_c45,Fscore = Fscore_c45)

Accuracy_Ripper <- mean(resultRipper[[1]][[1]],resultRipper[[5]][[1]],resultRipper[[9]][[1]],resultRipper[[13]][[1]],resultRipper[[17]][[1]])
Precision_Ripper <- mean(resultRipper[[2]][[1]],resultRipper[[6]][[1]],resultRipper[[10]][[1]],resultRipper[[14]][[1]],resultRipper[[18]][[1]])
Recall_Ripper <-mean(resultRipper[[3]][[1]],resultRipper[[7]][[1]],resultRipper[[11]][[1]],resultRipper[[15]][[1]],resultRipper[[19]][[1]])
Fscore_Ripper <-mean(resultRipper[[4]][[1]],resultRipper[[8]][[1]],resultRipper[[12]][[1]],resultRipper[[16]][[1]],resultRipper[[20]][[1]])
Ripper_metrics = list(Accuracy = Accuracy_Ripper, Precision = Precision_Ripper, Recall = Recall_Ripper,Fscore = Fscore_Ripper)

Accuracy_Svm <- mean(resultSvm[[1]][[1]],resultSvm[[5]][[1]],resultSvm[[9]][[1]],resultSvm[[13]][[1]],resultSvm[[17]][[1]])
Precision_Svm <- mean(resultSvm[[2]][[1]],resultSvm[[6]][[1]],resultSvm[[10]][[1]],resultSvm[[14]][[1]],resultSvm[[18]][[1]])
Recall_Svm <-mean(resultSvm[[3]][[1]],resultSvm[[7]][[1]],resultSvm[[11]][[1]],resultSvm[[15]][[1]],resultSvm[[19]][[1]])
Fscore_Svm <-mean(resultSvm[[4]][[1]],resultSvm[[8]][[1]],resultSvm[[12]][[1]],resultSvm[[16]][[1]],resultSvm[[20]][[1]])
Svm_metrics = list(Accuracy = Accuracy_Svm, Precision = Precision_Svm, Recall = Recall_Svm,Fscore = Fscore_Svm)

Accuracy_Knn <- mean(resultKnn[[1]][[1]],resultKnn[[5]][[1]],resultKnn[[9]][[1]],resultKnn[[13]][[1]],resultKnn[[17]][[1]])
Precision_Knn <- mean(resultKnn[[2]][[1]],resultKnn[[6]][[1]],resultKnn[[10]][[1]],resultKnn[[14]][[1]],resultKnn[[18]][[1]])
Recall_Knn <-mean(resultKnn[[3]][[1]],resultKnn[[7]][[1]],resultKnn[[11]][[1]],resultKnn[[15]][[1]],resultKnn[[19]][[1]])
Fscore_Knn <-mean(resultKnn[[4]][[1]],resultKnn[[8]][[1]],resultKnn[[12]][[1]],resultKnn[[16]][[1]],resultKnn[[20]][[1]])
Knn_metrics = list(Accuracy = Accuracy_Knn, Precision = Precision_Knn, Recall = Recall_Knn,Fscore = Fscore_Knn)

