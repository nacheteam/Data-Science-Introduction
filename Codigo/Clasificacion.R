library(foreign)
library(dplyr)
dataset_clasificacion<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart.dat", header=FALSE, comment.char = "@")
colnames(dataset_clasificacion)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
dataset_clasificacion

colnames(dataset_clasificacion)
attach(dataset_clasificacion)

library(caret)
library(class)

library(MASS)
library(ISLR)

###########################################################################
##                            Apartado 1                                 ##
###########################################################################

readFolds<-function(){
  fold1.train<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-1tra.dat", header=FALSE, comment.char = "@")
  fold1.test<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-1tst.dat", header=FALSE, comment.char = "@")
  colnames(fold1.train)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  colnames(fold1.test)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  
  fold2.train<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-2tra.dat", header=FALSE, comment.char = "@")
  fold2.test<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-2tst.dat", header=FALSE, comment.char = "@")
  colnames(fold2.train)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  colnames(fold2.test)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  
  fold3.train<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-3tra.dat", header=FALSE, comment.char = "@")
  fold3.test<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-3tst.dat", header=FALSE, comment.char = "@")
  colnames(fold3.train)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  colnames(fold3.test)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  
  fold4.train<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-4tra.dat", header=FALSE, comment.char = "@")
  fold4.test<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-4tst.dat", header=FALSE, comment.char = "@")
  colnames(fold4.train)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  colnames(fold4.test)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  
  fold5.train<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-5tra.dat", header=FALSE, comment.char = "@")
  fold5.test<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-5tst.dat", header=FALSE, comment.char = "@")
  colnames(fold5.train)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  colnames(fold5.test)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  
  fold6.train<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-6tra.dat", header=FALSE, comment.char = "@")
  fold6.test<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-6tst.dat", header=FALSE, comment.char = "@")
  colnames(fold6.train)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  colnames(fold6.test)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  
  fold7.train<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-7tra.dat", header=FALSE, comment.char = "@")
  fold7.test<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-7tst.dat", header=FALSE, comment.char = "@")
  colnames(fold7.train)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  colnames(fold7.test)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  
  fold8.train<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-8tra.dat", header=FALSE, comment.char = "@")
  fold8.test<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-8tst.dat", header=FALSE, comment.char = "@")
  colnames(fold8.train)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  colnames(fold8.test)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  
  fold9.train<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-9tra.dat", header=FALSE, comment.char = "@")
  fold9.test<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-9tst.dat", header=FALSE, comment.char = "@")
  colnames(fold9.train)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  colnames(fold9.test)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  
  fold10.train<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-10tra.dat", header=FALSE, comment.char = "@")
  fold10.test<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart-10-10tst.dat", header=FALSE, comment.char = "@")
  colnames(fold10.train)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  colnames(fold10.test)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  
  folds<-list(fold1=list(train=fold1.train, test=fold1.test),
              fold2=list(train=fold2.train, test=fold2.test),
              fold3=list(train=fold3.train, test=fold3.test),
              fold4=list(train=fold4.train, test=fold4.test),
              fold5=list(train=fold5.train, test=fold5.test),
              fold6=list(train=fold6.train, test=fold6.test),
              fold7=list(train=fold7.train, test=fold7.test),
              fold8=list(train=fold8.train, test=fold8.test),
              fold9=list(train=fold9.train, test=fold9.test),
              fold10=list(train=fold10.train, test=fold10.test))
  
  return(folds)
}

folds<-readFolds()

ejecutaKNN<-function(k, type="test"){
  cat("Valor de k: ", k, ", tipo: ", type)
  acc<-0
  for(i in 1:10){
    test<-folds[[i]]$test
    train<-folds[[i]]$train
    if(type=="test"){
      pred <- knn(train = train[,-length(train)], test = test[,-length(test)], cl = train[,length(train)], k=k)
      
      cat("\nFold ", i, ": ", mean(pred==test[,length(test)])*100, "%")
      acc<-acc+mean(pred==test[,length(test)])*100
      #print(table(pred,test[,length(test)]))
    }
    else{
      pred <- knn(train = train[,-length(train)], test = train[,-length(train)], cl = train[,length(train)], k=k)
      
      cat("\nFold ", i, ": ", mean(pred==train[,length(train)])*100, "%")
      acc<-acc+mean(pred==train[,length(train)])*100
      #print(table(pred,test[,length(test)]))
    }
  }
  acc<-acc/10
  cat("\nAccuracy medio: ", acc)
}

ejecutaKNN(3)
ejecutaKNN(5)
ejecutaKNN(7)
ejecutaKNN(9)
ejecutaKNN(11)
ejecutaKNN(13)
ejecutaKNN(15)
ejecutaKNN(17)
ejecutaKNN(19)
ejecutaKNN(21)
ejecutaKNN(23)

# El mejor valor es k=19

ejecutaKNN(3, "train")
ejecutaKNN(5, "train")
ejecutaKNN(7, "train")
ejecutaKNN(9, "train")
ejecutaKNN(11, "train")
ejecutaKNN(13, "train")
ejecutaKNN(15, "train")
ejecutaKNN(17, "train")
ejecutaKNN(19, "train")
ejecutaKNN(21, "train")
ejecutaKNN(23, "train")

###########################################################################
##                            Apartado 2                                 ##
###########################################################################

# Vamos a comprobar si nuestras variables cumplen la precondición de que son normales
# Sabemos ya por el EDA que no XD

# Prediciendo sobre el test
mean_score<-0
for(i in 1:10){
  test<-folds[[i]]$test
  train<-folds[[i]]$train
  model<-lda(Class~.,data=train)
  
  lda.pred <- predict(model,test)
  cat("\nFold ", i, ":")
  print(table(lda.pred$class,test$Class))
  print(mean(lda.pred$class==test$Class))
  mean_score<-mean_score+mean(lda.pred$class==test$Class)
}
mean_score<-mean_score/10
cat("\nScore final: ", mean_score)

# Prediciendo sobre el train
mean_score<-0
for(i in 1:10){
  test<-folds[[i]]$test
  train<-folds[[i]]$train
  model<-lda(Class~.,data=train)
  
  lda.pred <- predict(model,train)
  cat("\nFold ", i, ":")
  print(table(lda.pred$class,train$Class))
  print(mean(lda.pred$class==train$Class))
  mean_score<-mean_score+mean(lda.pred$class==train$Class)
}
mean_score<-mean_score/10
cat("\nScore final: ", mean_score)

###########################################################################
##                            Apartado 3                                 ##
###########################################################################

# Prediciendo sobre el test
mean_score<-0
for(i in 1:10){
  test<-folds[[i]]$test
  train<-folds[[i]]$train
  model<-qda(Class~.,data=train)
  
  lda.pred <- predict(model,test)
  cat("\nFold ", i, ":")
  print(table(lda.pred$class,test$Class))
  print(mean(lda.pred$class==test$Class))
  mean_score<-mean_score+mean(lda.pred$class==test$Class)
}
mean_score<-mean_score/10
cat("\nScore final: ", mean_score)

# Prediciendo sobre el train
mean_score<-0
for(i in 1:10){
  test<-folds[[i]]$test
  train<-folds[[i]]$train
  model<-qda(Class~.,data=train)
  
  lda.pred <- predict(model,train)
  cat("\nFold ", i, ":")
  print(table(lda.pred$class,train$Class))
  print(mean(lda.pred$class==train$Class))
  mean_score<-mean_score+mean(lda.pred$class==train$Class)
}
mean_score<-mean_score/10
cat("\nScore final: ", mean_score)

###########################################################################
##                            Apartado 4                                 ##
###########################################################################

