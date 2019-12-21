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

# Leemos los folds
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

# Función que ejecuta KNN sobre test o train con el valor de k introducido
ejecutaKNN<-function(k, type="test"){
  cat("Valor de k: ", k, ", tipo: ", type)
  acc<-0
  aciertos<-vector(mode="numeric", length = 10)
  for(i in 1:10){
    test<-folds[[i]]$test
    train<-folds[[i]]$train
    if(type=="test"){
      pred <- knn(train = train[,-length(train)], test = test[,-length(test)], cl = train[,length(train)], k=k)
      
      cat("\nFold ", i, ": ", mean(pred==test[,length(test)])*100, "%")
      aciertos[i]<-mean(pred==test[,length(test)])*100
      acc<-acc+mean(pred==test[,length(test)])*100
      #print(table(pred,test[,length(test)]))
    }
    else{
      pred <- knn(train = train[,-length(train)], test = train[,-length(train)], cl = train[,length(train)], k=k)
      
      cat("\nFold ", i, ": ", mean(pred==train[,length(train)])*100, "%")
      aciertos[i]<-mean(pred==train[,length(train)])*100
      acc<-acc+mean(pred==train[,length(train)])*100
      #print(table(pred,test[,length(test)]))
    }
  }
  acc<-acc/10
  cat("\nAccuracy medio: ", acc)
  return(list(aciertos=aciertos, acierto.medio=acc, desviacion=sd(aciertos)))
}

aciertos.knn.test<-sapply(seq(3,55,2), ejecutaKNN, type="test")
plot(seq(3,55,2), aciertos.knn.test[2,], type="b", col="blue", lwd=7, xlab="Valores de K", ylab="Porcentaje de acierto", main = "Acierto vs K para test")
aciertos.knn.test[,9]

aciertos.knn.train<-sapply(seq(3,55,2), ejecutaKNN, type="train")
plot(seq(3,55,2), aciertos.knn.train[2,], type="b", col="blue", lwd=7, xlab="Valores de K", ylab="Porcentaje de acierto", main = "Acierto vs K para train")
aciertos.knn.train[,1]
aciertos.knn.train[,9]

# Utilizamos TSNE para reducir los datos y pintarlos por clases
library(Rtsne)
reduced<-data.frame(Rtsne(dataset_clasificacion[,-1], dims=2))

p1 <- ggplot(as.data.frame(reduced$Y), aes(x=V1, y=V2)) + geom_point(aes(col=factor(dataset_clasificacion$Class))) + theme(axis.title.y = element_blank())
p1

ejecutaKNN(k=19, type="test")
ejecutaKNN(k=19, type="train")

###########################################################################
##                            Apartado 2                                 ##
###########################################################################

# Vamos a comprobar si nuestras variables cumplen la precondición de que son normales
# Sabemos ya por el EDA que no

# Todas las variables.

# Prediciendo sobre el test
mean_score<-0
aciertos.lda.test<-vector(mode = "numeric", length = 10)
for(i in 1:10){
  test<-folds[[i]]$test
  train<-folds[[i]]$train
  model<-lda(Class~.,data=train)
  
  lda.pred <- predict(model,test)
  cat("\nFold ", i, ":")
  print(table(lda.pred$class,test$Class))
  print(mean(lda.pred$class==test$Class))
  aciertos.lda.test[i]<-mean(lda.pred$class==test$Class)
  mean_score<-mean_score+mean(lda.pred$class==test$Class)
}
mean_score<-mean_score/10
cat("\nDesviación típica: ", sd(aciertos.lda.test)*100)
cat("\nScore final: ", mean_score)

# Prediciendo sobre el train
mean_score<-0
aciertos.lda.train<-vector(mode="numeric", length = 10)
for(i in 1:10){
  test<-folds[[i]]$test
  train<-folds[[i]]$train
  model<-lda(Class~.,data=train)
  
  lda.pred <- predict(model,train)
  cat("\nFold ", i, ":")
  print(table(lda.pred$class,train$Class))
  print(mean(lda.pred$class==train$Class))
  aciertos.lda.train[i]<-mean(lda.pred$class==train$Class)
  mean_score<-mean_score+mean(lda.pred$class==train$Class)
}
mean_score<-mean_score/10
cat("\nDesviación típica: ", sd(aciertos.lda.train)*100)
cat("\nScore final: ", mean_score)

###########################################################################
##                            Apartado 3                                 ##
###########################################################################

# Prediciendo sobre el test
mean_score<-0
aciertos.qda.test<-vector(mode="numeric", length = 10)
for(i in 1:10){
  test<-folds[[i]]$test
  train<-folds[[i]]$train
  model<-qda(Class~.,data=train)
  
  lda.pred <- predict(model,test)
  cat("\nFold ", i, ":")
  print(table(lda.pred$class,test$Class))
  print(mean(lda.pred$class==test$Class))
  aciertos.qda.test[i]<-mean(lda.pred$class==test$Class)
  mean_score<-mean_score+mean(lda.pred$class==test$Class)
}
mean_score<-mean_score/10
cat("\nDesviación típica: ", sd(aciertos.qda.test)*100)
cat("\nScore final: ", mean_score)

# Prediciendo sobre el train
mean_score<-0
aciertos.qda.train<-vector(mode="numeric", length = 10)
for(i in 1:10){
  test<-folds[[i]]$test
  train<-folds[[i]]$train
  model<-qda(Class~.,data=train)
  
  lda.pred <- predict(model,train)
  cat("\nFold ", i, ":")
  print(table(lda.pred$class,train$Class))
  print(mean(lda.pred$class==train$Class))
  aciertos.qda.train[i]<-mean(lda.pred$class==train$Class)
  mean_score<-mean_score+mean(lda.pred$class==train$Class)
}
mean_score<-mean_score/10
cat("\nDesviación típica: ", sd(aciertos.qda.train)*100)
cat("\nScore final: ", mean_score)

###########################################################################
##                            Apartado 4                                 ##
###########################################################################

computeWilcoxon<-function(model1, model2){
  # Leemos los resultados de test
  resultadostst <- data.frame(list(knn=aciertos.knn.test[,9]$aciertos, lda=aciertos.lda.test, qda=aciertos.qda.test))
  tablatst <- cbind(resultadostst[,1:dim(resultadostst)[2]])
  colnames(tablatst) <- names(resultadostst)[1:dim(resultadostst)[2]]
  
  # Leemos los resultados de train
  resultadostra <- data.frame(list(knn=aciertos.knn.train[,9]$aciertos, lda=aciertos.lda.train, qda=aciertos.qda.train))
  tablatra <- cbind(resultadostra[,1:dim(resultadostra)[2]])
  colnames(tablatra) <- names(resultadostra)[1:dim(resultadostra)[2]]
  
  # Calculamos las diferencias para test
  difs <- (tablatst[,model1] - tablatst[,model2]) / tablatst[,model1]
  wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
  colnames(wilc_1_2) <- c(colnames(tablatst)[model1], colnames(tablatst)[model2])
  head(wilc_1_2)
  
  # Hacemos el test
  LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
  Rmas.test <- LMvsKNNtst$statistic
  pvalue.test <- LMvsKNNtst$p.value
  LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
  Rmenos.test <- LMvsKNNtst$statistic
  
  # Calculamos las diferencias para train
  difs <- (tablatra[,model1] - tablatra[,model2]) / tablatra[,model1]
  wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
  colnames(wilc_1_2) <- c(colnames(tablatra)[model1], colnames(tablatra)[model2])
  head(wilc_1_2)
  
  # Hacemos el test
  LMvsKNNtra <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
  Rmas.train <- LMvsKNNtra$statistic
  pvalue.train <- LMvsKNNtra$p.value
  LMvsKNNtra <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
  Rmenos.train <- LMvsKNNtra$statistic
  return(list(test=list(Rmas=Rmas.test, Rmenos=Rmenos.test, pvalue=pvalue.test), train=list(Rmas=Rmas.train, Rmenos=Rmenos.train, pvalue=pvalue.train)))
}

computeWilcoxon(1,2)
computeWilcoxon(2,3)
computeWilcoxon(1,3)

# Test de friedman
friedman.test(as.matrix(tablatra))
friedman.test(as.matrix(tablatst))

# Test post-hoc Holm
tam <-dim(tablatra)
groups <-rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatra), groups, p.adjust= "holm", paired = T)

tam <-dim(tablatst)
groups <-rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust= "holm", paired = T)

