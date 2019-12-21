library(foreign)
library(dplyr)
dataset_regresion<-read.arff("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Regresion/treasury/treasury.dat")
dataset_regresion

colnames(dataset_regresion)
attach(dataset_regresion)

###########################################################################
##                            Apartado 1                                 ##
###########################################################################

# Vamos a hacer un modelo por variable
modelo.1Y_CMaturiyRate<-lm(`1MonthCDRate`~`1Y-CMaturityRate`)
modelo.30Y_CMortgageRate<-lm(`1MonthCDRate`~`30Y-CMortgageRate`)
modelo.3M_Rate_AuctionAverage<-lm(`1MonthCDRate`~`3M-Rate-AuctionAverage`)
modelo.3M_Rate_SecondaryMarket<-lm(`1MonthCDRate`~`3M-Rate-SecondaryMarket`)
modelo.3Y_CMaturityRate<-lm(`1MonthCDRate`~`3Y-CMaturityRate`)
modelo.5Y_CMaturityRate<-lm(`1MonthCDRate`~`5Y-CMaturityRate`)
modelo.bankCredit<-lm(`1MonthCDRate`~bankCredit)
modelo.currency<-lm(`1MonthCDRate`~currency)
modelo.demandDeposits<-lm(`1MonthCDRate`~demandDeposits)
modelo.federalFunds<-lm(`1MonthCDRate`~federalFunds)
modelo.moneyStock<-lm(`1MonthCDRate`~moneyStock)
modelo.checkableDeposits<-lm(`1MonthCDRate`~checkableDeposits)
modelo.loansLeases<-lm(`1MonthCDRate`~loansLeases)
modelo.savingsDeposits<-lm(`1MonthCDRate`~savingsDeposits)
modelo.tradeCurrencies<-lm(`1MonthCDRate`~tradeCurrencies)

# Pintamos y hacemos summary de cada modelo

# Modelo 1 0.2024 de R2 ajustado
plot(`1MonthCDRate`~`1Y-CMaturityRate`, dataset_regresion)
abline(modelo.1Y_CMaturiyRate, col="red")
summary(modelo.1Y_CMaturiyRate)

# Modelo 2 0.9551 de R2 ajustado
plot(`1MonthCDRate`~`30Y-CMortgageRate`, dataset_regresion)
abline(modelo.30Y_CMortgageRate, col="red")
summary(modelo.30Y_CMortgageRate)

# Modelo 3 0.7863 de R2 ajustado
plot(`1MonthCDRate`~`3M-Rate-AuctionAverage`, dataset_regresion)
abline(modelo.3M_Rate_AuctionAverage, col="red")
summary(modelo.3M_Rate_AuctionAverage)

# Modelo 4 0.9807 de R2 ajustado
plot(`1MonthCDRate`~`3M-Rate-SecondaryMarket`, dataset_regresion)
abline(modelo.3M_Rate_SecondaryMarket, col="red")
summary(modelo.3M_Rate_SecondaryMarket)

# Modelo 5 0.9835 de R2 ajustado
plot(`1MonthCDRate`~`3Y-CMaturityRate`, dataset_regresion)
abline(modelo.3Y_CMaturityRate, col="red")
summary(modelo.3Y_CMaturityRate)

# Modelo 6 0.8794 de R2 ajustado
plot(`1MonthCDRate`~`5Y-CMaturityRate`, dataset_regresion)
abline(modelo.5Y_CMaturityRate, col="red")
summary(modelo.5Y_CMaturityRate)

# Modelo 7 0.8291 de R2 ajustado
plot(`1MonthCDRate`~bankCredit, dataset_regresion)
abline(modelo.bankCredit, col="red")
summary(modelo.bankCredit)

# Modelo 8 0.4994 de R2 ajustado
plot(`1MonthCDRate`~currency, dataset_regresion)
abline(modelo.currency, col="red")
summary(modelo.currency)

# Modelo 9 0.4597 de R2 ajustado
plot(`1MonthCDRate`~demandDeposits, dataset_regresion)
abline(modelo.demandDeposits, col="red")
summary(modelo.demandDeposits)

# Modelo 10 0.4926 de R2 ajustado
plot(`1MonthCDRate`~federalFunds, dataset_regresion)
abline(modelo.federalFunds, col="red")
summary(modelo.federalFunds)

# Modelo 11 0.9893 de R2 ajustado
plot(`1MonthCDRate`~moneyStock, dataset_regresion)
abline(modelo.moneyStock, col="red")
summary(modelo.moneyStock)

# Modelo 12 0.6537 de R2 ajustado
plot(`1MonthCDRate`~checkableDeposits, dataset_regresion)
abline(modelo.checkableDeposits, col="red")
summary(modelo.checkableDeposits)

# Modelo 13 0.6854 de R2 ajustado
plot(`1MonthCDRate`~loansLeases, dataset_regresion)
abline(modelo.loansLeases, col="red")
summary(modelo.loansLeases)

# Modelo 14 0.4811 de R2 ajustado
plot(`1MonthCDRate`~savingsDeposits, dataset_regresion)
abline(modelo.savingsDeposits, col="red")
summary(modelo.savingsDeposits)

# Modelo 15 0.6287 de R2 ajustado
plot(`1MonthCDRate`~tradeCurrencies, dataset_regresion)
abline(modelo.tradeCurrencies, col="red")
summary(modelo.tradeCurrencies)

# Tabla de R2 ajustado
# Modelo1: 0.2024
# Modelo2: 0.9551
# Modelo3: 0.7863
# Modelo4: 0.9807
# Modelo5: 0.9835
# Modelo6: 0.8794
# Modelo7: 0.8291
# Modelo8: 0.4994
# Modelo9: 0.4597
# Modelo10: 0.4926
# Modelo11: 0.9893 ~
# Modelo12: 0.6537
# Modelo13: 0.6854
# Modelo14: 0.4811
# Modelo15: 0.6287

# Nos quedamos con los 5 mejores modelos.
modelo.moneyStock
modelo.3Y_CMaturityRate
modelo.3M_Rate_SecondaryMarket
modelo.30Y_CMortgageRate
modelo.5Y_CMaturityRate

###########################################################################
##                            Apartado 2                                 ##
###########################################################################

modelo1<-lm(`1MonthCDRate`~., dataset_regresion)
summary(modelo1)

# Quitamos federalFunds

modelo2<-lm(`1MonthCDRate`~.-federalFunds, dataset_regresion)
summary(modelo2)

# Quitamos 3M-Rate-AuctionAverage
modelo3<-lm(`1MonthCDRate`~.-federalFunds-`3M-Rate-AuctionAverage`, dataset_regresion)
summary(modelo3)

# Quitamos 3M-Rate-SecondaryMarket
modelo4<-lm(`1MonthCDRate`~.-federalFunds-`3M-Rate-AuctionAverage`-`3M-Rate-SecondaryMarket`, dataset_regresion)
summary(modelo4)

# Quitamos 5Y-CMaturiryRate
modelo5<-lm(`1MonthCDRate`~.-federalFunds-`3M-Rate-AuctionAverage`-`3M-Rate-SecondaryMarket`-`5Y-CMaturityRate`, dataset_regresion)
summary(modelo5)

# Vamos a probar a quitar demandDeposits
modelo6<-lm(`1MonthCDRate`~.-federalFunds-`3M-Rate-AuctionAverage`-`3M-Rate-SecondaryMarket`-`5Y-CMaturityRate`-demandDeposits, dataset_regresion)
summary(modelo6)

# Vamos a probar a quitar bankCredit
modelo7<-lm(`1MonthCDRate`~.-federalFunds-`3M-Rate-AuctionAverage`-`3M-Rate-SecondaryMarket`-`5Y-CMaturityRate`-demandDeposits-bankCredit, dataset_regresion)
summary(modelo7)

# Vamos a probar a quitar savingsDeposits
modelo8<-lm(`1MonthCDRate`~.-federalFunds-`3M-Rate-AuctionAverage`-`3M-Rate-SecondaryMarket`-`5Y-CMaturityRate`-demandDeposits-bankCredit-savingsDeposits, dataset_regresion)
summary(modelo8)

# Vamos a probar a quitar currency
modelo9<-lm(`1MonthCDRate`~.-federalFunds-`3M-Rate-AuctionAverage`-`3M-Rate-SecondaryMarket`-`5Y-CMaturityRate`-demandDeposits-bankCredit-savingsDeposits-currency, dataset_regresion)
summary(modelo9)

# Vamos a probar a quitar loansLeases
modelo10<-lm(`1MonthCDRate`~.-federalFunds-`3M-Rate-AuctionAverage`-`3M-Rate-SecondaryMarket`-`5Y-CMaturityRate`-demandDeposits-bankCredit-savingsDeposits-currency-loansLeases, dataset_regresion)
summary(modelo10)

# Vamos a probar a quitar checkableDeposits
modelo11<-lm(`1MonthCDRate`~.-federalFunds-`3M-Rate-AuctionAverage`-`3M-Rate-SecondaryMarket`-`5Y-CMaturityRate`-demandDeposits-bankCredit-savingsDeposits-currency-loansLeases-checkableDeposits, dataset_regresion)
summary(modelo11)

# Vamos a probar a quitar 30Y-CMortgageRate
modelo12<-lm(`1MonthCDRate`~.-federalFunds-`3M-Rate-AuctionAverage`-`3M-Rate-SecondaryMarket`-`5Y-CMaturityRate`-demandDeposits-bankCredit-savingsDeposits-currency-loansLeases-checkableDeposits-`30Y-CMortgageRate`, dataset_regresion)
summary(modelo12)

# Vamos a probar a quitar 1Y-CMaturityRate
modelo13<-lm(`1MonthCDRate`~.-federalFunds-`3M-Rate-AuctionAverage`-`3M-Rate-SecondaryMarket`-`5Y-CMaturityRate`-demandDeposits-bankCredit-savingsDeposits-currency-loansLeases-checkableDeposits-`30Y-CMortgageRate`-`1Y-CMaturityRate`, dataset_regresion)
summary(modelo13)

# Vamos a comprobar qué resultados obtiene el modelo con las variables que decidimos quedarnos en el EDA
var<-c(7,10,11,13,14,15)
names(dataset_regresion)[var]
modelo14<-lm(`1MonthCDRate`~`1Y-CMaturityRate`+`3M-Rate-SecondaryMarket`+`3Y-CMaturityRate`+currency+federalFunds+savingsDeposits+tradeCurrencies, dataset_regresion)
summary(modelo14)

# Quitamos las que tienen poca significancia
modelo15<-lm(`1MonthCDRate`~`1Y-CMaturityRate`+`3M-Rate-SecondaryMarket`+`3Y-CMaturityRate`+currency+savingsDeposits, dataset_regresion)
summary(modelo15)

# Si quisiéramos un modelo más simple podemos quitar 3M-Rate-SecondaryMarket
modelo16<-lm(`1MonthCDRate`~`1Y-CMaturityRate`+`3Y-CMaturityRate`+currency+savingsDeposits, dataset_regresion)
summary(modelo16)

# Finalmente nos hemos quedado con 4.
# El modelo6 es el mejor y el modelo 9 es más simple con resultados muy parecidos. Merece la pena el ultimo.

# Vamos a probar una dependencia cuadrática sobre estas variables aunque el resultado ya es lo suficientemente bueno.
modelo17<-lm(`1MonthCDRate`~`3Y-CMaturityRate`+moneyStock+tradeCurrencies+`3Y-CMaturityRate`*`3Y-CMaturityRate`+moneyStock*moneyStock+tradeCurrencies*tradeCurrencies+`3Y-CMaturityRate`*moneyStock+`3Y-CMaturityRate`*tradeCurrencies+moneyStock*tradeCurrencies, dataset_regresion)
summary(modelo17)
# No hay ninguna mejora

###########################################################################
##                            Apartado 3                                 ##
###########################################################################

# Función para leer los folds
readFolds<-function(){
  fold1.train<-read.arff("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Regresion/treasury/treasury-5-1tra.dat")
  fold1.test<-read.arff("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Regresion/treasury/treasury-5-1tst.dat")
  
  fold2.train<-read.arff("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Regresion/treasury/treasury-5-2tra.dat")
  fold2.test<-read.arff("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Regresion/treasury/treasury-5-2tst.dat")
  
  fold3.train<-read.arff("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Regresion/treasury/treasury-5-3tra.dat")
  fold3.test<-read.arff("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Regresion/treasury/treasury-5-3tst.dat")
  
  fold4.train<-read.arff("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Regresion/treasury/treasury-5-4tra.dat")
  fold4.test<-read.arff("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Regresion/treasury/treasury-5-4tst.dat")
  
  fold5.train<-read.arff("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Regresion/treasury/treasury-5-5tra.dat")
  fold5.test<-read.arff("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Regresion/treasury/treasury-5-5tst.dat")
  
  folds<-list(fold1=list(train=fold1.train, test=fold1.test),
              fold2=list(train=fold2.train, test=fold2.test),
              fold3=list(train=fold3.train, test=fold3.test),
              fold4=list(train=fold4.train, test=fold4.test),
              fold5=list(train=fold5.train, test=fold5.test))
  
  return(folds)
}

folds<-readFolds()

library(kknn)

# Función que ejecuta knn sobre test o train con el valor de k introducido y la fórmula pasada
ejecutaKNN<-function(formula, folds, k, tt="test"){
  errores<-vector(mode = "numeric", length = 5)
  for(i in 1:5){
    if(tt=="test"){
      modelo.knn<-kknn(formula, folds[[i]]$train, folds[[i]]$test, k=k)
      yprime=modelo.knn$fitted.values
      errores[i]<-sum(abs(folds[[i]]$test$`1MonthCDRate`-yprime)^2)/length(yprime)
    }
    else{
      modelo.knn<-kknn(formula, folds[[i]]$train, folds[[i]]$train, k=k)
      yprime=modelo.knn$fitted.values
      errores[i]<-sum(abs(folds[[i]]$train$`1MonthCDRate`-yprime)^2)/length(yprime)
    }
  }
  return(list(errores=errores, media.errores=mean(errores)))
}

# Todas las  variables con k desde 3 hasta 55 a saltos de 2
resultados<-sapply(seq(3,55,2), ejecutaKNN, formula=`1MonthCDRate`~., folds=folds, tt="test")
plot(seq(3,55,2), resultados[2,], type="b", col="blue", lwd=7, xlab="Valores de K", ylab="Error en test", main = "Gráfica de errores por valor de K para test")
resultados[,2]

resultados<-sapply(seq(3,55,2), ejecutaKNN, formula=`1MonthCDRate`~., folds=folds, tt="train")
plot(seq(3,55,2), resultados[2,], type="b", col="blue", lwd=7, xlab="Valores de K", ylab="Error en train", main = "Gráfica de errores por valor de K para train")
resultados[,1]

# Ahora vamos a probar con las variables del mejor modelo de una variable

resultados<-sapply(seq(3,55,2), ejecutaKNN, formula=`1MonthCDRate`~moneyStock, folds=folds, tt="test")
plot(seq(3,55,2), resultados[2,], type="b", col="blue", lwd=7, xlab="Valores de K", ylab="Error en test", main = "Gráfica de errores por valor de K para test")
resultados[,10]

resultados<-sapply(seq(3,55,2), ejecutaKNN, formula=`1MonthCDRate`~moneyStock, folds=folds, tt="train")
plot(seq(3,55,2), resultados[2,], type="b", col="blue", lwd=7, xlab="Valores de K", ylab="Error en train", main = "Gráfica de errores por valor de K para train")
resultados[,1]

# Ahora vamos a probar con el mejor modelo lineal de 3 variables

resultados<-sapply(seq(3,55,2), ejecutaKNN, formula=`1MonthCDRate`~`3Y-CMaturityRate`+moneyStock+tradeCurrencies, folds=folds, tt="test")
plot(seq(3,55,2), resultados[2,], type="b", col="blue", lwd=7, xlab="Valores de K", ylab="Error en test", main = "Gráfica de errores por valor de K para test")
resultados[,4]

resultados<-sapply(seq(3,55,2), ejecutaKNN, formula=`1MonthCDRate`~`3Y-CMaturityRate`+moneyStock+tradeCurrencies, folds=folds, tt="train")
plot(seq(3,55,2), resultados[2,], type="b", col="blue", lwd=7, xlab="Valores de K", ylab="Error en train", main = "Gráfica de errores por valor de K para train")
resultados[,1]

###########################################################################
##                            Apartado 4                                 ##
###########################################################################

# Función que eejecuta LM y obtiene los errores
ejecutaLM<-function(formula, folds, tt="test"){
  errores<-vector(mode = "numeric", length = 5)
  for(i in 1:5){
    if(tt=="test"){
      modelo<-lm(formula, folds[[i]]$train)
      yprime=predict(modelo,folds[[i]]$test)
      errores[i]<-sum(abs(folds[[i]]$test$`1MonthCDRate`-yprime)^2)/length(yprime)
    }
    else{
      modelo<-lm(formula, folds[[i]]$train)
      yprime=predict(modelo,folds[[i]]$train)
      errores[i]<-sum(abs(folds[[i]]$train$`1MonthCDRate`-yprime)^2)/length(yprime)
    }
  }
  return(list(errores=errores, media.errores=mean(errores)))
}

ejecutaLM(`1MonthCDRate`~moneyStock+tradeCurrencies+`3Y-CMaturityRate`, folds, tt="test")
ejecutaLM(`1MonthCDRate`~moneyStock+tradeCurrencies+`3Y-CMaturityRate`, folds, tt="train")

ejecutaKNN(formula = `1MonthCDRate`~. ,folds = folds, k=5, tt="test")
ejecutaKNN(formula = `1MonthCDRate`~. ,folds = folds, k=5, tt="train")

ejecutaLM(`1MonthCDRate`~., folds, tt="test")
ejecutaLM(`1MonthCDRate`~., folds, tt="train")

# Leemos los resultados de test
resultadostst <- read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Tablas de resultados para tests estadisticos/regr_test_alumnos.csv")
tablatst <- cbind(resultadostst[,2:dim(resultadostst)[2]])
colnames(tablatst) <- names(resultadostst)[2:dim(resultadostst)[2]]
rownames(tablatst) <- resultadostst[,1]

# Leemos los resultados de train
resultadostra <- read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Tablas de resultados para tests estadisticos/regr_train_alumnos.csv")
tablatra <- cbind(resultadostra[,2:dim(resultadostra)[2]])
colnames(tablatra) <- names(resultadostra)[2:dim(resultadostra)[2]]
rownames(tablatra) <- resultadostra[,1]

# Calculamos las diferencias para test
difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)

# Hacemos el test
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue

# Calculamos las diferencias para train
difs <- (tablatra[,1] - tablatra[,2]) / tablatra[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatra)[1], colnames(tablatra)[2])
head(wilc_1_2)

# Hacemos el test
LMvsKNNtra <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtra$statistic
pvalue <- LMvsKNNtra$p.value
LMvsKNNtra <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtra$statistic
Rmas
Rmenos
pvalue

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

