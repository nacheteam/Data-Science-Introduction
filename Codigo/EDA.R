###############################################################################################
###############################################################################################
##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  #####
#####                                 DATASET REGRESIÓN                                   #####
##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  #####
###############################################################################################
###############################################################################################

library(foreign)
library(dplyr)
dataset_regresion<-read.arff("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Regresion/treasury/treasury.dat")
dataset_regresion

# Ahora vamos a ver el número de variables y el tipo de cada una.
cat("El número de variables es: ", length(colnames(dataset_regresion)), "\n")
cat("El tipo de las variables es:\n")
for(i in 1:length(colnames(dataset_regresion))){
  cat("\t",colnames(dataset_regresion)[i], ": ", class(dataset_regresion[i][[1]]), "\n")
}

# Como podemos ver todas las variables son de tipo numérico
# Ahora vamos a calcular una serie de estadísticos interesantes para cada variable.
library(e1071)

# Función para calcular la moda
calculaModa <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Función que nos da los estadísticos de una columna del dataset
calculaEstadisticos<-function(col){
  estadisticos<-list(media=NA, mediana=NA, stdv=NA, moda=NA, curtosis=NA, asimetria=NA, minimo=NA, maximo=NA)
  estadisticos$media<-mean(col, na.rm=TRUE)
  estadisticos$mediana<-median(col, na.rm=TRUE)
  estadisticos$stdv<-sd(col, na.rm = TRUE)
  estadisticos$moda<-calculaModa(col)
  estadisticos$kurtosis<-kurtosis(col, na.rm=TRUE)
  estadisticos$asimetria<-skewness(col, na.rm=TRUE)
  estadisticos$minimo<-min(col, na.rm=TRUE)
  estadisticos$maximo<-max(col, na.rm=TRUE)
  return(estadisticos)
}

# Imprimimos bien formateados los estadísticos
cat("\n\n\n", "Estadísticos de las columnas:\n")
for(i in 1:length(colnames(dataset_regresion))){
  cat("\t", colnames(dataset_regresion)[i], ":", "\n")
  estadisticos<-calculaEstadisticos(dataset_regresion[,i])
  cat("\t\t", "Media: ", estadisticos$media, "\n")
  cat("\t\t", "Mediana: ", estadisticos$mediana, "\n")
  cat("\t\t", "Desviación típica: ", estadisticos$stdv, "\n")
  cat("\t\t", "Moda: ", estadisticos$moda, "\n")
  cat("\t\t", "Kurtosis: ", estadisticos$kurtosis, "\n")
  cat("\t\t", "Asimetría: ", estadisticos$asimetria, "\n")
  cat("\t\t", "Mínimo: ", estadisticos$minimo, "\n")
  cat("\t\t", "Máximo: ", estadisticos$maximo, "\n")
}

#######################################################################
##             Estudio de correlación de las variables               ##
#######################################################################

# Función que obtiene las variables cuya correlación supere threshold
obtainCorrelated<-function(varIndex, dataset, threshold=0.9){
  combinations<-combn(varIndex,2)
  
  correlations<-vector("numeric", dim(combinations)[2])
  
  for(i in 1:dim(combinations)[2]){
    pair<-combinations[,i]
    correlations[i]<-cor(dataset[,pair[1]], dataset[,pair[2]], method = c("pearson", "kendall", "spearman"))
  }
  
  for(i in 1:length(correlations)){
    if(abs(correlations[i])>threshold){
      cat("La pareja de variables: ", combinations[,i][1], ", ", combinations[,i][2], " tiene una correlación: ", correlations[i], "\n")
    }
  }
}

obtainCorrelated(1:(length(dataset_regresion)-1), dataset_regresion)

# Vemos que la variable 2 esta altamente correlada con el resto, la quitamos y repetimos
obtainCorrelated(c(1,3:(length(dataset_regresion)-1)), dataset_regresion)

# La variable 9 podemos quitarla tambien
obtainCorrelated(c(1,3:8,10:(length(dataset_regresion)-1)), dataset_regresion)

# La 4 podemos quitarla tambien
obtainCorrelated(c(1,3,5:8,10:(length(dataset_regresion)-1)), dataset_regresion)

# La 3 podemos quitarla tambien
obtainCorrelated(c(1,5:8,10:(length(dataset_regresion)-1)), dataset_regresion)

# La 12 podemos quitarla
obtainCorrelated(c(1,5:8,10:11,13:(length(dataset_regresion)-1)), dataset_regresion)

# La 5 podemos quitarla
obtainCorrelated(c(1,6:8,10:11,13:(length(dataset_regresion)-1)), dataset_regresion)

# La 6 la podemos quitar
obtainCorrelated(c(1,7:8,10:11,13:(length(dataset_regresion)-1)), dataset_regresion)

# La 8 podemos quitarla
obtainCorrelated(c(1,7,10:11,13:(length(dataset_regresion)-1)), dataset_regresion)

# Finalmente nos hemos quedado con las características 1,7,10,11,13,14,15
obtainCorrelated(c(1,7,10:11,13:(length(dataset_regresion)-1)), dataset_regresion, -1)
# Ahora merece la pena que veamos la correlación de las variables con la de salida


# Función que obtiene la correlación de las variables con las de salida
corrSalida<-function(var,dataset){
  for(v in var){
    correlation<-cor(dataset[,v], dataset[,length(dataset)], method=c("pearson", "kendall", "spearman"))
    cat("La correlación de la variable ", v, " con la salida es de: ", correlation, "\n")
  }
}

var<-c(1,7,10,11,13,14,15)
corrSalida(var, dataset_regresion)

# Viendo la salida parece lógico que quitemos la variable 1 porque tiene una correlación baja
var<-c(7,10,11,13,14,15)
corrSalida(var, dataset_regresion)

#######################################################################
##                Valores perdidos (Missing values)                  ##
#######################################################################

# Vamos a ver si tenemos valores perdidos
which(is.na(dataset_regresion))

# No tenemos valores perdidos

#######################################################################
##                           Outliers                                ##
#######################################################################

# Pairplot
pairs(dataset_regresion, pch=16, col="deepskyblue")

var<-c(7,10,11,13,14,15)
pairs(dataset_regresion[,c(var,16)], pch=16, col="deepskyblue")

boxplot(dataset_regresion)
boxplot(dataset_regresion[,c(-8,-9,-10,-14,-12,-15,-13)])
boxplot(dataset_regresion[,c(-1,-8,-9,-10,-12,-15,-13,-14)])
boxplot(dataset_regresion[,var])
boxplot(dataset_regresion[,var][,c(1,3)])
plot(dataset_regresion$bankCredit)

#######################################################################
##                Distribución de las variables                      ##
#######################################################################
library(plyr)
library(psych)
multi.hist(dataset_regresion)

# Test de normalidad para una variable
normalityTest<-function(dataset, var=-1){
  if(var==-1){
    for(i in 1:length(dataset)){
      cat("Test de normalidad para la variable ", i, "\n")
      pvalue<-wilcox.test(dataset[,i])$p.value
      cat("P-valor: ", pvalue, "\n")
      if(pvalue<0.05){
        cat("Como es menor a 0.05 se rechaza la hipótesis nula, no sigue una normal.")
      }
      else{
        cat("Como es mayor a 0.05 no podemos rechazar la hipótesis nula y por tanto no podemos afirmar nada.")
      }
      cat("\n\n")
    }
  }
  else{
    for(i in var){
      cat("Test de normalidad para la variable ", i, "\n")
      pvalue<-wilcox.test(dataset[,i])$p.value
      cat("P-valor: ", pvalue, "\n")
      if(pvalue<0.05){
        cat("Como es menor a 0.05 se rechaza la hipótesis nula, no sigue una normal.")
      }
      else{
        cat("Como es mayor a 0.05 no podemos rechazar la hipótesis nula y por tanto no podemos afirmar nada.")
      }
      cat("\n\n")
    }
  }
}

normalityTest(dataset_regresion)


###############################################################################################
###############################################################################################
##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  #####
#####                               DATASET CLASIFICACIÓN                                 #####
##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  #####
###############################################################################################
###############################################################################################

dataset_clasificacion<-read.csv("/home/nacheteam/MEGA/Master/Introduccion a la ciencia de datos/Trabajo Integrador/DATOS/Datasets Clasificacion/heart/heart.dat", header=FALSE, comment.char = "@")
colnames(dataset_clasificacion)<-c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
dataset_clasificacion

# Ahora vamos a ver el número de variables y el tipo de cada una.
cat("El número de variables es: ", length(colnames(dataset_clasificacion)), "\n")
cat("El tipo de las variables es:\n")
for(i in 1:length(colnames(dataset_clasificacion))){
  cat("\t",colnames(dataset_clasificacion)[i], ": ", class(dataset_clasificacion[i][[1]]), "\n")
}

# Ahora vamos a calcular los estadísticos de las variables

cat("\n\n\n", "Estadísticos de las columnas:\n")
for(i in 1:length(colnames(dataset_clasificacion))){
  cat("\t", colnames(dataset_clasificacion)[i], ":", "\n")
  estadisticos<-calculaEstadisticos(dataset_clasificacion[,i])
  cat("\t\t", "Media: ", estadisticos$media, "\n")
  cat("\t\t", "Mediana: ", estadisticos$mediana, "\n")
  cat("\t\t", "Desviación típica: ", estadisticos$stdv, "\n")
  cat("\t\t", "Moda: ", estadisticos$moda, "\n")
  cat("\t\t", "Kurtosis: ", estadisticos$kurtosis, "\n")
  cat("\t\t", "Asimetría: ", estadisticos$asimetria, "\n")
  cat("\t\t", "Mínimo: ", estadisticos$minimo, "\n")
  cat("\t\t", "Máximo: ", estadisticos$maximo, "\n")
}

#######################################################################
##             Estudio de correlación de las variables               ##
#######################################################################

obtainCorrelated(1:(length(dataset_clasificacion)-1), dataset_clasificacion, threshold = -1)

# Viendo la salida parece lógico que quitemos la variable 1 porque tiene una correlación baja
corrSalida(1:(length(dataset_clasificacion)-1), dataset_clasificacion)

#######################################################################
##                Valores perdidos (Missing values)                  ##
#######################################################################

# Vamos a ver si tenemos valores perdidos
which(is.na(dataset_clasificacion))

# No tenemos valores perdidos

#######################################################################
##                           Outliers                                ##
#######################################################################

pairs(dataset_clasificacion, pch=16, col="deepskyblue")

# Vamos a quitar las variables que no son muy interesantes
var<-c(-2,-3,-6,-7,-9,-11,-12,-13)
pairs(dataset_clasificacion[,var], pch=16, col="deepskyblue")
pairs(dataset_clasificacion[,c(-var, length(dataset_clasificacion))], pch=16, col="deepskyblue")

boxplot(dataset_clasificacion)
boxplot(dataset_clasificacion[,c(-1,-4,-5,-6,-8,-10)])

boxplot(dataset_clasificacion[,1]~dataset_clasificacion[,14], data=dataset_clasificacion)
boxplot(dataset_clasificacion[,2]~dataset_clasificacion[,14], data=dataset_clasificacion)
boxplot(dataset_clasificacion[,3]~dataset_clasificacion[,14], data=dataset_clasificacion)
boxplot(dataset_clasificacion[,4]~dataset_clasificacion[,14], data=dataset_clasificacion)
boxplot(dataset_clasificacion[,5]~dataset_clasificacion[,14], data=dataset_clasificacion)
boxplot(dataset_clasificacion[,6]~dataset_clasificacion[,14], data=dataset_clasificacion)
boxplot(dataset_clasificacion[,7]~dataset_clasificacion[,14], data=dataset_clasificacion)
boxplot(dataset_clasificacion[,8]~dataset_clasificacion[,14], data=dataset_clasificacion)
boxplot(dataset_clasificacion[,9]~dataset_clasificacion[,14], data=dataset_clasificacion)
boxplot(dataset_clasificacion[,10]~dataset_clasificacion[,14], data=dataset_clasificacion)
boxplot(dataset_clasificacion[,11]~dataset_clasificacion[,14], data=dataset_clasificacion)
boxplot(dataset_clasificacion[,12]~dataset_clasificacion[,14], data=dataset_clasificacion)
boxplot(dataset_clasificacion[,13]~dataset_clasificacion[,14], data=dataset_clasificacion)


#######################################################################
##                Distribución de las variables                      ##
#######################################################################
library(plyr)
library(psych)
multi.hist(dataset_clasificacion)

normalityTest(dataset_clasificacion)

