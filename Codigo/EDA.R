# En primer lugar vamos a visualizar el dataset de regresión.

library(foreign)
dataset_regresion<-read.arff("../..//DATOS/Datasets Regresion/treasury/treasury.dat")
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
calculaModa <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
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

