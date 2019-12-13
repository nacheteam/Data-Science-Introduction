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