#ebben a dokumentumban a szentimentelemzésem idősorait vizsgálom

library(tseries)
library(forecast)
library(ModelMetrics)

#ADATBETÖLTÉS: train és test set-ek létrehozása
#adatfájlok: havi_index, havi_index_gazd, havi_origo
#Teljes index
havi_index_train <- havi_index[1:200,]
havi_index_test <- havi_index[201:237,]

#Index gazdaság némi korrekcióval
havi_index_gazd <-rbind(havi_index_gazd[1:12,], havi_index_gazd[17:237,])
rownames(havi_index_gazd) <- c(1:233)
havi_gazd_train <- havi_index_gazd[1:200,]
havi_gazd_test <- havi_index_gazd[201:233,]

#Origo
havi_origo_train <- havi_origo[1:200,]
havi_origo_test <- havi_origo[201:237,]


##stacionaritás és autokorreláció vizsgálata
adf.test(havi_index$Átlag) #stacioner
Box.test(havi_index$Átlag, type = "Ljung") #autokorrelált

adf.test(havi_index_gazd$Átlag) #stacioner
Box.test(havi_index_gazd$Átlag, type = "Ljung") #autokorrelált

adf.test(havi_origo$Átlag) #stacioner
Box.test(havi_origo$Átlag, type = "Ljung") #autokorrelált

#1. MÓDSZER: legjobb arima modell a teljes idősoron
#korrelogram és ábrázolás
par(mfcol=c(2, 3))
acf(havi_index$Átlag, lag.max=10, main = "Index", xlab = "Késleltetés")
pacf(havi_index$Átlag, lag.max=10, main = "Index", xlab = "Késleltetés", ylab = "PACF")

acf(havi_index_gazd$Átlag, lag.max=10, main = "Index Gazdaság", xlab = "Késleltetés")
pacf(havi_index_gazd$Átlag, lag.max=10, main = "Index Gazdaság", xlab = "Késleltetés", ylab = "PACF")

acf(havi_origo$Átlag, lag.max=10, main = "Origo", xlab = "Késleltetés")
pacf(havi_origo$Átlag, lag.max=10, main = "Origo", xlab = "Késleltetés", ylab = "PACF")

par(mfrow=c(1,1))

#be kell source-olni az arima_modellezes függvényt
source(file = "https://raw.githubusercontent.com/heilmanni/UNKP/master/4_arima_modellezes.R")
##adott maximális AR és MA késleltetések melletti eredmény
arima_index <- arima_modellezes(adat = havi_index$Átlag, ar = 8, ma = 2, d = 0)
arima_index_gazdasag <- arima_modellezes(adat = havi_index$Átlag, ar = 6, ma = 2, d = 0)
arima_origo <- arima_modellezes(adat = havi_origo$Átlag, ar = 10, ma = 7, d = 0)

#válasszuk ki azokat a modelleket, ahol a p érték nagyobb mint 0.05 (nem autokorrelált)
arima_index <- arima_index[which(arima_index[,4] > 0.05), ]
arima_index_gazdasag <- arima_index_gazdasag[which(arima_index_gazdasag[,4] > 0.05), ]
arima_origo <- arima_origo[which(arima_origo[,4] > 0.05), ]

#a legjobb modell legyen az, ahol a legkisebb az AIC-mutató
arima_index[which.min(arima_index[,3]),]
arima_index_gazdasag[which.min(arima_index_gazdasag[,3]),]
arima_origo[which.min(arima_origo[,3]),]

#az egyenletek
best_1_index <- arima(havi_index$Átlag, order = c(1, 0, 1))
best_1_index_gazd <- arima(havi_index_gazd$Átlag, order = c(1, 0, 1))
best_1_origo <- arima(havi_origo$Átlag, order = c(3, 0, 4))



#2. MÓDSZER: a legjobb előrejelző modell
#be kell source-olni az arima_elorejelzes fuggvenyt
source(file = "https://raw.githubusercontent.com/heilmanni/UNKP/master/4_arima_elorejelzes.R")
#stacionerek-e?
adf.test(havi_index$Átlag)
adf.test(havi_gazd_train$Átlag)
adf.test(havi_origo$Átlag)

#korrelogramok
par(mfcol=c(2, 3))

acf(havi_index_train$Átlag, lag.max=10, main = "Index", xlab = "Késleltetés")
pacf(havi_index_train$Átlag, lag.max=10, main = "Index", xlab = "Késleltetés", ylab = "PACF")

acf(havi_gazd_train$Átlag, lag.max=10, main = "Index Gazdaság", xlab = "Késleltetés")
pacf(havi_gazd_train$Átlag, lag.max=10, main = "Index Gazdaság", xlab = "Késleltetés", ylab = "PACF")

acf(havi_origo_train$Átlag, lag.max=10, main = "Origo", xlab = "Késleltetés")
pacf(havi_origo_train$Átlag, lag.max=10, main = "Origo", xlab = "Késleltetés", ylab = "PACF")

par(mfrow=c(1,1))

#legjobb előrejelző modell keresése
arima_fcast_index <- arima_elorejelzes(training = havi_index_train$Átlag, 
                                       test = havi_index_test$Átlag, ar = 7, ma = 1, d = 0)
arima_fcast_index_gazd <- arima_elorejelzes(training = havi_gazd_train$Átlag, 
                                            test = havi_gazd_test$Átlag, ar = 6, ma = 2, d = 0)
arima_fcast_origo <- arima_elorejelzes(training = havi_origo_train$Átlag, 
                                       test = havi_origo_test$Átlag, ar = 9, ma = 3, d = 0)

#válasszuk ki azokat a modelleket, ahol a p érték nagyobb mint 0.05 (nem autokorrelált)
arima_fcast_index <- arima_fcast_index[which(arima_fcast_index[,4] > 0.05), ]
arima_fcast_index_gazd <- arima_fcast_index_gazd[which(arima_fcast_index_gazd[,4] > 0.05), ]
arima_fcast_origo <- arima_fcast_origo[which(arima_fcast_origo[,4] > 0.05), ]

#a legjobb modell legyen az, ahol a legkisebb az MSE-mutató
arima_fcast_index[which.min(arima_fcast_index[,3]),]
arima_fcast_index_gazd[which.min(arima_fcast_index_gazd[,3]),]
arima_fcast_origo[which.min(arima_fcast_origo[,3]),]

#az egyenletek
best_2_index <- arima(havi_index$Átlag, order = c(2, 0, 1))
best_2_index_gazd <- arima(havi_index_gazd$Átlag, order = c(2, 0, 2))
best_2_origo <- arima(havi_origo$Átlag, order = c(2, 0, 0))


#3. ÖSSZEHASONLÍTÁS: Diebold-Mariano teszt a két különböző eljárás esetén kapott modellek 
#hibáinak különbözőségének becslésére

dm.test(residuals(best_1_index), residuals(best_2_index))
dm.test(residuals(best_1_index_gazd), residuals(best_2_index_gazd))
dm.test(residuals(best_1_origo), residuals(best_2_origo))
dm.test(residuals(best_1_origo), residuals(best_2_origo), alternative = "less")
dm.test(residuals(best_1_origo), residuals(best_2_origo), alternative = "greater")
