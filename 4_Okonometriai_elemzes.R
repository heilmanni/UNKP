#ebben a dokumentumban a szentimentelemz�sem id�sorait vizsg�lom

library(tseries)
library(forecast)
library(ModelMetrics)

#ADATBET�LT�S: train �s test set-ek l�trehoz�sa
#havi_index, havi_index_gazd, havi_origo
#Teljes index
havi_index_train <- havi_index[1:200,]
havi_index_test <- havi_index[201:237,]

#Index gazdas�g n�mi korrekci�val
havi_index_gazd <-rbind(havi_index_gazd[1:12,], havi_index_gazd[17:237,])
rownames(havi_index_gazd) <- c(1:233)
havi_gazd_train <- havi_index_gazd[1:200,]
havi_gazd_test <- havi_index_gazd[201:233,]

#Origo
havi_origo_train <- havi_origo[1:200,]
havi_origo_test <- havi_origo[201:237,]


##stacionarit�s �s autokorrel�ci� vizsg�lata
adf.test(havi_index$�tlag) #stacioner
Box.test(havi_index$�tlag, type = "Ljung") #autokorrel�lt

adf.test(havi_index_gazd$�tlag) #stacioner
Box.test(havi_index_gazd$�tlag, type = "Ljung") #autokorrel�lt

adf.test(havi_origo$�tlag) #stacioner
Box.test(havi_origo$�tlag, type = "Ljung") #autokorrel�lt

#1. M�DSZER: legjobb arima modell a teljes id�soron
#korrelogram �s �br�zol�s
par(mfcol=c(2, 3))
acf(havi_index$�tlag, lag.max=10, main = "Index", xlab = "K�sleltet�s")
pacf(havi_index$�tlag, lag.max=10, main = "Index", xlab = "K�sleltet�s", ylab = "PACF")

acf(havi_index_gazd$�tlag, lag.max=10, main = "Index Gazdas�g", xlab = "K�sleltet�s")
pacf(havi_index_gazd$�tlag, lag.max=10, main = "Index Gazdas�g", xlab = "K�sleltet�s", ylab = "PACF")

acf(havi_origo$�tlag, lag.max=10, main = "Origo", xlab = "K�sleltet�s")
pacf(havi_origo$�tlag, lag.max=10, main = "Origo", xlab = "K�sleltet�s", ylab = "PACF")

par(mfrow=c(1,1))

#be kell source-olni az arima_modellezes f�ggv�nyt
##adott maxim�lis AR �s MA k�sleltet�sek melletti eredm�ny
arima_index <- arima_modellezes(adat = havi_index$�tlag, ar = 8, ma = 2, d = 0)
arima_index_gazdasag <- arima_modellezes(adat = havi_index$�tlag, ar = 6, ma = 2, d = 0)
arima_origo <- arima_modellezes(adat = havi_origo$�tlag, ar = 10, ma = 7, d = 0)

#v�lasszuk ki azokat a modelleket, ahol a p �rt�k nagyobb mint 0.05 (nem autokorrel�lt)
arima_index <- arima_index[which(arima_index[,4] > 0.05), ]
arima_index_gazdasag <- arima_index_gazdasag[which(arima_index_gazdasag[,4] > 0.05), ]
arima_origo <- arima_origo[which(arima_origo[,4] > 0.05), ]

#a legjobb modell legyen az, ahol a legkisebb az AIC-mutat�
arima_index[which.min(arima_index[,3]),]
arima_index_gazdasag[which.min(arima_index_gazdasag[,3]),]
arima_origo[which.min(arima_origo[,3]),]

#az egyenletek
best_1_index <- arima(havi_index$�tlag, order = c(1, 0, 1))
best_1_index_gazd <- arima(havi_index_gazd$�tlag, order = c(1, 0, 1))
best_1_origo <- arima(havi_origo$�tlag, order = c(3, 0, 4))



#2. M�DSZER: a legjobb el�rejelz� modell
#be kell source-olni az arima_elorejelzes fuggvenyt
#stacionerek-e?
adf.test(havi_index$�tlag)
adf.test(havi_gazd_train$�tlag)
adf.test(havi_origo$�tlag)

#korrelogramok
par(mfcol=c(2, 3))

acf(havi_index_train$�tlag, lag.max=10, main = "Index", xlab = "K�sleltet�s")
pacf(havi_index_train$�tlag, lag.max=10, main = "Index", xlab = "K�sleltet�s", ylab = "PACF")

acf(havi_gazd_train$�tlag, lag.max=10, main = "Index Gazdas�g", xlab = "K�sleltet�s")
pacf(havi_gazd_train$�tlag, lag.max=10, main = "Index Gazdas�g", xlab = "K�sleltet�s", ylab = "PACF")

acf(havi_origo_train$�tlag, lag.max=10, main = "Origo", xlab = "K�sleltet�s")
pacf(havi_origo_train$�tlag, lag.max=10, main = "Origo", xlab = "K�sleltet�s", ylab = "PACF")

par(mfrow=c(1,1))

#legjobb el�rejelz� modell keres�se
arima_fcast_index <- arima_elorejelzes(training = havi_index_train$�tlag, 
                                       test = havi_index_test$�tlag, ar = 7, ma = 1, d = 0)
arima_fcast_index_gazd <- arima_elorejelzes(training = havi_gazd_train$�tlag, 
                                            test = havi_gazd_test$�tlag, ar = 6, ma = 2, d = 0)
arima_fcast_origo <- arima_elorejelzes(training = havi_origo_train$�tlag, 
                                       test = havi_origo_test$�tlag, ar = 9, ma = 3, d = 0)

#v�lasszuk ki azokat a modelleket, ahol a p �rt�k nagyobb mint 0.05 (nem autokorrel�lt)
arima_fcast_index <- arima_fcast_index[which(arima_fcast_index[,4] > 0.05), ]
arima_fcast_index_gazd <- arima_fcast_index_gazd[which(arima_fcast_index_gazd[,4] > 0.05), ]
arima_fcast_origo <- arima_fcast_origo[which(arima_fcast_origo[,4] > 0.05), ]

#a legjobb modell legyen az, ahol a legkisebb az MSE-mutat�
arima_fcast_index[which.min(arima_fcast_index[,3]),]
arima_fcast_index_gazd[which.min(arima_fcast_index_gazd[,3]),]
arima_fcast_origo[which.min(arima_fcast_origo[,3]),]

#az egyenletek
best_2_index <- arima(havi_index$�tlag, order = c(2, 0, 1))
best_2_index_gazd <- arima(havi_index_gazd$�tlag, order = c(2, 0, 2))
best_2_origo <- arima(havi_origo$�tlag, order = c(2, 0, 0))




#3. �SSZEHASONL�T�S: Diebold-Mariano teszt a k�t k�l�nb�z� elj�r�s eset�n kapott modellek 
#hib�inak k�l�nb�z�s�g�nek becsl�s�re

dm.test(residuals(best_1_index), residuals(best_2_index))
dm.test(residuals(best_1_index_gazd), residuals(best_2_index_gazd))
dm.test(residuals(best_1_origo), residuals(best_2_origo))
dm.test(residuals(best_1_origo), residuals(best_2_origo), alternative = "less")
dm.test(residuals(best_1_origo), residuals(best_2_origo), alternative = "greater")