#közös ábrán plottolni a gazdasági és és hangulat idõsorokat
library(tseries)
library(forecast)
library(ModelMetrics)

#az index idõsora fehér zaj-e?
#adatok: havi_index és havi_index_gazd
#0. lépés: havi_index_gazd hiányzó adatainak levágása, illetve egy kiugró hónap kivétele
havi_index_gazd2 <-rbind(havi_index_gazd[1:12,], havi_index_gazd[17:237,])
rownames(havi_index_gazd2) <- c(1:233)

#1. lépés: stacionaritás vizsgálat (ADF-teszt)
adf.test(havi_index$Átlag)
#stacioner, p=0.04
adf.test(havi_index_gazd2$Átlag)
#stacioner, p=0.01

#2.lépés: tartalmaz-e autokorrelációt? (Ljung-Box-teszt)
Box.test(havi_index$Átlag, type = "Ljung")
Box.test(havi_index$Átlag, type = "Ljung", lag = 12)
#autokorrelált, p közel nulla
Box.test(havi_index_gazd2$Átlag, type = "Ljung")
Box.test(havi_index_gazd2$Átlag, type = "Ljung", lag = 12)
#szintén erõsen autokorrelált, p közel nulla

#azaz a folyamat ugyan stacioner, de erõs autokorreláció van benne

#TRAINING ÉS TEST SET LÉTREHOZÁSA

#a training set legyen 200 hosszú, a test set 33 (14%)
gazd_training <- havi_index_gazd2[1:200,]
gazd_training$Dátum <- c(1:nrow(gazd_training))
gazd_test <- havi_index_gazd2[201:233,]
gazd_test$Dátum <- c(1:nrow(gazd_test))

plot(y = gazd_training$Átlag, x = gazd_training$Dátum, type = 'l')


#ARMA MODELLEZÉS

#1. MÓDSZER: AIC MUTATÓ A TELJES HALMAZRA

#nézzünk meg egy korrelogramot
acf(havi_index_gazd2$Átlag, lag.max=20)
#ez leginkább lecseng, a 8. késleltetés után lesz 0.05-nél kisebb
pacf(havi_index_gazd2$Átlag, lag.max=20)
#leginkább 3 után törik le, de akár le is csenghet

#be kell source-olni az arima_modellezes függvényt
eredmeny <- arima_modellezes(adat = havi_index_gazd2$Átlag, ar = 8, ma = 3)

#válasszuk ki azokat a modelleket, ahol a p érték nagyobb mint 0.05 (nem autokorrelált)
eredmeny <- eredmeny[which(eredmeny[,4] > 0.05), ]
#azaz a 0,0 modellen kívül mindegyikbõl kiszûrtük az autokorrelációt

#a legjobb modell legyen az, ahol a legkisebb az AIC-mutató
eredmeny[which.min(eredmeny[,3]),]
#eszerint a legjobb modell az ARMA(6, 2), ami meglehetõsen komplex
#érdemes lehet egyszerûbbet választani, fõleg azt figyelembe, hogy nem túl hosszú az idõsor
modell <- arima(havi_index_gazd2$Átlag, order = c(6, 0, 2))
summary(modell)

#érdemes a teljes index adathalmazra megnézni
eredmeny2 <- arima_modellezes(havi_index$Átlag, ar = 6, ma = 6)
eredmeny2 <- eredmeny2[which(eredmeny2[,4] > 0.05), ]
eredmeny2[which.min(eredmeny2[,3]),]
#itt a legjobb egy ARMA(1,1) modell, ami kellõen egyszerû is




#2. MÓDSZER: TRAINING ÉS TESZT SET, MSE MUTATÓ ALAPJÁN

#most csak a training setre nézzük meg az arima-modellezést
arima_2_modszer <- arima_modellezes(adat = gazd_training$Átlag, ar = 8, ma = 4)
arima_2_modszer <- arima_2_modszer[which(arima_2_modszer[,4] > 0.05), ]

#a legjobb modell legyen az, ahol a legkisebb az AIC-mutató
arima_2_modszer[which.min(arima_2_modszer[,3]),]
#eszerint most az ARMA (1,1 a legjobb)

fit2 <- Arima(gazd_training$Átlag, order = c(2, 0, 5))
fit2 <- forecast(object = Arima(gazd_training$Átlag, order = c(1, 0, 1)))
fit2$mean
length(fit2$mean)

fit2 <- arima_elorejelzes(training = gazd_training$Átlag, test = gazd_test$Átlag, ar = 8, ma = 8)
fit2[which.min(fit2[,3]), ]
# eszerint ARMA (2, 5) a legjobb, MSE=0.002511944
fit_arima <- Arima(gazd_training$Átlag, order = c(2, 0, 5))
fit_arima$coef #paraméterek értékei

#egy kis ábrázolás
fcast <- forecast(object = Arima(gazd_training$Átlag, order = c(2, 0, 5)), h = 33)
autoplot(fcast) #csak az elõrejelzés plotja
mse(actual = gazd_test$Átlag, predicted = fcast$mean) #még egyszer az MSE-mutató

#közös ábrán ábrozolva
xaxis = c(201:233) #tengely
plot(xaxis, gazd_test$Átlag, type="l", col="black") #tényleges adatok
lines(xaxis, fcast$mean, col="blue") #elõrejelzés NN-nel



#NEURÁLIS HÁLÓ ÉPÍTÉSE A KÖVETKEZÕ SZAKASZBAN
library(neuralnet)

#függvénnyel legjobb neurális háló keresése
best_modell <- neur_halo(training = gazd_training$Átlag, test = gazd_test$Átlag,
                         dimenzio = 6, neuron = 20)
best_modell[which.min(best_modell[,3]), ]
#eszerint az optimális háló esetén a p = 5, size = 16, MSE = 0.002141319
# késõbbi ellenõrzõ funkció beépítéséhez: mosaic::is.wholenumber(-2)

fit <- nnetar(gazd_training$Átlag, p = 5, size = 20, repeats = 10) #legjobb háló
autoplot(forecast(fit, h=33)) #plot a teljes idõsorra
fcast <- forecast(fit, PI=TRUE, h=33) #elõrejelzés konfidenciaintervallumokkal
autoplot(fcast) #csak az elõrejelzés plotja
mse(actual = gazd_test$Átlag, predicted = fcast$mean) #még egyszer az MSE-mutató

#közös ábrán ábrozolva
xaxis = c(201:233) #tengely
plot(xaxis, gazd_test$Átlag, type="l", col="black") #tényleges adatok
lines(xaxis, fcast$mean, col="blue") #elõrejelzés NN-nel

#különbségek:
dif <- gazd_test$Átlag - fcast$mean
plot(xaxis, dif, type = "l")
lines(xaxis, y = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )
#Box.test(dif, type = c("Ljung"))



#HYBRID MODELL ÉPÍTÉSE A KÖVETKEZÕ SZAKASZBAN
#alapja, hogy az arma-modell hibatagjára építek egy neurális hálót

best_arma <- Arima(gazd_training$Átlag, order = c(2, 0, 5))
hibak_training <- best_arma$residuals

#ezekre a hibatagokra kell egy neurális hálót építeni
best_modell_hiba <- neur_halo(training = hibak_training, test = hibak_test,
                         dimenzio = 6, neuron = 20)
best_modell_hiba[which.min(best_modell_hiba[,3]), ]
#eszerint a legjobb modell esetén p = 4, size = 8, MSE = 0.00136769

#a hibrid modell egy arma és egy nn összege
#minden idõszakban elõrejelzek egy idõpontra, majd utána az addigiak alapján egy újabbra
hibrid <- hibrid_elorejelzes(training = gazd_training$Átlag, ar = 2, ma = 5, p = 4, size = 8, h = 33)

plot(hibrid, type = "l")
mse(actual = gazd_test$Átlag, predicted = hibrid[201:233])

#közös ábrán ábrozolva
xaxis = c(201:233) #tengely
plot(xaxis, gazd_test$Átlag, type="l", col="black") #tényleges adatok
lines(xaxis, hibrid[201:233], col="blue") #elõrejelzés NN-nel

#különbségek:
dif <- gazd_test$Átlag - hibrid[201:233]
plot(xaxis, dif, type = "l")
lines(xaxis, y = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )
#Box.test(dif, type = c("Ljung"))


