#ez a függvény hibrid modellel készít dinamikus elõrejelzést
#szükséges hozzá megadni egy training idõsort, amibõl elõrejelez
#meg kell adni a training idõsorra legjobban illeszkedõ arma modell ar, ma paramétereit
#meg kell adni a training idõsor arma elõrejelzésének maradékaira épített neurális háló p és size paraméterét
#meg kell adni, hogy hány idõszakra szeretnék dinamukisan elõrejelezni (h)


hibrid_elorejelzes <- function(training, ar, ma, p, size, h){
  #kezdetben nevezzük át a training adatokat tesztnek, késõbb ezt bõvíti majd a függvény
  test <- gazd_training$Átlag
  
  for (i in 1:h){
    arma_modell <- Arima(test, order = c(ar, 0, ma)) #arma modell a megadott paraméterekkel
    arma_elorejelzes <- as.numeric(forecast(arma_modell, h = 1)$mean) #elõrejelzés egy idõszakra
    nn_elorejelzes <- as.numeric(forecast(nnetar(arma_modell$residuals, 
                                                 p = p, size = size, repeats = 10), h = 1)$mean) #neurális háló építése az egy idõszaki elõrejelzés hibatagjára
    y <- arma_elorejelzes + nn_elorejelzes #az elõrejelzés
    test <- c(test, y) #a teszt bõvítése az új elõrejelzéssel - ezért dinamikus
  }
  return(test)
}