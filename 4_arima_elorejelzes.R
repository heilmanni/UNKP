#ez a függvény kiszámolja, hogy adott phi (AR) és theta (MA) folyamatok mellett 
#mekkora az elõrejelzés MSE-je
#szükséges hozzá megadni egy training és egy test set-et (idõsorok)
#az ar és ma paraméter pedig a phi és theta legnagyobb értéke (0-nál nem kisebb egész)


arima_elorejelzes <- function(training, test, ar, ma){
  #kell egy üres data.frame, ami majd a végsõ eredményeket tárolja
  info <- data.frame()
  
  for (phi in 0:ar)
  {
    for (theta in 0:ma)
    {
      fcast <- forecast(object = Arima(training, order = c(phi, 0, theta)), h = length(test)) #elõrejelez egy phi és theta rendû arma-modellt a training set alapján, a test set hosszára
      hiba <- mse(actual = test, predicted = fcast$mean) #mse mutató számítása a teszt és elõrejelzett adatok közt
      
      uj <- cbind(phi, theta, hiba) #az adott ciklus új információ
      info <- rbind(info, uj) #összekötés a korábbi információkkal
    }
  }
  return(info)
}