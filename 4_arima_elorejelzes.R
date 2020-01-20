#ez a függvény kiszámolja, hogy adott phi (AR) és theta (MA) folyamatok mellett 
#mekkora az elõrejelzés MSE-je
#szükséges hozzá megadni egy training és egy test set-et (idõsorok)
#az ar és ma paraméter pedig a phi és theta legnagyobb értéke (0-nál nem kisebb egész)


arima_elorejelzes <- function(training, test, ar, ma, d){
  #kell egy üres data.frame, ami majd a végsõ eredményeket tárolja
  info <- data.frame()
  
  for (phi in 0:ar)
  {
    for (theta in 0:ma)
    {
      #a következõ ciklusban felül lesz írva
      teljes <- training
      for (pred in 1:length(test))
      {
        fcast <- forecast(object = Arima(teljes, order = c(phi, d, theta)), h = 1) #elõrejelez egy phi és theta rendû arma-modellt a training set alapján, egy idõszakra
        teljes <- c(teljes, fcast$mean) #az új elõrejelzéssel bõvíti a traininget, majd erre illeszti a következõ arima modellt
      }
      p_value <- Box.test(resid(arima(teljes, order = c(phi, d, theta))), type = "Ljung") #Ljung-Box-teszt elvégzése (lista formátum)
      p_value_autokor <- as.numeric(p_value[3]) #a lista p_value értéke számként értelmezve
      hiba <- mse(actual = test, predicted = teljes[(length(teljes) - length(test) + 1):length(teljes)])
      info <- rbind(info, cbind(phi, theta, hiba, p_value_autokor))
    }
  }
  return(info)
}