#ez a függvény kiszámolja adott adatok esetén egy phi és theta paraméterű függvény
#AIC-mutatóját, illetve a Ljung-Box-teszt p-értékét
#meg kell adni: adat (idősorként), 
#az ar és ma paraméter pedig a phi és theta legnagyobb értéke (0-nál nem kisebb egész)

arima_modellezes <- function(adat, ar, ma, d){
  #három ideiglenes dataframe létrehozása
  lags <- data.frame() #ez megadja majd phi és theta értékeit
  aic <- data.frame() #ez megadja az AIC-mutatót
  p_value_autokor <- data.frame() #ez megadja a Ljung-Box-teszt p-értékét
  
  for (phi in 0:ar)
  {
    for (theta in 0:ma)
    {
      lags <- rbind(lags, c(phi, theta)) #ez megadja az adott phi és theta értékeket
      modell <- arima(adat, order = c(phi, d, theta)) #arima modell készítése a paraméterekkel
      aic <- rbind(aic, AIC(modell)) #AIC-mutató
      p_value <- Box.test(resid(modell), type = "Ljung") #Ljung-Box-teszt elvégzése (lista formátum)
      p_value_autokor <- rbind(p_value_autokor, as.numeric(p_value[3])) #a lista p_value értéke számként értelmezve
    }
  }
  details <- cbind(lags, aic, p_value_autokor)
  colnames(details) <- c("AR (phi)", "MA (theta)", "AIC", "p-value of Ljung-Box-test")
  return(details)
}
