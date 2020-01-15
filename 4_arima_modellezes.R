#ez a f�ggv�ny kisz�molja adott adatok eset�n egy phi �s theta param�ter� f�ggv�ny
#AIC-mutat�j�t, illetve a Ljung-Box-teszt p-�rt�k�t
#meg kell adni: adat (id�sork�nt), 
#az ar �s ma param�ter pedig a phi �s theta legnagyobb �rt�ke (0-n�l nem kisebb eg�sz)

arima_modellezes <- function(adat, ar, ma){
  #h�rom ideiglenes dataframe l�trehoz�sa
  lags <- data.frame() #ez megadja majd phi �s theta �rt�keit
  aic <- data.frame() #ez megadja az AIC-mutat�t
  p_value_autokor <- data.frame() #ez megadja a Ljung-Box-teszt p-�rt�k�t
  
  for (phi in 0:ar)
  {
    for (theta in 0:ma)
    {
      lags <- rbind(lags, c(phi, theta)) #ez megadja az adott phi �s theta �rt�keket
      modell <- arima(adat, order = c(phi, 0, theta)) #arima modell k�sz�t�se a param�terekkel
      aic <- rbind(aic, AIC(modell)) #AIC-mutat�
      p_value <- Box.test(resid(modell), type = "Ljung") #Ljung-Box-teszt elv�gz�se (lista form�tum)
      p_value_autokor <- rbind(p_value_autokor, as.numeric(p_value[3])) #a lista p_value �rt�ke sz�mk�nt �rtelmezve
    }
  }
  details <- cbind(lags, aic, p_value_autokor)
  colnames(details) <- c("AR (phi)", "MA (theta)", "AIC", "p-value of Ljung-Box-test")
  return(details)
}