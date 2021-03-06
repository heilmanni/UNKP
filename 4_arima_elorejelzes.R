#ez a f�ggv�ny kisz�molja, hogy adott phi (AR) �s theta (MA) folyamatok mellett 
#mekkora az el�rejelz�s MSE-je
#sz�ks�ges hozz� megadni egy training �s egy test set-et (id�sorok)
#az ar �s ma param�ter pedig a phi �s theta legnagyobb �rt�ke (0-n�l nem kisebb eg�sz)


arima_elorejelzes <- function(training, test, ar, ma, d){
  #kell egy �res data.frame, ami majd a v�gs� eredm�nyeket t�rolja
  info <- data.frame()
  
  for (phi in 0:ar)
  {
    for (theta in 0:ma)
    {
      #a k�vetkez� ciklusban fel�l lesz �rva
      teljes <- training
      for (pred in 1:length(test))
      {
        fcast <- forecast(object = Arima(teljes, order = c(phi, d, theta)), h = 1) #el�rejelez egy phi �s theta rend� arma-modellt a training set alapj�n, egy id�szakra
        teljes <- c(teljes, fcast$mean) #az �j el�rejelz�ssel b�v�ti a traininget, majd erre illeszti a k�vetkez� arima modellt
      }
      p_value <- Box.test(resid(arima(teljes, order = c(phi, d, theta))), type = "Ljung") #Ljung-Box-teszt elv�gz�se (lista form�tum)
      p_value_autokor <- as.numeric(p_value[3]) #a lista p_value �rt�ke sz�mk�nt �rtelmezve
      hiba <- mse(actual = test, predicted = teljes[(length(teljes) - length(test) + 1):length(teljes)])
      info <- rbind(info, cbind(phi, theta, hiba, p_value_autokor))
    }
  }
  return(info)
}