#ez a függvény beállítja a neurális háló optimális beágyazott dimenzióinak számát, illetve
#neuronszámot
#szükséges hozzá megadni egy training és egy test set-et (idõsorok)
#a dimenzio és a neuron a maximális száma a dimenziónak és neuronnak (0-nál nagyobb egész)
#a rétegek száma nem állítható

neur_halo <- function(training, test, dimenzio, neuron){
  #ideiglenes data.frame létrehozása
  fit <- data.frame()
  
  for (p in 1:dimenzio)
  {
    for (size in 1:neuron)
    {
      modell <- nnetar(training, p = p, size = size, repeats = 10) #adott dimenziójú, adott méretû neurális háló, 10-es ismétléssel
      fcast <- forecast(modell, h = length(test)) #a teszthalmaz teljes idõszakára elõrejelez
      hiba <- mse(actual = test, predicted = fcast$mean) #mse mutató számítása a teszt és elõrejelzett adatok közt
      
      details <- cbind(p, size, hiba)
      fit <- rbind(fit, details)
      
    }
  }
  return(fit)
}