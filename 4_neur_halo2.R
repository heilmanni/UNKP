#ez a függvény kiszámítja egy training idõsorra optimalizált idõsor MSE-jét egy test idõsoron
#két idõsor szükséges hozzá: training és test
#13 neurális hálót próbál ki, 1, 2 vagy 3 rejtett réteggel, maximum 5 neuronnal rétegenként
#a reps megadja, hogy hányszor futasson egy adott modellt újra - idõ vagy pontosabb eredmény nyerhetõ vele
#szükséges csomag: library(nnfor)

neur_halo2 <- function(training, test, reps){
  #az adatok biztosan idõsorok legyenek
  training <- ts(training)
  test <- ts(test)
  
  #létre kell hozni egy listát, amely tartalmazza a lehetséges rétegek és bennük lévõ neuronok számát
  hd1_1 <- 2
  hd1_2 <- 3
  hd1_3 <- 4
  hd1_4 <- 5
  hd2_1 <- c(2, 2)
  hd2_2 <- c(3, 3)
  hd2_3 <- c(4, 4)
  hd3_1 <- c(2, 2, 2)
  hd3_2 <- c(2, 3, 2)
  hd3_3 <- c(2, 4, 2)
  hd3_4 <- c(3, 2, 3)
  hd3_5 <- c(3, 3, 3)
  hd3_6 <- c(3, 4, 3)
  
  hd <- list(hd1_1, hd1_2, hd1_3, hd1_4, hd2_1, hd2_2, hd2_3, hd3_1, hd3_2, hd3_3, hd3_4, hd3_5, hd3_6)
  
  #két dataframe létrehozása, melyet feltölt majd adatokkal
  nn_info <- data.frame()
  nn_sum <- data.frame()
  
  for (i in 1:13)
  {
    nn <- elm(y = training, hd = hd[[i]], reps = reps) #nn optimalizálás, hd a 13 modellen fut végig
    nn_fcast <- forecast(object = nn, h = nrow(test)) #elõrejelzése a legjobb nn alapján a test teljes hosszára
    hiba <- ModelMetrics::mse(actual = test, predicted = nn_fcast$mean) #mse mutató a test tényleges és becsült értékeire
    nn_sum <- cbind(i, nn$MSE) #új információk
    nn_info <- rbind(nn_info, nn_sum) #összekötés az eddigi információkkal
    print(i) #a türelmetlen programozót segíti, mutatja, hogy hányadik modellnél jár a függvény
  }
  colnames(nn_info) <- c("hd[[]]", "MSE")
  return(nn_info)
  return(hd)
}