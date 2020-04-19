#ez a függvény kiszámolja és tárolja adott késleltetésű Granger-tesztek p-értékét
#y a magyaérzott idősor, x és annak késleltetettjei a magyarázó változók
#mindkét idősornak stacionernek kell lenniük
#a max_lag a maximális késleltetések számát adja meg

granger_test <- function(y, x, max_lag){
  
  p <- data.frame()
  
  for (i in 1:max_lag)
  {
    test <- grangertest(y = y, x = x, order = i)
    szign <- ifelse (test[2,4] < 0.05, "*", "") 
    p <- rbind(p, cbind(i, test[2,4], szign))
  }
  colnames(p) <- c("Késleltetés", "p-érték", "Szignifikancia")
  
  return(p)
}
