havi_atlagok <- function(adat){
  adat_ev <- data.frame()
  adat_ev_atlag <- data.frame()

  for (ev in 1:20)
  {
    for (honap in 1:12){
      sorszam <- 12*(ev-1)+honap
      adat_ev <- subset(adat, adat$Év == ev+1999 & adat$Hónap == honap)
      adat_ev_atlag[sorszam,1] <- paste(ev+1999, honap, "01",  sep = "-")
      adat_ev_atlag[sorszam,2] <- mean(adat_ev$score, na.rm = T)
    }
  }
  colnames(adat_ev_atlag) <- c("Dátum", "Átlag")
  adat_ev_atlag$`Dátum` <- as.Date(adat_ev_atlag$`Dátum`, format("%Y-%m-%d"))
  adat_ev_atlag <- adat_ev_atlag[1:(length(adat_ev_atlag$Átlag)-3),]
  
  return(adat_ev_atlag)
}

evi_atlagok <- function(adat){
  adat_ev <- data.frame()
  adat_ev_atlag <- data.frame()
  
  for (ev in 1:20)
  {
    adat_ev <- subset(adat, adat$Év == ev+1999)
    adat_ev_atlag[ev,1] <- ev+1999
    adat_ev_atlag[ev,2] <- mean(adat_ev$score, na.rm = T)
  }
  colnames(adat_ev_atlag) <- c("Dátum", "Átlag")
return(adat_ev_atlag)
  
  }

