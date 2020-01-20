#Ez a függvény visszaadja a havi, illetve évi szentimentátlagokat
#az imputnak olyan fájlnak kell lennie, mely tartalmaz "ÉV", a havi_atlagok esetén "Hónap", illetve "score" oszlopokat
#mivel speciálisan 2000. 01. 01. és 2019. 09. 20. közt vizsgálom az elemzést, ezért most a kezdeti és végdátumot nem lehet változtatni

havi_atlagok <- function(adat){
  adat_ev <- data.frame()
  adat_ev_atlag <- data.frame()

  for (ev in 1:20)
  {
    for (honap in 1:12){
      sorszam <- 12*(ev-1)+honap #hányadik hónapnál vagyunk 2000 januárja óta
      adat_ev <- subset(adat, adat$Év == ev+1999 & adat$Hónap == honap) #adott év adott hónapja - részhalmaz
      adat_ev_atlag[sorszam,1] <- paste(ev+1999, honap, "01",  sep = "-") #adat_ev_atlag első oszlopa az yyyy-mm-01 formátumú
      adat_ev_atlag[sorszam,2] <- mean(adat_ev$score, na.rm = T) #adat_ev_atlag második oszlopa a részhalmaz score oszlopának átlaga
    }
  }
  colnames(adat_ev_atlag) <- c("Dátum", "Átlag") #oszlopok elnevezése
  adat_ev_atlag$`Dátum` <- as.Date(adat_ev_atlag$`Dátum`, format("%Y-%m-%d")) #a Dátum dátumként értelmezése
  adat_ev_atlag <- adat_ev_atlag[1:(length(adat_ev_atlag$Átlag)-3),] #mivel csak 2019. szeptemberig nézzük, ezért az utolsó három hónap NA, azt kivesszük
  
  return(adat_ev_atlag)
}

#Ennél csak "Év" oszlopra van szükség az input fájl esetén
evi_atlagok <- function(adat){
  adat_ev <- data.frame()
  adat_ev_atlag <- data.frame()
  
  for (ev in 1:20)
  {
    adat_ev <- subset(adat, adat$Év == ev+1999) #adat_ev részhalmaz létrehozása
    adat_ev_atlag[ev,1] <- ev+1999 #az adat_ev_atlag első oszlopa az év
    adat_ev_atlag[ev,2] <- mean(adat_ev$score, na.rm = T) #az adat_ev_atlag második oszlopa a részhalmaz score értékeinek átlaga
  }
  colnames(adat_ev_atlag) <- c("Dátum", "Átlag") #oszlopok elnevezése
return(adat_ev_atlag)
  
  }
