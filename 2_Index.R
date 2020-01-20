#Ez a dokumentum letölti az Index adott tematikájú cikkeit adott kezdõ és végdátummal

#webscrapinghez kell egy package

library("rvest")

#ezután meg kell adni dátumként a kezdõ- és záróidõpontot
start <- as.Date("00-01-01", format="%y-%m-%d")
end <- as.Date("00-12-31", format="%y-%m-%d")

#létre kell hozni az adatfájl
cikkcimek_index_2000_gazdasag <- data.frame()
#theDate jelöli a futó dátumot, ami alapján leszedi majd az adott nap cikkeit a webscraping
#elõször a kezdõnap legyen, majd folyamatosan nõ eggyel
theDate <- start

#a következõ szakaszban letölti az adott nap cikkeit egy while ciklussal
#elõször megírja a linket, amihez kell a theDate változóként
#ezután beolvassa az oldalt: belfold, kulfold, gazdasag témakörökben, ezt a linken belül kell átírni
#a .cim a beolvassa az adott nap cikkeinek címe
#ezt szövegesíti
#végül megvizsgálja, hogy írodott-e cikk azon a napon: ha nem, a dataframe nem változik
#ha igen, akkor hozzáfûzi a dátummal együtt
#végül az index weboldalának sajátossága miatt be kell lassítani a scrapinget, mivel
#egy idõ után letilt, ha túl sok cikket tölt be, és kézzel újra kell indítani a folyamatot
#a lassítást 0.5 és 2 másodperc között érdemes beállítani


while (theDate <= end) 
{
  link <- paste("https://index.hu/24ora/?s=&tol=", theDate, "&ig=", 
                theDate, "&profil=&rovat=kulfold&cimke=&word=1&pepe=1", sep = "")
  oldal <- read_html(link)
  cim <- html_nodes(oldal, ".cim a")
  cim <- html_text(cim)
  
  ifelse(length(cim)==0,
         cikkcimek_index_2000_gazdasag <- cikkcimek_index_2000_gazdasag,
         cikkcimek_index_2000_gazdasag<- rbind(cikkcimek_index_2000_gazdasag, data.frame(theDate, cim)))
  
  
  Sys.sleep(1)
  theDate <- theDate+1
  
}

save(cikkcimek_index_2000_gazdasag, file =  "Index_cimek_2000_gazdasag.RData")

