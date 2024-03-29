#Ez a dokumentum let�lti az Index adott tematik�j� cikkeit adott kezd� �s v�gd�tummal

#webscrapinghez kell egy package

library("rvest")

#ezut�n meg kell adni d�tumk�nt a kezd�- �s z�r�id�pontot
start <- as.Date("00-01-01", format="%y-%m-%d")
end <- as.Date("00-12-31", format="%y-%m-%d")

#l�tre kell hozni az adatf�jl
cikkcimek_index_2000_gazdasag <- data.frame()
#theDate jel�li a fut� d�tumot, ami alapj�n leszedi majd az adott nap cikkeit a webscraping
#el�sz�r a kezd�nap legyen, majd folyamatosan n� eggyel
theDate <- start

#a k�vetkez� szakaszban let�lti az adott nap cikkeit egy while ciklussal
#el�sz�r meg�rja a linket, amihez kell a theDate v�ltoz�k�nt
#ezut�n beolvassa az oldalt: belfold, kulfold, gazdasag t�mak�r�kben, ezt a linken bel�l kell �t�rni
#a .cim a beolvassa az adott nap cikkeinek c�me
#ezt sz�veges�ti
#v�g�l megvizsg�lja, hogy �rodott-e cikk azon a napon: ha nem, a dataframe nem v�ltozik
#ha igen, akkor hozz�f�zi a d�tummal egy�tt
#v�g�l az index weboldal�nak saj�toss�ga miatt be kell lass�tani a scrapinget, mivel
#egy id� ut�n letilt, ha t�l sok cikket t�lt be, �s k�zzel �jra kell ind�tani a folyamatot
#a lass�t�st 0.5 �s 2 m�sodperc k�z�tt �rdemes be�ll�tani


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

