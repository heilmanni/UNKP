#Ez a dokumentum letölti az Origo adott tematikájú cikkeit adott kezdõ és végdátummal

#Origo: 2000. 01. 01. és 2019. 09. 20. közötti cikkcímek
start <- as.Date("00-01-01", format="%y-%m-%d")
end <- as.Date("19-09-20", format="%y-%m-%d")

#webscraping package
library("rvest")

#adatbázis létrehozása
cikkcimek <- data.frame()
#a theDate lesz a futó index, elõször a kezdõiõponttal indul
theDate <- start

#while ciklus
#theDate-t szét kell szedni évre, hónapra, napra, és így adja majd ki a linket
#utána be kell tölteni a linket
#leszedni a címet
#ezt szöveggé alakítani
#végül év-hónap-nap és cím összekötése a már meglévõ adatbázissal
#sajnos kategóriánként nem lehet válogatni, csak egy adott napi cikket

while (theDate <= end) 
{
  theDate_ev <- substr(theDate, 1, 4)
  theDate_honap <- substr(theDate, 6, 7)
  theDate_nap <- substr(theDate, 9, 10)
  link <- paste("https://www.origo.hu/hir-archivum/", theDate_ev, "/",
                      theDate_ev, theDate_honap, theDate_nap,
                      ".html", sep = "")
  oldal <- read_html(link)
  cim <- html_nodes(oldal, "#archive-articles a")
  cim <- html_text(cim)
  
  cikkcimek <- rbind(cikkcimek, data.frame(theDate_ev, theDate_honap, theDate_nap, cim))
  
  theDate <- theDate+1
}

#lementeni
save(cikkcimek, file =  "Origo_cimek.RData")
