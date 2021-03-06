#Ez a dokumentum let�lti az Origo adott tematik�j� cikkeit adott kezd� �s v�gd�tummal

#Origo: 2000. 01. 01. �s 2019. 09. 20. k�z�tti cikkc�mek
start <- as.Date("00-01-01", format="%y-%m-%d")
end <- as.Date("19-09-20", format="%y-%m-%d")

#webscraping package
library("rvest")

#adatb�zis l�trehoz�sa
cikkcimek <- data.frame()
#a theDate lesz a fut� index, el�sz�r a kezd�i�ponttal indul
theDate <- start

#while ciklus
#theDate-t sz�t kell szedni �vre, h�napra, napra, �s �gy adja majd ki a linket
#ut�na be kell t�lteni a linket
#leszedni a c�met
#ezt sz�vegg� alak�tani
#v�g�l �v-h�nap-nap �s c�m �sszek�t�se a m�r megl�v� adatb�zissal
#sajnos kateg�ri�nk�nt nem lehet v�logatni, csak egy adott napi cikket

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
