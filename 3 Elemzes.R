library(tm)
library(dplyr)
library(SnowballC)
library(wordcloud)
library(tidytext)
library(textdata)
library(stringr)
library(tidyverse)
library(dygraphs)

#Creating a sentiment dictinary from a two text files - just importing
Negative$point <- -1 #negatívhoz -1 rendelve
Positive$point <- 1 #pozitivhoz +1 rendelve
sentimentdic <- rbind(Negative, Positive) #egybekötve
colnames(sentimentdic) <- c("word", "point") #oszlopok elnevezve

#creating an extra row - later itt will be useful
sentimentdic[7689,] <- c("", "hiba")

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#adatok beimportálása, tisztítása
origo <- cikkcimek
origo$cim <- as.character(origo$cim)
origo$theDate_ev <- as.numeric(origo$theDate_ev)+ 1999 #sajnos valamiért 1-nél indul 2000 helyett, illetve factorként kezeli numeric helyett
origo$theDate_honap <- as.numeric(origo$theDate_honap)
origo$theDate_nap <- as.numeric(origo$theDate_nap)
colnames(origo) <- c("Év", "Hónap", "Nap", "cim")

#sentiment_points függvényt be kell source-olni
origo <- sentiment_points(origo)

#évenkénti bontásban kellene vizsgálni a hangulatváltozást
#be kell source-olni az atlagokat
evi_origo <- evi_atlagok(origo)
havi_origo <- havi_atlagok(origo)
#látványosabb különbségért
evi_origo$Átlag <- evi_origo$Átlag*10
havi_origo$Átlag <- havi_origo$Átlag*10

#plottolás
simple_plot(havi_origo)
simple_plot(evi_origo)

#dygraphs
dyplot(evi_origo)
dyplot(havi_origo)

#%>% dyAxis("x", valueFormatter = "function(v){return (as.Date(v).format(%Y-%m-%d))}")

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
#INDEX
#adatok beimportálása, finomítása
index <- index_cikkek
index$cim <- as.character(index$cim)
index$ev <- as.numeric(substr(index$theDate, 1, 4))
index$honap <- as.numeric(substr(index$theDate, 6, 7))
index$nap <- as.numeric(substr(index$theDate, 9, 10))
index <- index[, 2:5]
colnames(index) <- c("cim", "Év", "Hónap", "Nap")

#szentimenetelemzés függvénnyel
index <- sentiment_points(index)

#évenkénti bontásban kellene vizsgálni a hangulatváltozást
#külön függvényben megírva a havi és éves átlag
evi_index <- evi_atlagok(index)
havi_index <- havi_atlagok(index)
#látványosabb különbségért
evi_index$Átlag <- evi_index$Átlag*10
havi_index$Átlag <- havi_index$Átlag*10

#plottolás
simple_plot(havi_index)
simple_plot(evi_index)

#dygraphs
dyplot(evi_index)
dyplot(havi_index)
