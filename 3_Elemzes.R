library(tm)
library(dplyr)
library(tidytext)
library(textdata)
library(stringr)
library(tidyverse)
library(dygraphs)
library(tseries)

#Szentimentszótár létrehozása - már csak be kell importálni!
#Adatfájl: sentiment_dictionary
Negative$point <- -1 #negatívhoz -1 rendelve
Positive$point <- 1 #pozitivhoz +1 rendelve
sentimentdic <- rbind(Negative, Positive) #egybekötve
colnames(sentimentdic) <- c("word", "point") #oszlopok elnevezve


#egy extra sor hozzáadása, később még hasznos lesz
sentimentdic[7689,] <- c("", "hiba")

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#adatok beimportálása, tisztítása
#adatfájl: Origo_cimek
origo <- cikkcimek
origo$cim <- as.character(origo$cim)
origo$theDate_ev <- as.numeric(origo$theDate_ev)+ 1999 #sajnos valamiért 1-nél indul 2000 helyett, illetve factorként kezeli numeric helyett
origo$theDate_honap <- as.numeric(origo$theDate_honap)
origo$theDate_nap <- as.numeric(origo$theDate_nap)
colnames(origo) <- c("Év", "Hónap", "Nap", "cim") #oszlopok elnevezése

#sentiment_points függvényt be kell source-olni
source(file = "https://raw.githubusercontent.com/heilmanni/UNKP/master/3_sentiment_points.R")
origo <- sentiment_points(origo)

#évi és havi bontásban kellene vizsgálni a hangulatváltozást
#be kell source-olni az atlagokat
source(file = "https://raw.githubusercontent.com/heilmanni/UNKP/master/3_Atlagok.R")
#ha rosszul kezeli a source-olást, akkor az a magyar karakterek miatt van benne, érdemes letölteni és úgy source-olni
evi_origo <- evi_atlagok(origo)
havi_origo <- havi_atlagok(origo)
#látványosabb különbségért

#plottolás
#be kell source-olni a simple_plot és dyplot függvényt
source(file = "https://raw.githubusercontent.com/heilmanni/UNKP/master/3_plots.R")
#ha rosszul kezeli a source-olást, akkor az a magyar karakterek miatt van benne, érdemes letölteni és úgy source-olni
simple_plot(havi_origo)
simple_plot(evi_origo)

#dygraphs
dyplot(evi_origo)
dyplot(havi_origo)

adf.test(havi_origo$Átlag)

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
#INDEX
#adatok beimportálása, finomítása
#adatfájl: Index_cikkek
index <- index_cikkek
index$cim <- as.character(index$cim)
#a dátumot szétválasztom három oszlopra, utána törlöm a dátum oszlopot
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

#plottolás
simple_plot(havi_index)
simple_plot(evi_index)

#dygraphs
dyplot(evi_index)
dyplot(havi_index)

adf.test(havi_index$Átlag)

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
#INDEX GAZDASÁG
#adatfájl: Index_gazdasag
index_gazd <- index_gazdasag_2
index_gazd$cim <- as.character(index_gazd$cim)
#a dátumot szétválasztom három oszlopra, utána törlöm a dátum oszlopot
index_gazd$ev <- as.numeric(substr(index_gazd$theDate, 1, 4))
index_gazd$honap <- as.numeric(substr(index_gazd$theDate, 6, 7))
index_gazd$nap <- as.numeric(substr(index_gazd$theDate, 9, 10))
index_gazd <- index_gazd[, 2:5]
colnames(index_gazd) <- c("cim", "Év", "Hónap", "Nap")

#szentimenetelemzés függvénnyel
index_gazd <- sentiment_points(index_gazd)

#évenkénti bontásban kellene vizsgálni a hangulatváltozást
#külön függvényben megírva a havi és éves átlag
evi_index_gazd <- evi_atlagok(index_gazd)
havi_index_gazd <- havi_atlagok(index_gazd)

#plottolás
simple_plot(havi_index_gazd)
simple_plot(evi_index_gazd)

#dygraphs
dyplot(evi_index_gazd)
dyplot(havi_index_gazd)

#néhány adat
summary(havi_index$Átlag)
havi_index[which.min(havi_index$Átlag),]
havi_index[which.max(havi_index$Átlag),]
sd(havi_index$Átlag)
sd(havi_index$Átlag)/mean(havi_index$Átlag)

#hány napra van adata az idősoroknak
origo$datum <- paste(origo$Év, origo$Hónap, origo$Nap, sep = "-")
length(unique(havi_index$Dátum))
length(unique(havi_index_gazd$Dátum))
length(unique(havi_origo$Dátum))
