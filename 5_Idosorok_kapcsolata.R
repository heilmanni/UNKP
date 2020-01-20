#ebben a fájlban a hangulatelemzés idõsora és a havi inflációs adatok közti kapcsolatot vizsgálom

library(lmtest)
library(tseries)
library(forecast)

#0. adatok importálása
#szükséges: havi_index_gazdaság, havi_index, havi_origo, inflacio
#mivel az inflációs adatok csak 2004. januártól állnak rendelkezésre, így szûkíteni kell
havi_index_gazd <- havi_index_gazd[49:237,]
rownames(havi_index_gazd) <- c(1:189)
havi_index <- havi_index[49:237,]
rownames(havi_index) <- c(1:189)
havi_origo <- havi_origo[49:237,]
rownames(havi_origo) <- c(1:189)


#1.A Granger-teszt feltétele, hogy mindkét idõsor stacioner legyen

adf.test(x = havi_index_gazd$Átlag) #stacioner
adf.test(x = havi_index$Átlag) #stacioner, bár közel 5% a p-érték
adf.test(x = havi_origo$Átlag) #egységgyök
adf.test(x = inflacio$`Fogyasztóiár-index`) #egységgyök

xaxis = c(1:189) #tengely
plot(xaxis, inflacio$`Fogyasztóiár-index`, type="l", col="black")

ndiffs(havi_origo$Átlag) #egyszer kell differenciázni
ndiffs(inflacio$`Fogyasztóiár-index`) #egyszer kell differenciázni
havi_origo$stacioner <- c(diff(havi_origo$Átlag, differences = 1), 0)
inflacio$stacioner <- c(diff(inflacio$`Fogyasztóiár-index`, differences = 1), 0)
adf.test(x = havi_origo$stacioner) #már stacioner
adf.test(x = inflacio$stacioner) #már stacioner



#2. Granger-okság

granger_test(y = inflacio$stacioner, x = havi_index_gazd$Átlag, max_lag = 12)
granger_test(y = inflacio$stacioner, x = havi_index$Átlag, max_lag = 12)
granger_test(y = inflacio$stacioner, x = havi_origo$stacioner, max_lag = 12)

#változással nézni
havi_index_gazd$valtozas <- c(0)
for (i in 1:188)
{
  havi_index_gazd$valtozas[i] <- (havi_index_gazd$Átlag[i+1] - 
                                    havi_index_gazd$Átlag[i])
}

adf.test(havi_index_gazd$valtozas)
granger_test(y = inflacio$stacioner, x = havi_index_gazd$valtozas, max_lag = 12)


havi_index$valtozas <- c(0)
for (i in 1:188)
{
  havi_index$valtozas[i] <- (havi_index$Átlag[i+1] - 
                                    havi_index$Átlag[i])
}

adf.test(havi_index$valtozas)
granger_test(y = inflacio$stacioner, x = havi_index$valtozas, max_lag = 12)

#az infláció okozza-e a rossz hangulatú híreket
granger_test(x = inflacio$stacioner, y = havi_index_gazd$Átlag, max_lag = 12)
granger_test(x = inflacio$stacioner, y = havi_index$Átlag, max_lag = 12)
granger_test(x = inflacio$stacioner, y = havi_origo$stacioner, max_lag = 12)
granger_test(x = inflacio$stacioner, y = havi_index_gazd$valtozas, max_lag = 12)
granger_test(x = inflacio$stacioner, y = havi_index$valtozas, max_lag = 12)


#KOINTEGRÁCIÓ TESZTELÉSE

#az infláció és origo lineáris kapcsolatának becslése
summary(glm(inflacio$`Fogyasztóiár-index` ~ havi_origo$Átlag))
#a koefficiens 327.980

z <- inflacio$`Fogyasztóiár-index` - 327.980*havi_origo$Átlag
adf.test(z)


summary(lm(inflacio$`Fogyasztóiár-index` ~ havi_index$Átlag))
#162.058
z2 <- inflacio$`Fogyasztóiár-index` - 162.058*havi_index$Átlag
adf.test(z2)


xaxis = c(1:189) #tengely
plot(xaxis, inflacio$stacioner, type="l", col="black")
lines(xaxis, havi_index_gazd$valtozas, col="blue")
plot(xaxis, havi_index_gazd$valtozas, col="blue", type = 'l')
