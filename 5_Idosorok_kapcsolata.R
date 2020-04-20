#ebben a fájlban a hangulatelemzés idősora és a havi inflációs adatok közti kapcsolatot vizsgálom

library(lmtest)
library(tseries)
library(forecast)
library(vars)

#0. adatok importálása
#szükséges: havi_index_gazdasag, havi_index, havi_origo, inflacio
#mivel az inflációs adatok csak 2004. januártól állnak rendelkezésre, így szűkíteni kell
havi_index_gazd <- havi_index_gazd[49:237,]
rownames(havi_index_gazd) <- c(1:189)
havi_index <- havi_index[49:237,]
rownames(havi_index) <- c(1:189)
havi_origo <- havi_origo[49:237,]
rownames(havi_origo) <- c(1:189)


#1.A Granger-teszt feltétele, hogy mindkét idősor stacioner legyen

adf.test(x = havi_index_gazd$Átlag) #stacioner
adf.test(x = havi_index$Átlag) #stacioner, bár közel 5% a p-érték
adf.test(x = havi_origo$Átlag) #egységgyök
adf.test(x = inflacio$`Fogyasztóiár-index`) #egységgyök

plot(inflacio$`Fogyasztóiár-index`, type="l", col="black")

ndiffs(havi_origo$Átlag) #egyszer kell differenciázni
ndiffs(inflacio$`Fogyasztóiár-index`) #egyszer kell differenciázni
havi_origo$stacioner <- c(diff(havi_origo$Átlag, differences = 1), 0)
inflacio$stacioner <- c(diff(inflacio$`Fogyasztóiár-index`, differences = 1), 0)
adf.test(x = havi_origo$stacioner) #már stacioner
adf.test(x = inflacio$stacioner) #már stacioner



#2. Granger-okság

#be kell source-olni a granger_test függvényt
source(file = "https://raw.githubusercontent.com/heilmanni/UNKP/master/5_granger_test.R")

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


#VAR MODELLEK

#megfelelő mátrixok létrehozása
inf_gazd_ <- matrix(ncol=2, c(inflacio$stacioner, havi_index_gazd$valtozas)) #infláció és gazdasági hírek
colnames(inf_gazd_) <- c("inflacio", "havi_gazd")

inf_index <- matrix(ncol = 2, c(inflacio$stacioner, havi_index$valtozas)) #infláció és index hírei
colnames(inf_index) <- c("inflacio", "index")

#VAR modellezés
#var_inf_gazd <- VAR(y = inf_gazd_, type = "const", lag.max = 12, ic = "AIC") #ha a késleltetések automatikusan szelektáltak (javasolt: 5)
var_inf_gazd <- vars::VAR(y = inf_gazd_, type = "const", p = 5) #5 késleltetés esetén
summary(var_inf_gazd)

var_inf_gazd_8 <- vars::VAR(y = inf_gazd_, type = "const", p = 8) #8 késleltetés esetén
summary(var_inf_gazd_8)

#var_inf_index <- VAR(y = inf_index, type = "const", lag.max = 12, ic = "AIC") #ha a késleltetések automatikusan szelektáltak (javasolt: 5)
var_inf_index <- vars::VAR(y = inf_index, type = "const", p = 5) #5 késleltetés esetén
summary(var_inf_index)


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


plot(inflacio$stacioner, type="l", col="black")
lines(havi_index_gazd$valtozas, col="blue")
plot(havi_index_gazd$valtozas, col="blue", type = 'l')
