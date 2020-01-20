#ebben a f�jlban a hangulatelemz�s id�sora �s a havi infl�ci�s adatok k�zti kapcsolatot vizsg�lom

library(lmtest)
library(tseries)
library(forecast)

#0. adatok import�l�sa
#sz�ks�ges: havi_index_gazdas�g, havi_index, havi_origo, inflacio
#mivel az infl�ci�s adatok csak 2004. janu�rt�l �llnak rendelkez�sre, �gy sz�k�teni kell
havi_index_gazd <- havi_index_gazd[49:237,]
rownames(havi_index_gazd) <- c(1:189)
havi_index <- havi_index[49:237,]
rownames(havi_index) <- c(1:189)
havi_origo <- havi_origo[49:237,]
rownames(havi_origo) <- c(1:189)


#1.A Granger-teszt felt�tele, hogy mindk�t id�sor stacioner legyen

adf.test(x = havi_index_gazd$�tlag) #stacioner
adf.test(x = havi_index$�tlag) #stacioner, b�r k�zel 5% a p-�rt�k
adf.test(x = havi_origo$�tlag) #egys�ggy�k
adf.test(x = inflacio$`Fogyaszt�i�r-index`) #egys�ggy�k

xaxis = c(1:189) #tengely
plot(xaxis, inflacio$`Fogyaszt�i�r-index`, type="l", col="black")

ndiffs(havi_origo$�tlag) #egyszer kell differenci�zni
ndiffs(inflacio$`Fogyaszt�i�r-index`) #egyszer kell differenci�zni
havi_origo$stacioner <- c(diff(havi_origo$�tlag, differences = 1), 0)
inflacio$stacioner <- c(diff(inflacio$`Fogyaszt�i�r-index`, differences = 1), 0)
adf.test(x = havi_origo$stacioner) #m�r stacioner
adf.test(x = inflacio$stacioner) #m�r stacioner



#2. Granger-oks�g

granger_test(y = inflacio$stacioner, x = havi_index_gazd$�tlag, max_lag = 12)
granger_test(y = inflacio$stacioner, x = havi_index$�tlag, max_lag = 12)
granger_test(y = inflacio$stacioner, x = havi_origo$stacioner, max_lag = 12)

#v�ltoz�ssal n�zni
havi_index_gazd$valtozas <- c(0)
for (i in 1:188)
{
  havi_index_gazd$valtozas[i] <- (havi_index_gazd$�tlag[i+1] - 
                                    havi_index_gazd$�tlag[i])
}

adf.test(havi_index_gazd$valtozas)
granger_test(y = inflacio$stacioner, x = havi_index_gazd$valtozas, max_lag = 12)


havi_index$valtozas <- c(0)
for (i in 1:188)
{
  havi_index$valtozas[i] <- (havi_index$�tlag[i+1] - 
                                    havi_index$�tlag[i])
}

adf.test(havi_index$valtozas)
granger_test(y = inflacio$stacioner, x = havi_index$valtozas, max_lag = 12)

#az infl�ci� okozza-e a rossz hangulat� h�reket
granger_test(x = inflacio$stacioner, y = havi_index_gazd$�tlag, max_lag = 12)
granger_test(x = inflacio$stacioner, y = havi_index$�tlag, max_lag = 12)
granger_test(x = inflacio$stacioner, y = havi_origo$stacioner, max_lag = 12)
granger_test(x = inflacio$stacioner, y = havi_index_gazd$valtozas, max_lag = 12)
granger_test(x = inflacio$stacioner, y = havi_index$valtozas, max_lag = 12)


#KOINTEGR�CI� TESZTEL�SE

#az infl�ci� �s origo line�ris kapcsolat�nak becsl�se
summary(glm(inflacio$`Fogyaszt�i�r-index` ~ havi_origo$�tlag))
#a koefficiens 327.980

z <- inflacio$`Fogyaszt�i�r-index` - 327.980*havi_origo$�tlag
adf.test(z)


summary(lm(inflacio$`Fogyaszt�i�r-index` ~ havi_index$�tlag))
#162.058
z2 <- inflacio$`Fogyaszt�i�r-index` - 162.058*havi_index$�tlag
adf.test(z2)


xaxis = c(1:189) #tengely
plot(xaxis, inflacio$stacioner, type="l", col="black")
lines(xaxis, havi_index_gazd$valtozas, col="blue")
plot(xaxis, havi_index_gazd$valtozas, col="blue", type = 'l')