#ez a függvény elvégzi a hangulatelemzést
#ehhez egy adatfájlt kell megadni, ami tartalmaz "cim" oszlopot, és az szavakat tartalmaz

sentiment_points <- function(adat){
  adat$id <- row.names(adat) #azonosítóval látja el a sorokat
  
  words <- unnest_tokens(tbl = adat, output = word, input = cim) #a cimeket szavakra bontja
  words$word %>% removeWords(stopwords("hungarian")) %>% removeNumbers()  -> words$word #kiveszi a (magyar nyelvű) töltelékszavakat és számokat - ezek helyére majd "hiba" kerül, nem NA
  words2 <- left_join(words, sentimentdic, by="word") #szavak egyezősége alapján összeköti a szentimentszótárral (üres sorok "hiba" feliratot kapnak)
  words2$point <- ifelse(is.na(words2$point), 0, words2$point) #NA-t 0-ra cseréli
  words2 <- subset(words2, !words2$point == "hiba") #a "hiba" sorok kivétele
  words2$point <- as.numeric(words2$point) #numerikus értelmezés
  
  final <- words2 %>% group_by(id) %>% summarize(score=mean(point)) #id szerint csoportosítás, átlagpontszámítás
  adat <- left_join(adat, final, by="id") #visszaírás az eredeti adattáblába
  
  return(adat)
}
