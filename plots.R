#ezek a függvények sima plotot, illetve dygraphot készítenek
#ehhez olyan adatfájl kell megadni, ami tartalmaz "Dátum" és "Átlag" oszlopokat

simple_plot <- function(adat)
{
  plot(x = adat$`Dátum`, y = adat$Átlag, type = "l",
       xlab = "Dátum", ylab = "Átlag", 
       main = "A hírportál adatainak átlagos hangulatvilága")
}

dyplot <- function(adat){
  adat$Dátum <- as.numeric(c(adat$Dátum))
  dygraph(data = adat, main = "A hírportál adatainak átlagos hangulatvilága") %>% 
    dyRangeSelector(fillColor = "grey") %>% 
    dyAxis("x", label = "Dátum", axisLabelFontSize = 20,labelWidth = 30, axisLabelWidth = 60) %>% 
    dyAxis("y", label = "Havi átlag tízszerese", axisLabelFontSize = 20,labelWidth = 20, axisLabelWidth = 50)
}


