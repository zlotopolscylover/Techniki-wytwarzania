dane <- read.csv("C:/Users/Ja/Desktop/Studia/Metale/nasze/DANE_UZUP_TWARD.csv")
dane_wzorcowe <- read.csv("C:/Users/Ja/Desktop/Studia/Metale/nasze/dane_model_rm_a5.csv")
names(dane_wzorcowe) <- c("C", "Mn", "Si", "P", "S", "Cr", "Ni", "Cu", "Mg", "Rm.as.cast", "A5.as.cast", "Twardosc")
dane1 <- dane[, c("C", "Mn", "Si", "P", "S", "Cr", "Ni", "Cu", "Mg", "Rm.as.cast", "A5.as.cast", "Twardosc")]
  colnames(dane_wzorcowe)

### dane dla ktorych jest wytrzymalosc - Rm  oraz A5 as cast###
dane1[which(!is.na(dane1$Rm.as.cast) & !is.na(dane1$A5.as.cast)),] -> dane2
#dane1[which(!is.na(dane1$A5.as.cast)),] -> dane1_a5

dane_do_modelowania <- rbind(dane_wzorcowe, dane2)
colnames(dane_wzorcowe)
write.csv(dane_do_modelowania, "C:/Users/Ja/Desktop/Studia/Metale/nasze/polaczone_dane_do_modelowania.csv", row.names = FALSE)
