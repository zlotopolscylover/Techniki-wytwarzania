
###################################################################################################################
# bibilioteki

library(dplyr)
library(rstudioapi)


###################################################################################################################
# wczytanie danych

metal <- read.csv(file = 'dane-zeliwo-v1.csv', header = TRUE, sep = ',')
twardosc <- read.csv(file = 'C:/Users/Ja/Desktop/Studia/Metale/Techniki Wytwarzania Kocha�ski-20200118T163114Z-001/Techniki Wytwarzania Kocha�ski/TechnikiWytwarzania  - repozytorium/Przeliczanie twardosci/Twardosc.csv', header = TRUE, sep = ',')

###################################################################################################################
# funkcje dom przeliczania twardosci

# Vickersa -> Brinella
twardosc.temp <- twardosc %>% filter(Twardosc.Vickersa != 0 & Twardosc.Brinella != 0) %>% select(Twardosc.Vickersa, Twardosc.Brinella)
model <- lm(twardosc.temp$Twardosc.Brinella ~ twardosc.temp$Twardosc.Vickersa)
a1 <- model$coefficients[1]
b1 <- model$coefficients[2]

plot(twardosc.temp$Twardosc.Vickersa, twardosc.temp$Twardosc.Brinella)
abline(a1, b1)



df3 <- data.frame(Twardosc.Vickersa = twardosc.temp$Twardosc.Vickersa, Twardosc.Brinella = twardosc.temp$Twardosc.Brinella)
library(ggplot2)
ggplot(data = df3, aes(x=Twardosc.Vickersa, y = Twardosc.Brinella)) + geom_point() + geom_abline(intercept = a1, slope = b1) + ggtitle("Twardość Vickersa vs Twardość Brinella")

twardosc.vb <- function(v){
  as.numeric(b1*v + a1)
}

# Rockwella -> Brinella
twardosc.temp <- twardosc %>% filter(Twardosc.Rockwella != 0 & Twardosc.Brinella != 0) %>% select(Twardosc.Rockwella, Twardosc.Brinella)
model <- lm(twardosc.temp$Twardosc.Brinella ~ poly(twardosc.temp$Twardosc.Rockwella, 2, raw=TRUE))
a2 <- model$coefficients[1]
b2 <- model$coefficients[2]
c2 <- model$coefficients[3]

df4 <- data.frame(Twardosc.Brinella = twardosc.temp$Twardosc.Brinella, Twardosc.Rockwella = twardosc.temp$Twardosc.Rockwella)
ggplot(data = df4, aes(x=Twardosc.Rockwella, y = Twardosc.Brinella)) + geom_point()+stat_smooth(aes(x=Twardosc.Rockwella, y =Twardosc.Brinella),method = "lm", formula = y ~ poly(x, 2, raw =TRUE), size = 1, col="black") + ggtitle("Twardość Rockwella vs Twardość Brinella") 
plot(twardosc.temp$Twardosc.Rockwella, twardosc.temp$Twardosc.Brinella)
lines(twardosc.temp$Twardosc.Rockwella, predict(model, data.frame(twardosc.temp$Twardosc.Rockwella)), col="red")

twardosc.rb <- function(r){
  as.numeric(c2*r*r + b2*r + a2)
}

###################################################################################################################
# przeliczamu twardości do TWARDOŚCI BRINELLA:

temp1 <- metal %>% filter( !is.na(Twardosc.Brinella) ) # pozostaje bez zmian
temp2 <- metal %>% filter( !is.na(Twardosc.Vickersa) ) # nie dubluje się z inną twardością
temp3 <- metal %>% filter( !is.na(Twardosc.Rockwella) & is.na(Twardosc.Brinella) ) # dubluje się z twardością BRINELLA, wtedy bierzemy od razu BRINELLA
# temp4 <- metal %>% filter(!is.na(Twardosc.Rockwella.1)) # pomijamy, bo 12 obserwacji, a nie wiemy czym się różni od Rozkwella
# temp5 <- metal %>% filter(!is.na(Twardosc.Rockwella.2)) # pomijamy, bo brak obserwacji
temp6 <- metal %>% filter( is.na(Twardosc.Vickersa) & is.na(Twardosc.Brinella) & is.na(Twardosc.Rockwella) & is.na(Twardosc.Rockwella.1) & is.na(Twardosc.Rockwella.2) ) # obserwacje bez twardości - pomijamy

# liczba pbserwacji się zgadza:

dim(metal)[1]
dim(temp1)[1] + dim(temp2)[1] + dim(temp3)[1] + 12 + dim(temp6)[1]

# zmianna twardości (obliczenia):

temp1 <- temp1 %>% mutate(Twardosc = Twardosc.Brinella) %>% 
  select(-Twardosc.Brinella, -Twardosc.Vickersa, -Twardosc.Rockwella, -Twardosc.Rockwella.1, -Twardosc.Rockwella.2)
summary(df$Twardosc.Vickersa)
temp2 <- temp2 %>% filter(Twardosc.Vickersa >= 170) %>% mutate(Twardosc = twardosc.vb(Twardosc.Vickersa)) %>%
  select(-Twardosc.Brinella, -Twardosc.Vickersa, -Twardosc.Rockwella, -Twardosc.Rockwella.1, -Twardosc.Rockwella.2)

temp3 <- temp3 %>% mutate(Twardosc = twardosc.rb(Twardosc.Rockwella)) %>%
  select(-Twardosc.Brinella, -Twardosc.Vickersa, -Twardosc.Rockwella, -Twardosc.Rockwella.1, -Twardosc.Rockwella.2)

###################################################################################################################
# zwrot i zapisanie wyniku:

wynik <- rbind(temp1, temp2, temp3)
head(wynik)
write.csv(wynik, 'Twardo��_NASZE.csv', row.names = FALSE)

###################################################################################################################
