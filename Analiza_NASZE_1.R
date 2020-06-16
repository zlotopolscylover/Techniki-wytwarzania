df <- read.csv("dane-zeliwo-v1.csv")
## uzgodnione typy, uzgodnione wartości dla wielkości sferoidów

## Czyszczenie danych

summary(df)
dim(df) #1508   42

## usuwamy wiersze, gdzie wszystkie obserwacje sa NA
nNA <- is.na(df)
na.wiersze<-numeric(nrow(nNA))
for(i in 1:nrow(nNA)){
  na.wiersze[i]<-sum(nNA[i,])
}

summary(na.wiersze)

df <- df[which(na.wiersze<42), ]

# usuwamy kolumny, gdzie wszystkie obserwacji NA
na.kolumny <- logical()
for (i in 1:ncol(df)){
  na.kolumny[i] <- sum(is.na(df[,i]))
}

summary(na.kolumny)

df <- df[, which(na.kolumny<1504)]

dim(df)#1504   41


## Uzupełnianie danych

#węgiel - srednia
df$C <- ifelse(is.na(df$C), mean(df$C[!is.na(df$C)]), df$C)

#krzem - srednia
df$Si <- ifelse(is.na(df$Si), mean(df$Si[!is.na(df$Si)]), df$Si)

#siarka - uzupełniamy jako 0.01
df$S <- ifelse(is.na(df$S), 0.01, df$S)
hist(df$P)

#forsor - uzupełniamy jako 0.02
df$P <- ifelse(is.na(df$P), 0.02, df$P)

#Mg - tutaj coś jest nie tak, wartosci z przedzialu 0.15 - 0.35-40
#zamieniam wartości 0.75 na 0.075, 3 obs z 0.1 zostawiam
df$Mg <- ifelse(df$Mg > 0.07, 0.07, df$Mg)
df$Mg <- ifelse(df$Mg < 0.03, 0.03, df$Mg)

df$Mg <- ifelse(is.na(df$Mg), mean(df$Mg[-which(is.na(df$Mg))]), df$Mg) #Mg na NA wartość minimalna

#Mn - dodaje wartosc minimalna
df$Mn <- ifelse(is.na(df$Mn), mean(na.omit(df$Mn)), df$Mn)

#Ni na NA wartosc 0.001
df$Ni <- ifelse(is.na(df$Ni), 0.001, df$Ni)

#Cu na Na wartosc 0.001
df$Cu <- ifelse(is.na(df$Cu), 0.001, df$Cu)

#Mo na Na wartosc 0.001
df$Mo <- ifelse(is.na(df$Mo), 0.001, df$Mo)

#Cr
df$Cr <- ifelse(is.na(df$Cr), 0.001, df$Cr)

#Al
df$Al <- ifelse(is.na(df$Al), 0.001, df$Al)

#Sn
df$Sn <- ifelse(is.na(df$Sn), 0.001, df$Sn)

#B
df$B <- ifelse(is.na(df$B), 0.001, df$B)

#V
df$V <- ifelse(is.na(df$V), 0.001, df$V)





# nodularity
df$Nodularity <- ifelse(is.na(df$Nodularity), 90, df$Nodularity)

#minimalna grubosc scianki - usuwamy zmienną
df <- df[, -which(names(df) == "Minimalna.grubosc.scianki")]

#czas austenityzacji
hist(df$Czas.austenityzacji)

#temperatura austen
df$Temperatura.austenityzacji <- ifelse(is.na(df$Temperatura.austenityzacji), 900, df$Temperatura.austenityzacji)

#temp udarnosci  !!! dopisać bo nie wiem
hist(df$Temperatura.pomiaru.udarnosci)
#df$Temperatura_pomiaru_udarnosci <- ifelse(is.na(df$Temperatura_pomiaru_udarnosci), 22, df$Temperatura_pomiaru_udarnosci)

#udzail austenitu
df$Udzial.austenitu <- ifelse(is.na(df$Udzial.austenitu), 30, df$Udzial.austenitu)
summary(df)

write.csv(df, "C:/Users/Ja/Desktop/Studia/Metale/nasze/DANE_UZUP.csv", row.names = FALSE)
?write

















