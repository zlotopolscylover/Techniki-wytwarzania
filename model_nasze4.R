
setwd("C:/Users/Ja/Desktop/Studia/Metale/nasze")

dane <- read.csv("polaczone_dane_do_modelowania.csv")
head(dane)
### model do wyliczenia wytrzymalosci w stanie lanym - Rm.as.cast

# usuwamy kolumne z A5.as.cast 

dane1 <- dane[,-which(names(dane) == "A5.as.cast")]

#model liniowy
model1 <- lm(Rm.as.cast ~ ., data = dane)
summary(model1)
dane$Rm.as.cast - predict(model1, dane) -> reszty1
boxplot(reszty1)
plot(reszty1)
plot(dane$Rm.as.cast, predict(model1, dane))
abline(0, 1, col = "red")
####
# R-squared na poziomie 0.82
# Rozkład reszt oscyluje wokół zera - wydaje się dobrze dopasowany
####


#model randomForest
library(randomForest)

model2 <-  randomForest(Rm.as.cast ~ ., data = dane1)
summary(model2)
dane1$Rm.as.cast - predict(model2, dane1) -> reszty2
boxplot(reszty2)
plot(reszty2)

#rzeczywiste vs prognoza
plot(dane1$Rm.as.cast, predict(model2, dane1))
abline(0,1, col = "red")

####
# Rozkład reszt oscyluje wokół zera - lepiej dopasowany niż model linowy, mniejszy rozrzut
####

### => ostatecznie wybór modelu randomForest jako uzupełnienie wytrzymalości




### NN - Ania

index <- sample(1:nrow(dane1),round(0.75*nrow(dane1)))

maxs <- apply(dane1, 2, max) 
mins <- apply(dane1, 2, min)

scaled <- as.data.frame(scale(dane1, center = mins, scale = maxs - mins))
#train_ <- scaled
#test_ <- scaled
train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
nn <- neuralnet(Rm.as.cast ~ .,data=train_,hidden=c(5,3),linear.output=T)
plot(nn)

pr.nn <- compute(nn,test_[,c(1:9,11)])

pr.nn_ <- pr.nn$net.result*(max(dane1$Rm.as.cast)-min(dane1$Rm.as.cast))+min(dane1$Rm.as.cast)
test.r <- (test_$Rm.as.cast)*(max(dane1$Rm.as.cast)-min(dane1$Rm.as.cast))+min(dane1$Rm.as.cast)

## wykres real vs predicted NN
plot(test.r,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1, col="black")
legend('bottomright',legend='NN',pch=18,col='red', bty='n')







### model do wyliczenia wydluzenia w stanie lanym - A5.as.cast

# usuwamy kolumne z Rm.as.cast 

dane3 <- dane[,-which(names(dane) == "Rm.as.cast")]

#model liniowy
model3 <- lm(A5.as.cast ~ ., data = dane3)
summary(model3)
dane3$A5.as.cast - predict(model3, dane3) -> reszty3
boxplot(reszty3)
plot(reszty3)
plot(dane3$A5.as.cast, predict(model3, dane3))
abline(0, 1, col = "red")
####
# R-squared na poziomie 0.77
# Rozkład reszt oscyluje wokół zera - wydaje się dobrze dopasowany
####


#model randomForest
library(randomForest)

model4 <-  randomForest(A5.as.cast ~ ., data = dane3)
summary(model4)
dane3$A5.as.cast - predict(model4, dane3) -> reszty4
boxplot(reszty4)
plot(reszty4)

#rzeczywiste vs prognoza
plot(dane$A5.as.cast, predict(model4, dane3))
abline(0,1, col = "red")

####
# Rozkład reszt oscyluje wokół zera - lepiej dopasowany niż model linowy, mniejszy rozrzut
####

### => ostatecznie wybór modelu randomForest jako uzupełnienie wytrzymalości


### NN - Dawid

index <- sample(1:nrow(dane3),round(0.75*nrow(dane3)))

maxs <- apply(dane3, 2, max) 
mins <- apply(dane3, 2, min)

scaled <- as.data.frame(scale(dane3, center = mins, scale = maxs - mins))
#train_ <- scaled
#test_ <- scaled
train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
nn2 <- neuralnet(A5.as.cast ~ .,data=train_, hidden=c(5), linear.output=T)
plot(nn2)

pr.nn2 <- compute(nn2, test_[,-10])

pr.nn2_ <- pr.nn2$net.result*(max(dane3$A5.as.cast) - min(dane3$A5.as.cast)) + min(dane3$A5.as.cast)
test.r <- (test_$A5.as.cast)*(max(dane3$A5.as.cast) - min(dane3$A5.as.cast)) + min(dane3$A5.as.cast)

## wykres real vs predicted NN
plot(test.r,pr.nn2_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1, col="black")
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

# model rm model2 a do a5 model4


df_temp <- read.csv("C:\\Users\\Ja\\Desktop\\Studia\\Metale\\nasze\\DANE_UZUP_TWARD.csv")

df_temp1 <- df_temp[, c("C", "Mn", "Si", "P", "S", "Cr", "Ni", "Cu", "Mg", "Rm.as.cast", "A5.as.cast", "Twardosc")]

### dane, gdzie nie ma wartosci Rm.as.cast
index_na_rm<-which(is.na(df_temp1$Rm.as.cast))
dane_b1 <- df_temp1[which(is.na(df_temp1$Rm.as.cast)),]
dane_b1 <- dane_b1[,-which(names(dane_b1)=="Rm.as.cast")]
dane_b1 <- dane_b1[,-which(names(dane_b1)=="A5.as.cast")]



dane_b1$Rm.as.cast_1 <- predict(model2, dane_b1)

df_temp1[index_na_rm,"Rm.as.cast"]<-predict(model2, dane_b1)

### dane, gdzie nie ma wartości A5.as.cast
index_na_a5<-which(is.na(df_temp1$A5.as.cast))
dane_b2 <- df_temp1[which(is.na(df_temp1$A5.as.cast)),]
dane_b2 <- dane_b2[,-which(names(dane_b2)=="A5.as.cast")]
dane_b2 <- dane_b2[,-which(names(dane_b2)=="Rm.as.cast")]

dane_b2$A5.as.cast_1 <- predict(model4, dane_b2)
df_temp1[index_na_a5,"A5.as.cast"]<-predict(model4, dane_b2)
#### df_temp - dolaczenie danych z modelu dla Rm.as.cast oraz A5.as.cast


write.csv(df_temp1, "C:/Users/Ja/Desktop/Studia/Metale/nasze/dane_uzupelnione_wartosci_modelu.csv", row.names = FALSE)
