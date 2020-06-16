dane1 <- read.csv("C:/Users/Ja/Desktop/Studia/Metale/nasze/DANE_UZUP_TWARD.csv")

dane2 <- read.csv("C:/Users/Ja/Desktop/Studia/Metale/nasze/dane_uzupelnione_wartosci_modelu.csv")
dane1<-dane1[c(colnames(dane2),"Rm","Udarnosc.Charpy")]


dane1$Rm.as.cast<-dane2$Rm.as.cast
dane1$A5.as.cast<-dane2$A5.as.cast
dane<-dane1


##RM
dane_Rm_ = dane[which(!is.na(dane$Rm)),]
dane_Rm_<-dane_Rm_[,-14]
colnames(dane_Rm_)


#1 randomForest

model1_Rm <-  randomForest(Rm ~ ., data = dane_Rm_)


plot(dane_Rm_$Rm, predict(model1_Rm, dane_Rm_))
abline(0, 1, col = "red")


MSE<-function(y, y_pred){
  
  sum((y-y_pred)^2)/length(y)
}

R_kwadrat <- function(y, y_pred){
  y_sr <- mean(y)
  sum((y_pred-y_sr)^2) / sum((y-y_sr)^2)
}

##rf dla Rm
R_kwadrat(dane_Rm_$Rm, predict(model1_Rm, dane_Rm_))
#0.7975407


#wyliczenie r^2
a1 <- sum((dane_Rm_$Rm-predict(model1_Rm, dane_Rm_))^2)
a2 <- sum((dane_Rm_$Rm-mean(dane_Rm_$Rm))^2)

r2 <- 1 - a1/a2
r2
#0.9355264

MSE(dane_Rm_$Rm, predict(model1_Rm, dane_Rm_))
# NN
maxs <- apply(dane_Rm_, 2, max) 
mins <- apply(dane_Rm_, 2, min)

scaled <- as.data.frame(scale(dane_Rm_, center = mins, scale = maxs - mins))

train <- scaled
test <- scaled

library(neuralnet)
model2_Rm <- neuralnet(Rm ~.,data=train, hidden=c(7,3,2), linear.output=T)


pr.nn <- neuralnet::compute(model2_Rm,  test[,-13])
pr.nn <- pr.nn$net.result*(max(dane_Rm_$Rm)-min(dane_Rm_$Rm))+min(dane_Rm_$Rm)
test.r <- (test$Rm)*(max(dane_Rm_$Rm)-min(dane_Rm_$Rm))+min(dane_Rm_$Rm)

## wykres real vs predicted NN
plot(test.r,pr.nn,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1, col="black")
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

MSE(test.r, pr.nn)

#wyliczenie r^2
a1 <- sum((test.r-pr.nn)^2)
a2 <- sum((test.r-mean(test.r))^2)

r2 <- 1 - a1/a2
r2

dane1$Rm.as.cast<-dane2$Rm.as.cast
dane1$A5.as.cast<-dane2$A5.as.cast
dane<-dane1


##Udarnosc
dane_U_ = dane[which(!is.na(dane$Udarnosc.Charpy)),]
dane_U_<-dane_U_[,-13]
colnames(dane_U_)


#1 randomForest

model1_U <-  randomForest(Udarnosc.Charpy ~ .,
                           data = dane_U_)


plot(dane_U_$Udarnosc.Charpy, predict(model1_U, dane_U_))
abline(0, 1, col = "red")



R_kwadrat <- function(y, y_pred){
  y_sr <- mean(y)
  sum((y_pred-y_sr)^2) / sum((y-y_sr)^2)
}

##rf dla Rm
R_kwadrat(dane_U_$Udarnosc.Charpy, predict(model1_U, dane_U_))
#0.7975407


#wyliczenie r^2
a1 <- sum((dane_U_$Udarnosc.Charpy-predict(model1_U, dane_U_))^2)
a2 <- sum((dane_U_$Udarnosc.Charpy-mean(dane_U_$Udarnosc.Charpy))^2)

r2 <- 1 - a1/a2
r2
#0.9355264
MSE(dane_U_$Udarnosc.Charpy, predict(model1_U, dane_U_))

# NN
maxs <- apply(dane_U_, 2, max) 
mins <- apply(dane_U_, 2, min)

scaled <- as.data.frame(scale(dane_U_, center = mins, scale = maxs - mins))

train <- scaled
test <- scaled

library(neuralnet)
model2_U <- neuralnet(Udarnosc.Charpy ~.,data=train, hidden=c(4,9,4), linear.output=T)


pr.nn <- neuralnet::compute(model2_U,  test[,-13])
pr.nn <- pr.nn$net.result*(max(dane_U_$Udarnosc.Charpy)-min(dane_U_$Udarnosc.Charpy))+min(dane_U_$Udarnosc.Charpy)
test.r <- (test$Udarnosc.Charpy)*(max(dane_U_$Udarnosc.Charpy)-min(dane_U_$Udarnosc.Charpy))+min(dane_U_$Udarnosc.Charpy)

## wykres real vs predicted NN
plot(test.r,pr.nn,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1, col="black")
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

MSE(test.r, pr.nn)
#wyliczenie r^2
a1 <- sum((test.r-pr.nn)^2)
a2 <- sum((test.r-mean(test.r))^2)

r2 <- 1 - a1/a2
r2

